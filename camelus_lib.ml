(****************************************************************************)
(*                                                                          *)
(*    Copyright (c) 2015 OCamlPro                                           *)
(*                                                                          *)
(* Permission to use, copy, modify, and distribute this software for any    *)
(* purpose with or without fee is hereby granted, provided that the above   *)
(* copyright notice and this permission notice appear in all copies.        *)
(*                                                                          *)
(* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES *)
(* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         *)
(* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  *)
(* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   *)
(* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    *)
(* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  *)
(* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           *)
(*                                                                          *)
(****************************************************************************)

open Lwt.Infix

let log_tag : string Lwt.key= Lwt.new_key ()

let log fmt =
  let tag =
    match Lwt.get log_tag with
    | None -> "??"
    | Some s -> s
  in
  OpamConsole.msg ("[%s] "^^ fmt ^^ "\n%!") tag

let verbose =
  try Sys.getenv "CAMELUS_VERBOSE" <> ""
  with Not_found -> false

type repo = {
  user: string;
  name: string;
  auth: (string * string) option; (* user, token *)
}

type full_ref = {
  repo: repo;
  ref: string;
  sha: string;
}

type pull_request = {
  number: int;
  base: full_ref;
  head: full_ref;
  pr_user: string;
  message: string * string;
}

type push_event = {
  push_repo: repo;
  push_head: string;
  push_ancestor: string;
}

module Conf = struct
  module C = struct
    let internal = ".opam-ci"

    type t = {
      port: int;
      name: string;
      token: Github.Token.t;
      secret: Cstruct.t;
      repo: repo;
      roles: [ `Pr_checker | `Push_upgrader ] list;
      base_branch: string;
      dest_branch: string;
      camelus_child_loc : string;
    }

    let empty = {
      port = 8122;
      name = "opam-ci";
      token = Github.Token.of_string "";
      secret = Cstruct.of_string "";
      repo = { user="ocaml"; name="opam-repository"; auth=None };
      roles = [ `Pr_checker ];
      base_branch = "master";
      dest_branch = "2.0.0";
      camelus_child_loc = "./camelus_child.native";
    }

    open OpamPp.Op

    let role_of_string = function
      | "pr_checker" -> `Pr_checker
      | "push_upgrader" -> `Push_upgrader
      | _ -> failwith "Invalid role (accepted are pr_checker, push_upgrader)"

    let role_to_string = function
      | `Pr_checker -> "pr_checker"
      | `Push_upgrader -> "push_upgrader"

    let fields = [
      "port", OpamPp.ppacc (fun port t -> {t with port}) (fun t -> t.port)
        OpamFormat.V.pos_int;
      "name", OpamPp.ppacc (fun name t -> {t with name}) (fun t -> t.name)
        OpamFormat.V.string;
      "token", OpamPp.ppacc (fun token t -> {t with token}) (fun t -> t.token)
        (OpamFormat.V.string -|
         OpamPp.of_module "token" (module Github.Token));
      "secret", OpamPp.ppacc
        (fun secret t -> {t with secret}) (fun t -> t.secret)
        (OpamFormat.V.string -|
         OpamPp.of_pair "secret"
           Cstruct.((of_string ?allocator:None ?off:None ?len:None), to_string));
      "repo-user", OpamPp.ppacc
        (fun user t -> {t with repo = {t.repo with user}})
        (fun t -> t.repo.user)
        OpamFormat.V.string;
      "repo-name", OpamPp.ppacc
        (fun name t -> {t with repo = {t.repo with name}})
        (fun t -> t.repo.name)
        OpamFormat.V.string;
      "roles", OpamPp.ppacc
        (fun roles t -> {t with roles })
        (fun t -> t.roles)
        (OpamFormat.V.map_list ~depth:1 @@
         OpamFormat.V.ident -|
         OpamPp.of_pair "role" (role_of_string, role_to_string));
      "base-branch", OpamPp.ppacc
        (fun base_branch t -> {t with base_branch })
        (fun t -> t.base_branch)
        OpamFormat.V.string;
      "dest-branch", OpamPp.ppacc
        (fun dest_branch t -> {t with dest_branch })
        (fun t -> t.dest_branch)
        OpamFormat.V.string;
      "child-process", OpamPp.ppacc (fun camelus_child_loc t -> {t with camelus_child_loc}) (fun t -> t.camelus_child_loc)
        OpamFormat.V.string;

    ]

    let pp =
      OpamFormat.I.map_file @@
      OpamFormat.I.fields ~name:internal ~empty fields -|
      OpamFormat.I.show_errors ~name:internal ~strict:true ()
  end
  include C
  include OpamFile.SyntaxFile(C)
end

module Semaphore = struct

  type t =
    {
      max_count : int;
      mutable curr_count : int;
      signal : unit Lwt_condition.t;
    }

  let make max_count =
    { max_count; curr_count = 0;
      signal = Lwt_condition.create (); }

  let obtain x =
    let go () =
      x.curr_count <- succ x.curr_count;
      Lwt.return_unit
    in
    if x.curr_count < x.max_count
    then go ()
    else ( Lwt_condition.wait x.signal >>= fun () -> go () )

  let release x =
    x.curr_count <- pred x.curr_count;
    Lwt_condition.signal x.signal ()

end

module FdPool = struct

  let max_count = 50

  let c = Semaphore.make max_count

  let fd_use () = Semaphore.obtain c

  let fd_free () = Semaphore.release c

  let with_fd (f : unit -> 'a Lwt.t) : 'a Lwt.t =
    begin fd_use () >>= f end
      [%lwt.finally fd_free (); Lwt.return_unit]

end

module GH = struct

  type _ req =
    | Count_previous_posts : pull_request -> int req
    | Comment : pull_request * string -> string req
    | Status : (pull_request * Github_t.status_state * string option) -> unit req
    | Pr : int -> Github_t.pull req
    | Needing_check : int list req

  type breq = R : 'a req * 'a Lwt.u -> breq

  let count_previous_posts conf { pr_user = user; number; _ } =
    let open Github.Monad in
    let open Github_t in
    let s = Github.Search.issues
        ~qualifiers:[`Author user;
                     `Repo (conf.Conf.repo.user ^"/"^conf.Conf.repo.name);]
        ~keywords:[] ()
    in
    bind (function
        | None -> return 0
        | Some ({repository_issue_search_total_count = c}, _) -> return c
      )
      (Github.Stream.next s)

  let comment conf pr body =
    let open Github.Monad in
    let open Github_t in
    let token = conf.Conf.token in
    let user = pr.base.repo.user in
    let repo = pr.base.repo.name in
    let num = pr.number in
    let rec find_comment stream =
      Github.Stream.next stream >>= function
      | Some (c, s) ->
        if c.issue_comment_user.user_login = conf.name then return (Some c)
        else find_comment s
      | None -> return None
    in
    begin
      find_comment (Github.Issue.comments ~token ~user ~repo ~num ())
      >>= function
      | None ->
        Github.Issue.create_comment ~token ~user ~repo ~num ~body ()
      | Some { issue_comment_id = id; _ } ->
        Github.Issue.update_comment ~token ~user ~repo ~id ~body ()
    end >>~ (fun { issue_comment_html_url = url; _ } -> return url)

  let status conf pr status text =
      let open Github.Monad in
      let open Github_t in
      let token = conf.Conf.token in
      let status = {
        new_status_state = status;
        new_status_target_url = None;
        new_status_description = text;
        new_status_context = Some conf.Conf.name;
      } in
      Github.Status.create
        ~token ~user:pr.base.repo.user ~repo:pr.base.repo.name
        ~status ~sha:pr.head.sha ()
      >>~ fun _ -> return ()

  let pr conf repo num =
    let open Github.Monad in
    let open Github_t in
    Github.Pull.get ~user:repo.user ~repo:repo.name ~num () >|=
    Github.Response.value

  let needing_check conf repo =
    let open Github.Monad in
    let open Github_t in
    let token = conf.Conf.token in
    Github.Pull.for_repo
      ~token ~state:`Open ~user:conf.repo.user ~repo:conf.repo.name ()
    |> Github.Stream.map (fun pr ->
        let stream = Github.Issue.comments
            ~token ~user:repo.user ~repo:repo.name ~num:pr.pull_number ()
        in
        Github.Stream.find
          (fun { issue_comment_user = u; _ } -> u.user_login = conf.Conf.name)
          stream
        >>= function
        | Some ({ issue_comment_body = b; _ },_) ->
          begin
            try Scanf.sscanf b "Commit: %s\n"
                  (fun c ->
                     if String.equal c pr.pull_head.branch_sha
                     then return []
                     else return [pr.pull_number])
            with _ -> return [pr.pull_number] end
        | None -> return [pr.pull_number]
      )
    |> Github.Stream.to_list

  let eval : type a. Conf.t -> repo -> a req -> a Github.Monad.t =
    fun conf repo req ->
      let open Github.Monad in
      let open Github_t in
      match req with
      | Count_previous_posts pr -> count_previous_posts conf pr
      | Comment (pr,body) -> comment conf pr body
      | Status (pr,s,text) -> status conf pr s text
      | Pr num -> pr conf repo num
      | Needing_check -> needing_check conf repo


  let gh_stream, gh_push = Lwt_stream.create ()

  let request : type a. a req -> a Lwt.t = fun req ->
    let (promise, resolve) = Lwt.wait () in
    gh_push ( Some ( R ( req, resolve ) ) );
    promise

  let loop ?(ntries=3) ?(retry_interval=60.) ~conf () =
    let step () =
      match%lwt Lwt_stream.next gh_stream with
      | exception exn ->
        log "Event handler failed: %s" (Printexc.to_string exn);
        Lwt.return (Github.Monad.return ())
      | R ( req, resolve ) ->
        let repo = conf.Conf.repo in
        let rec evaln n =
          ( let open Github.Monad in
            catch
              (fun () -> eval conf repo req >>= fun res ->
                Lwt.wakeup_later resolve res; return () )
              (fun exn ->
                 if n > 0
                 then ( embed (Lwt_unix.sleep retry_interval) >>= fun () -> evaln (pred n) )
                 else ( Lwt.wakeup_later_exn resolve exn; return () ) ) )
        in
        Lwt.return @@ evaln ntries
    in
    let rec looper () =
      let open Github.Monad in
      step () |> embed |> bind (fun x -> x) >>= looper
    in
    Github.Monad.run @@ looper ()

end

module RepoGit = struct

  module M = OpamStd.String.Map

  type t = repo

  let github_repo_string repo =
    Printf.sprintf "https://%sgithub.com/%s/%s.git"
      (match repo.auth with
       | None -> ""
       | Some (user, token) -> Printf.sprintf "%s:%s@" user token)
      repo.user repo.name

  let github_repo repo =
    Uri.of_string @@ github_repo_string repo


  let local_mirror repo =
    Fpath.v (Fmt.strf "./%s%%%s.git" repo.user repo.name)

  let write_lock = Lwt_mutex.create ()

  let git
      ?(can_fail=false) ?(silent_fail=false) ?(verbose=verbose) ?(writing=false)
      repo ?env ?input args =
    let cmd = Array.of_list ("git" :: "-C" :: (Fpath.to_string (local_mirror repo)) :: args) in
    let str_cmd =
      OpamStd.List.concat_map " "
        (fun s -> if String.contains s ' ' then Printf.sprintf "%S" s else s)
        (Array.to_list cmd) in
    if verbose then log "+ %s" str_cmd;
    let env =
      match env with
      | None -> None
      | Some e -> Some (Array.append (Unix.environment ()) e)
    in
    let git_call () =
      let p = Lwt_process.open_process ("git", cmd) ?env in
      let ic = p#stdout in
      let oc = p#stdin in
      let%lwt r = (
        let%lwt () = (match input with
            | None -> Lwt.return_unit
            | Some s -> Lwt_io.write oc s
          ) [%lwt.finally Lwt_io.close oc]
        in
        Lwt_io.read ic
      ) [%lwt.finally Lwt_io.close ic ]
      in
      if verbose then
        List.iter (fun s -> print_string "- "; print_endline s)
          (OpamStd.String.split r '\n');
      match%lwt p#close with
      | Unix.WEXITED 0 -> Lwt.return r
      | Unix.WEXITED i ->
        if not silent_fail then log "ERROR: command %s returned %d" str_cmd i;
        if can_fail then Lwt.return r else Lwt.fail (Failure str_cmd)
      | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
        log "ERROR: command %s interrupted" str_cmd;
        Lwt.fail (Failure str_cmd)
    in
    FdPool.with_fd @@ if writing then (fun () -> Lwt_mutex.with_lock write_lock git_call) else git_call

  let get repo =
    OpamSystem.mkdir (Fpath.to_string (local_mirror repo));
    let%lwt _ = git ~writing:true repo ["init"] in
    let%lwt _ = git ~writing:true repo ["config"; "receive.denyCurrentBranch"; "ignore"] in
    Lwt.return (Ok repo)

  let get_file t sha path =
    try%lwt
      git t ~verbose:false ~silent_fail:true ["show"; sha ^":"^ path]
      >|= OpamStd.Option.some
    with Failure _ -> Lwt.return None

  let get_file_exn t sha path =
    match%lwt get_file t sha path with
    | Some f -> Lwt.return f
    | None ->
      log "GET_FILE %s: not found" path;
      Lwt.fail Not_found

  let get_blob t sha =
    try%lwt
      git t ~verbose:false ~silent_fail:false ["cat-file"; "blob"; sha]
      >|= OpamStd.Option.some
    with Failure _ -> Lwt.return_none

  let get_blob_exn t sha =
    match%lwt get_blob t sha with
    | Some f -> Lwt.return f
    | None -> log "GET_BLOB %s: not found" sha; Lwt.fail Not_found

  let branch_reference name = "refs/heads/" ^ name

  let pr_branch pr = "pr/" ^ (string_of_int pr.number)

  let get_branch t branch =
    git t ["rev-parse"; branch_reference branch]
    >|= String.trim

  let set_branch t name commit_hash =
    git t ["branch"; "-f"; branch_reference name; commit_hash]
    >|= ignore

  let fetch t ?(manual_branches=[]) ?(branches=[]) repo =
    let remote b = "refs/remotes/" ^ repo.user ^ "/" ^ b in
    let b =
      manual_branches @
      List.map (fun b -> "+" ^ branch_reference b ^ ":" ^ remote b) branches
    in
    let%lwt _ = git ~writing:true t ("fetch" :: Uri.to_string (github_repo repo) :: b) in
    Lwt_list.map_s (fun b ->
        log "fetched %s" b;
        git t ["rev-parse"; remote b] >|= String.trim)
      branches

  let fetch_pr pull_request t =
    let%lwt _ =
      fetch t ~branches:[pull_request.base.ref] pull_request.base.repo
    in
    log "fetched upstream";
    let%lwt _head_fetch =
      let prn = string_of_int pull_request.number in
      fetch t ~manual_branches:[ "+pull/" ^ prn ^"/head:pr/" ^ prn ] pull_request.base.repo
    in
    log "fetched user pr";
    Lwt.return_unit

  let push ?(force=false) t branch repo =
    git t ["push"; Uri.to_string (github_repo repo);
           (if force then "+" else "") ^
           branch_reference branch ^":"^ branch]
    >|= ignore

  let common_ancestor pull_request t =
    git t ["merge-base"; pull_request.base.sha; pull_request.head.sha ]
    >|= String.trim

  let changed_files base head t =
    git t ["diff-tree"; "-r"; "--name-only"; "--diff-filter=ACMRD"; base; head]
    >>= fun s ->
    let paths = OpamStd.String.split s '\n' in
    Lwt_list.map_s (fun p -> get_file t head p >|= fun c -> p, c) paths

  let opam_hash_and_file_re =
    Re.(compile @@ seq [
        bos;
        repn digit 6 (Some 6);
        str " blob ";
        group @@ repn xdigit 40 (Some 40);
        char '\t';
        group @@
        seq
          [
            str "packages/";
            rep1 @@ diff any (char '/');
            opt @@ seq [char '/'; rep1 @@ diff any (char '/')];
            str "/opam";
          ];
        eos;
      ])

  let opam_files t sha =
    git t ["ls-tree"; "-r"; sha; "packages/"]
    >|= (fun s -> OpamStd.String.split s '\n')
    >>= Lwt_list.filter_map_p (fun s ->
        match Re.exec_opt opam_hash_and_file_re s with
        | None -> Lwt.return_none
        | Some g ->
          let hash = Re.Group.get g 1 and f = Re.Group.get g 2 in
          let filename = OpamFile.make (OpamFilename.of_string f) in
          try%lwt
            let%lwt opam = get_blob_exn t hash in
            Lwt.return_some (OpamFile.OPAM.read_from_string ~filename opam)
          with _ -> Lwt_io.printlf "failed on %s" f >>= fun () -> Lwt.return_none)


  (* returns a list (rel_filename * contents) *)
  let extra_files t sha package =
    let ( / ) a b = a ^ "/" ^ b in
    let dir =
      "packages" /
      OpamPackage.name_to_string package /
      OpamPackage.to_string package /
      "files" / ""
    in
    git t ["ls-tree"; "-r"; "--name-only"; sha; dir]
    >|= (fun s -> OpamStd.String.split s '\n')
    >|= List.sort compare
    >|= List.rev
    >>= Lwt_list.map_s (fun f ->
        let%lwt contents = get_file_exn t sha f in
        Lwt.return (OpamStd.String.remove_prefix ~prefix:dir f, contents))

end

module Git = struct
  module User = struct
    type user = {
      name: string;
      email: string;
      date: int64 * unit option;
    }
  end
end

module FormatUpgrade = struct

  let git_identity () = {
    Git.User.
    name = "Camelus";
    email = "opam-commits@lists.ocaml.org";
    date = Int64.of_float (Unix.time ()), None;
  }

  let get_updated_opam commit gitstore nv =
    let opam_dir =
      Printf.sprintf "packages/%s/%s/"
        (OpamPackage.name_to_string nv)
        (OpamPackage.to_string nv)
    in
    let opam_file = opam_dir^"opam" in
    let%lwt opam_str = RepoGit.get_file_exn gitstore commit opam_file in
    let%lwt url_str = RepoGit.get_file gitstore commit (opam_dir^"url") in
    let%lwt descr_str = RepoGit.get_file gitstore commit (opam_dir^"descr") in
    let opam =
      OpamFile.OPAM.read_from_string
        ~filename:(OpamFile.make (OpamFilename.of_string opam_file))
        opam_str
    in
    let opam = match descr_str with
      | None -> opam
      | Some d ->
        OpamFile.OPAM.with_descr (OpamFile.Descr.read_from_string d) opam
    in
    let opam = match url_str with
      | None -> opam
      | Some u ->
        OpamFile.OPAM.with_url (OpamFile.URL.read_from_string u) opam
    in
    let opam = OpamFormatUpgrade.opam_file ~quiet:true opam in
    let%lwt extra_files =
      RepoGit.extra_files gitstore commit nv >>=
      Lwt_list.map_s (fun (f, contents) ->
          Lwt.return
            (OpamFilename.Base.of_string f,
             OpamHash.compute_from_string contents))
    in
    let opam = OpamFile.OPAM.with_extra_files extra_files opam in
    let opam_str =
      OpamFile.OPAM.to_string_with_preserved_format
        ~format_from_string:opam_str
        (OpamFile.make (OpamFilename.of_string opam_file))
        opam
    in
    Lwt.return opam_str

  module CompilerConversion = struct
    (* Taken from OpamAdminRepoUpgrade ; should be generalised and called *)
    open OpamStd.Option.Op
    open OpamProcess.Job.Op

    let cache_file : string list list OpamFile.t =
      OpamFile.make @@
      OpamFilename.of_string "~/.cache/opam-compilers-to-packages/url-hashes"

    let get_url_md5, save_cache =
      let url_md5 = Hashtbl.create 187 in
      let () =
        OpamFile.Lines.read_opt cache_file +! [] |> List.iter @@ function
        | [url; md5] ->
          Hashtbl.add url_md5 (OpamUrl.of_string url) (OpamHash.of_string md5)
        | _ -> failwith "Bad cache, run 'opam admin upgrade --clear-cache'"
      in
      (fun url ->
         try Done (Some (Hashtbl.find url_md5 url))
         with Not_found ->
           OpamFilename.with_tmp_dir_job @@ fun dir ->
           OpamProcess.Job.ignore_errors ~default:None
             (fun () ->
                OpamDownload.download ~overwrite:false url dir @@| fun f ->
                let hash = OpamHash.compute (OpamFilename.to_string f) in
                Hashtbl.add url_md5 url hash;
                Some hash)),
      (fun () ->
         Hashtbl.fold
           (fun url hash l -> [OpamUrl.to_string url; OpamHash.to_string hash]::l)
           url_md5 [] |>
         OpamFile.Lines.write cache_file)

    let opam_of_comp comp_name comp descr =
      let nv =
        match OpamStd.String.cut_at comp_name '+' with
        | None ->
          OpamPackage.create (OpamPackage.Name.of_string "ocaml-base-compiler")
            (OpamPackage.Version.of_string comp_name)
        | Some (version,variant) ->
          OpamPackage.create (OpamPackage.Name.of_string "ocaml-variants")
            (OpamPackage.Version.of_string (version^"+"^variant))
      in
      let opam =
        OpamFormatUpgrade.comp_file ~package:nv ?descr comp |>
        OpamFile.OPAM.with_conflict_class
          [OpamPackage.Name.of_string "ocaml-core-compiler"]
      in
      let opam =
        match OpamFile.OPAM.url opam with
        | Some urlf when OpamFile.URL.checksum urlf = [] ->
          (match OpamProcess.Job.run (get_url_md5 (OpamFile.URL.url urlf)) with
           | None ->
             Printf.ksprintf failwith "Could not get the archive of %s."
               (OpamPackage.to_string nv)
           | Some hash ->
             OpamFile.OPAM.with_url (OpamFile.URL.with_checksum [hash] urlf)
               opam)
        | _ -> opam
      in
      let patches = OpamFile.Comp.patches comp in
      if patches <> [] then
        log "Fetching patches of %s to check their hashes...\n"
          (OpamPackage.to_string nv);
      let extra_sources =
        (* Download them just to get their MD5 *)
        OpamParallel.map
          ~jobs:3
          ~command:(fun url ->
              get_url_md5 url @@| function
              | Some md5 -> url, md5
              | None ->
                Printf.ksprintf failwith
                  "Could not get patch file for %s from %s, skipping"
                  (OpamPackage.to_string nv) (OpamUrl.to_string url))
          (OpamFile.Comp.patches comp)
      in
      OpamFile.OPAM.with_extra_sources
        (List.map (fun (url, hash) ->
             OpamFilename.Base.of_string (OpamUrl.basename url),
             OpamFile.URL.create ~checksum:[hash] url)
            extra_sources)
        opam

  end

  let get_compiler_opam commit gitstore comp_name =
    let bname =
      Printf.sprintf "compilers/%s/%s/%s"
        (match OpamStd.String.cut_at comp_name '+'
         with Some (v,_) -> v | None -> comp_name)
        comp_name comp_name
    in
    let filename = bname^".comp" in
    let%lwt comp_str = RepoGit.get_file_exn gitstore commit filename in
    let%lwt descr_str = RepoGit.get_file gitstore commit (bname^".descr") in
    let comp =
      OpamFile.Comp.read_from_string
        ~filename:(OpamFile.make (OpamFilename.of_string filename))
        comp_str
    in
    let descr =
      OpamStd.Option.map OpamFile.Descr.read_from_string descr_str
    in
    let opam =
      CompilerConversion.opam_of_comp comp_name comp descr
    in
    let opam_str =
      OpamFile.OPAM.write_to_string
        (opam
         |> OpamFile.OPAM.with_name_opt None
         |> OpamFile.OPAM.with_version_opt None)
      ^"\n"
    in
    Lwt.return (OpamFile.OPAM.package opam, opam_str)

  let pkg_of_comp c =
    let ocaml_official_pkgname = OpamPackage.Name.of_string "ocaml-base-compiler" in
    let ocaml_variants_pkgname = OpamPackage.Name.of_string "ocaml-variants" in
    match OpamStd.String.cut_at c '+' with
    | None ->
      OpamPackage.create ocaml_official_pkgname
        (OpamPackage.Version.of_string c)
    | Some (version,variant) ->
      OpamPackage.create ocaml_variants_pkgname
        (OpamPackage.Version.of_string (version^"+"^variant))

  let get_updated_subtree commit gitstore changed_files =
    let compilers, packages, files, removed =
      List.fold_left (fun (compilers, packages, files, removed) (f, contents) ->
          try Scanf.sscanf f "compilers/%_s@/%s@/"
                (fun s ->
                   if contents = None then
                     compilers, packages, files,
                     OpamPackage.Set.add (pkg_of_comp s) removed
                   else
                     OpamStd.String.Set.add s compilers,
                     packages, files, removed)
          with Scanf.Scan_failure _ -> try
              Scanf.sscanf f "packages/%_s@/%s@/%s@/"
                (fun s -> function
                   | "opam" when contents = None ->
                     compilers, packages, files,
                     OpamPackage.Set.add (OpamPackage.of_string s) removed
                   | "opam" | "url" | "descr" ->
                     compilers,
                     OpamPackage.Set.add (OpamPackage.of_string s) packages,
                     files, removed
                   | "files" ->
                     compilers, packages,
                     OpamStd.String.Map.add f contents files,
                     removed
                   | _ -> compilers, packages, files, removed)
            with Scanf.Scan_failure _ -> compilers, packages, files, removed)
        (OpamStd.String.Set.empty,
         OpamPackage.Set.empty,
         OpamStd.String.Map.empty,
         OpamPackage.Set.empty)
        changed_files
    in
    let%lwt compiler_packages =
      Lwt_list.fold_left_s (fun acc comp_name ->
          let%lwt nv, opam = get_compiler_opam commit gitstore comp_name in
          Lwt.return (OpamPackage.Map.add nv opam acc))
        OpamPackage.Map.empty
        (OpamStd.String.Set.elements compilers)
    in
    let%lwt upgraded_packages =
      Lwt_list.fold_left_s (fun acc nv ->
          try%lwt
            let%lwt opam = get_updated_opam commit gitstore nv in
            Lwt.return (OpamPackage.Map.add nv opam acc)
          with Not_found -> Lwt.return acc)
        compiler_packages
        (OpamPackage.Set.elements packages)
    in
    let pkg_filename nv =
      Printf.sprintf "packages/%s/%s/opam"
        (OpamPackage.name_to_string nv)
        (OpamPackage.to_string nv)
    in
    Lwt.return @@
    (OpamPackage.keys upgraded_packages,
     removed,
     OpamPackage.Map.fold (fun nv opam ->
         OpamStd.String.Map.add (pkg_filename nv) (Some opam))
       upgraded_packages @@
     OpamPackage.Set.fold (fun nv ->
         OpamStd.String.Map.add (pkg_filename nv) None)
       removed @@
     files)

(*
  let rec add_file_to_tree gitstore tree path contents =
    let add_to_tree entry t =
      let name = entry.S.Value.Tree.name in
      S.Value.Tree.of_list
        (entry ::
         List.filter (fun e -> e.S.Value.Tree.name <> name)
           (S.Value.Tree.to_list t))
    in
    match path with
    | [] -> Lwt.fail (Failure "Empty path")
    | [file] ->
      (match%lwt
         S.write gitstore
           (S.Value.blob (S.Value.Blob.of_string contents))
       with
       | Ok (hash, i) ->
         let entry = { S.Value.Tree.perm = `Normal; name = file; node = hash } in
         Lwt.return (add_to_tree entry tree)
       | Error s -> Lwt.fail (Failure "Could not write new blob to git"))
    | dir::path ->
      let subtree =
        try
          Some (List.find
                  (fun e ->
                     e.S.Value.Tree.name = dir && e.S.Value.Tree.perm = `Dir)
                  (S.Value.Tree.to_list tree)).S.Value.Tree.node
        with Not_found -> None
      in
      let%lwt subtree =
        match subtree with
        | Some h -> RepoGit.get_tree gitstore h
        | None -> Lwt.return (S.Value.Tree.of_list [])
      in
      let%lwt subtree = add_file_to_tree gitstore subtree path contents in
      match%lwt S.write gitstore (S.Value.tree subtree) with
      | Ok (hash, _) ->
        let entry = { S.Value.Tree.perm = `Dir; name = dir; node = hash } in
        Lwt.return (add_to_tree entry tree)
      | Error e -> Lwt.fail (Failure "Could not write new subtree")

  let get ~err x =
    match%lwt x with
    | Error e -> Lwt.fail (Failure (Fmt.strf "%a" err e))
    | Ok (x, _) -> Lwt.return x
*)
  let gen_upgrade_commit
      ~merge changed_files head onto gitstore author message =
    let%lwt packages, removed_packages, replace_files =
      get_updated_subtree head gitstore changed_files
    in
    if OpamPackage.Set.(is_empty packages && is_empty removed_packages) &&
       OpamStd.String.Map.is_empty replace_files
    then Lwt.return None
    else
    let%lwt _ =
      RepoGit.git gitstore
        ["reset"; "-q"; "--mixed"; if merge then onto else head]
    in
    let%lwt () =
      Lwt_list.iter_s (fun (path, contents) ->
          match contents with
          | Some contents ->
            let%lwt hash =
              RepoGit.git gitstore ["hash-object"; "-w"; "--stdin"] ~input:contents
              >|= String.trim
            in
            let%lwt _ =
              RepoGit.git gitstore
                ["update-index"; "--ignore-missing"; "--add";
                 "--cacheinfo"; "100644,"^hash^","^path]
            in
            Lwt.return_unit
          | None ->
            let%lwt _ =
              RepoGit.git gitstore
                ["update-index"; "--ignore-missing"; "--remove"; "--"; path]
            in
            Lwt.return_unit)
        (OpamStd.String.Map.bindings replace_files)
    in
    let%lwt tree = RepoGit.git gitstore ["write-tree"] >|= String.trim in
    let committer = git_identity () in
    let env = [|
      "GIT_AUTHOR_NAME="^ author.Git.User.name;
      "GIT_AUTHOR_EMAIL="^ author.Git.User.email;
      "GIT_COMMITTER_NAME="^ committer.Git.User.name;
      "GIT_COMMITTER_EMAIL="^ committer.Git.User.email;
    |] in
    let message =
      message (OpamPackage.Set.elements
                 (OpamPackage.Set.union packages removed_packages))
    in
    RepoGit.git gitstore ~env
      ("commit-tree" ::
       "-m" :: message ::
       (if merge then ["-p"; onto] else []) @
       [ "-p"; head;
         tree ])
    >|= String.trim
    >|= fun hash -> Some (hash, message)

  (** We have conflicts if [onto] was changed in the meantime, i.e. the rewrite
      of [ancestor] doesn't match what we have at the current [onto]. This is
      the case where we don't want to force an overwrite *)
  let check_for_conflicts changed_files ancestor onto gitstore =
    let%lwt changed_files_on_ancestor =
      Lwt_list.map_s (fun (f, _) ->
          RepoGit.get_file gitstore ancestor f >|= fun c -> f, c)
        changed_files
    in
    let%lwt _packages, _removed, rewritten_ancestor_tree =
      get_updated_subtree ancestor gitstore
        changed_files_on_ancestor
    in
    let rec changed = function
      | (path, contents) :: r ->
        let%lwt c = RepoGit.get_file gitstore onto path in
        if c <> contents then
          (log "Conflict on %s:\n<<<<<<\n%s======\n%s>>>>>>" path
             (OpamStd.Option.to_string (fun s -> s) contents)
             (OpamStd.Option.to_string (fun s -> s) c);
           changed r >>= fun acc -> Lwt.return (path::acc))
        else
          changed r
      | [] -> Lwt.return []
    in
    changed (OpamStd.String.Map.bindings rewritten_ancestor_tree)

  let run base_branch onto_branch ancestor head_hash gitstore repo =
    log "Format upgrade: %s to %s" base_branch onto_branch;
    let%lwt _head_hash, onto_hash =
      match%lwt
        RepoGit.fetch
          ~branches:[base_branch; onto_branch]
          gitstore repo
      with
      | [head_hash; onto_hash] -> Lwt.return (head_hash, onto_hash)
      | _ -> Lwt.fail (Failure "Branch fetch failed")
    in
    (* assert (head = head_hash); *)
    (* let%lwt remote_onto =
     *   try Lwt.return (List.assoc (RepoGit.branch_reference onto_branch) refs)
     *   with Not_found -> Lwt.fail (Failure ("Branch "^onto_branch^" not found"))
     * in *)
    (* let%lwt remote_onto =
     *   RepoGit.get_branch gitstore ("origin/"^onto_branch)
     * in *)
    log "Fetched new commits: head %s onto %s" head_hash onto_hash;
    (* let%lwt _ =
     *   RepoGit.set_branch gitstore onto_branch remote_onto
     * in
     * log "Updated branch"; *)
    try%lwt
      (* let%lwt onto_head = RepoGit.get_commit gitstore onto_hash in
       * let%lwt head_commit = RepoGit.get_commit gitstore head_hash in *)
      let author = git_identity () in
      log "Rewriting commit %s (and possible parents)" (*" by %s"*)
        head_hash (* (S.Value.Commit.author head_commit).Git.User.name *);
      let%lwt changed_files =
        RepoGit.changed_files ancestor head_hash gitstore
      in
      let%lwt conflicts =
        check_for_conflicts changed_files ancestor onto_hash gitstore
      in
      let rec firstn n = if n <= 0 then fun _ -> ["..."] else function
          | x::r -> x::firstn (n-1) r
          | [] -> []
      in
      let message packages =
        if conflicts <> [] then
          Printf.sprintf
            "Partial format upgrade (%s)\n\n\
             Update done by Camelus based on opam-lib %s\n\
             This might overwrite changes done on the current %s branch, so it \
             was not automatically merged. Conflicting files:\n%s"
            (String.concat ", "
               (firstn 5 (List.map OpamPackage.to_string packages)))
            OpamVersion.(to_string (full ()))
            onto_branch
            (OpamStd.Format.itemize (fun s -> s) conflicts)
        else
          Printf.sprintf
            "Format upgrade merge (%s)\n\n\
             Merge done by Camelus based on opam-lib %s"
            (OpamStd.List.concat_map ", " OpamPackage.to_string packages)
            OpamVersion.(to_string (full ()))
      in
      match%lwt
        gen_upgrade_commit ~merge:true
          changed_files head_hash onto_hash gitstore author message
      with
      | None ->
        log "No changes needed to %s branch" onto_branch;
        Lwt.return None
      | Some (commit_hash, msg) ->
        let dest_branch =
          if conflicts <> [] then "camelus-"^(String.sub head_hash 0 8)
          else onto_branch
        in
        let%lwt _ = RepoGit.set_branch gitstore dest_branch commit_hash in
        log "Pushing new commit %s onto %s (there are %sconflicts)"
          ((* S.Hash.to_hex  *)commit_hash) dest_branch
          (if conflicts <> [] then "" else "no ");
        let%lwt () =
          RepoGit.push ~force:(conflicts<>[]) gitstore dest_branch repo
        in
        log "Upgrade done";
        Lwt.return (if conflicts <> [] then Some (dest_branch, msg) else None)
    with e ->
      log "Upgrade and push to branch %s failed: %s\n%s" onto_branch
        (Printexc.to_string e)
        (Printexc.get_backtrace ());
      Lwt.return None

end

module PrChecks = struct

  let pkg_to_string p = Printf.sprintf "`%s`" (OpamPackage.to_string p)

  let max_items_in_post = 50

  let changed_opam_files ancestor head gitstore =
    let%lwt files = RepoGit.changed_files ancestor head gitstore in
    Lwt.return @@
    let opamfiles, others = List.partition (fun (s,c) ->
        match c with
        | Some c when
            OpamStd.String.starts_with ~prefix:"packages/" s &&
            OpamStd.String.ends_with ~suffix:"/opam" s
          -> true
        | _ -> false)
        files
    in
    List.map (function (s, Some c) -> (OpamFilename.of_string s, c) | (_,None) -> assert false) opamfiles,
    List.map fst others

  let lint head gitstore opam_files =
    let%lwt lint =
      Lwt_list.map_s (fun (file,contents) ->
          let nv =
            match OpamPackage.of_filename file with
            | Some nv -> nv
            | None -> OpamPackage.of_string "invalid-package-name.v"
          in
          let%lwt check_extra_files =
            RepoGit.extra_files gitstore head nv >>=
            Lwt_list.map_s (fun (f, contents) ->
                Lwt.return
                  (OpamFilename.Base.of_string f,
                   fun h ->
                     OpamHash.compute_from_string ~kind:(OpamHash.kind h)
                       contents
                     = h))
          in
          let r, opamopt =
            OpamFileTools.lint_string ~check_extra_files
              (OpamFile.make file)
              contents
          in
          Lwt.return (file, r, opamopt))
        opam_files
    in
    let unwanted_warns = [] in
    let lint =
      List.map (fun (f,r,o) ->
          f, List.filter (fun (n,_,_) -> not (List.mem n unwanted_warns)) r, o)
        lint
    in
    let passed, failed =
      List.partition (function _, [], Some _ -> true | _ -> false) lint
    in
    let errors, warnings =
      List.partition (fun (_, we, _) ->
          List.exists (function _, `Error, _ -> true | _ -> false) we)
        failed
    in
    let title =
      if errors <> [] then
        "##### :cloud_with_lightning: opam-lint errors"
      else if warnings <> [] then
        "##### :sun_behind_small_cloud: opam-lint warnings"
      else if passed <> [] then
        "##### :sunny: All lint checks passed"
      else
        "##### :sunny: No new or changed opam files"
    in
    let title =
      Printf.sprintf "%s <small>%s</small>\n\n" title head
    in
    let pkgname (f,_,_) =
      OpamStd.Option.Op.(
        (OpamPackage.of_filename f >>| pkg_to_string)
        +! OpamFilename.to_string f)
    in
    let pass =
      OpamStd.List.concat_map ", "
        ~nil:""
        ~left:"* These packages passed lint tests: "
        ~right:"\n"
        pkgname passed
    in
    let warns =
      if List.length warnings + List.length errors > max_items_in_post then
        OpamStd.List.concat_map
          ~left:"* **Packages with warnings**: " ~right:"\n" ", "
          pkgname warnings
      else
        OpamStd.List.concat_map "\n\n"
          (fun ((_, warns, _) as fe) ->
             Printf.sprintf "* **%s** has some warnings:\n\n%s\n"
               (pkgname fe)
               (OpamStd.Format.itemize ~bullet:"  * "
                  (fun (num,_,msg) ->
                     Printf.sprintf "**warning %d**: %s" num msg)
                  warns))
          warnings
    in
    let errs =
      if List.length errors > max_items_in_post then
        OpamStd.List.concat_map
          ~left:"* **Packages with errors**: " ~right:"\n" ", "
          pkgname errors
      else
        OpamStd.List.concat_map "\n\n"
          (fun ((_, we, _) as fe) ->
             Printf.sprintf "* **%s** has errors:\n\n%s\n"
               (pkgname fe)
               (OpamStd.Format.itemize ~bullet:"  * "
                  (fun (num,kind,msg) ->
                     let kind = match kind with
                       | `Warning -> "warning"
                       | `Error -> "error"
                     in
                     Printf.sprintf "**%s %d:** %s" kind num msg)
                  we))
          errors
    in
    let status =
      if errors <> [] then `Errors (List.map pkgname errors)
      else if warnings <> [] then `Warnings (List.map pkgname warnings)
      else `Passed
    in
    Lwt.return
      (status, String.concat "" [title; errs; warns; pass])

  let get_universe gitstore sha ~name =
    let%lwt opams = RepoGit.opam_files gitstore sha in
    log "opam files at %s %s: %d" name sha (List.length opams);
    let open OpamTypes in
    let m =
      List.fold_left (fun m o ->
          let nv =
            OpamPackage.create
              (OpamFile.OPAM.name o) (OpamFile.OPAM.version o)
          in
          OpamPackage.Map.add nv o m)
        OpamPackage.Map.empty opams
    in
    let all_packages =
      OpamPackage.Set.of_list (OpamPackage.Map.keys m)
    in
    let env_global v =
      match OpamVariable.Full.scope v,
            OpamVariable.(to_string (Full.variable v))
      with
      | OpamVariable.Full.Global, "opam-version" ->
        Some (S OpamVersion.(to_string current))
      | OpamVariable.Full.Global, "with-test" -> Some (B false)
      | OpamVariable.Full.Global, "with-doc" -> Some (B false)
      | OpamVariable.Full.Global, "dev" -> Some (B false)
      | _ -> None
    in
    let env nv v =
      match OpamVariable.Full.scope v,
            OpamVariable.(to_string (Full.variable v))
      with
      | (OpamVariable.Full.Global | OpamVariable.Full.Self), "name" ->
        Some (S (OpamPackage.Name.to_string nv.name))
      | (OpamVariable.Full.Global | OpamVariable.Full.Self), "version" ->
        Some (S (OpamPackage.Version.to_string nv.version))
      | _ -> env_global v
    in
    Lwt.return {
      u_packages = all_packages;
      u_action = Query;
      u_installed = OpamPackage.Set.empty;
      u_available =
        OpamPackage.Map.filter (fun _ opam ->
            OpamFilter.eval_to_bool ~default:true env_global
              (OpamFile.OPAM.available opam))
          m
        |> OpamPackage.keys;
      u_depends =
        OpamPackage.Map.mapi
          (fun nv o ->
             OpamFile.OPAM.depends o |>
             OpamFilter.partial_filter_formula (env nv))
          m;
      u_depopts =
        OpamPackage.Map.mapi
          (fun nv o ->
             OpamFile.OPAM.depopts o |>
             OpamFilter.partial_filter_formula (env nv))
          m;
      u_conflicts =
        OpamPackage.Map.mapi
          (fun nv o ->
             OpamFile.OPAM.conflicts o |>
             OpamFilter.filter_formula ~default:false (env nv))
          m;
      u_installed_roots = OpamPackage.Set.empty;
      u_pinned = OpamPackage.Set.empty;
      u_base = OpamPackage.Set.empty;
      u_attrs = [];
      u_reinstall = OpamPackage.Set.empty;
    }

  let reverse_dependencies universe packages =
    OpamPackage.Set.union packages @@
    OpamPackage.Set.of_list @@
    OpamSolver.reverse_dependencies
      ~depopts:false ~build:true ~post:true ~installed:false ~unavailable:true
      universe packages

  let installable universe packages ~name =
    let packages =
      OpamPackage.Set.inter packages universe.OpamTypes.u_packages
    in
    log "At %s: among %d packages..." name (OpamPackage.Set.cardinal packages);
    let%lwt installable =
      Lwt_preemptive.detach (OpamSolver.installable_subset universe) packages
    in
    log "... %d are installable"
      (OpamPackage.Set.cardinal installable);
    Lwt.return (packages, installable)

  let installability_check ancestor head gitstore packages =
    let open OpamPackage.Set.Op in
    let%lwt univ_before = get_universe gitstore ancestor ~name:"ANCESTOR" in
    let%lwt univ_after = get_universe gitstore head ~name:"HEAD" in
    let consider_packages =
      reverse_dependencies univ_before packages ++
      reverse_dependencies univ_after packages
    in
    log "Considering %d related packages"
      (OpamPackage.Set.cardinal consider_packages);
    let%lwt packages_before, installable_before =
      installable univ_before consider_packages ~name:"ANCESTOR"
    in
    let%lwt packages_after, installable_after =
      installable univ_after consider_packages ~name:"HEAD"
    in
    let fresh = packages_after -- packages_before in
    let broken_before = packages_before -- installable_before in
    let broken_after = packages_after -- installable_after in
    let breaks = broken_after -- broken_before in
    let repairs = broken_before -- broken_after in
    let no_breaks = OpamPackage.Set.is_empty breaks in
    let title =
      Printf.sprintf "\n\n##### :%s: Installability check (%+d)\n\n"
        (if no_breaks then "sunny" else "sun_behind_small_cloud")
        (OpamPackage.Set.cardinal installable_after -
         OpamPackage.Set.cardinal installable_before)
    in
    let msg (s,set) =
      if OpamPackage.Set.is_empty set then None else
        Some (Printf.sprintf "%s (%d): %s" s
                (OpamPackage.Set.cardinal set)
                (OpamStd.List.concat_map " " pkg_to_string
                   (OpamPackage.Set.elements set)))
    in
    let status =
      if no_breaks then `Passed else
        `Errors (List.map pkg_to_string
                   (OpamPackage.Set.elements breaks))
    in
    Lwt.return (
      status,
      title ^
      OpamStd.Format.itemize ~bullet:"* " (fun s -> s) @@
      OpamStd.List.filter_map msg [
        "these releases are **not installable** anymore",
        breaks %% packages_before;
        "these releases can now be installed, well done",
        repairs %% packages_after;
        "new installable packages",
        fresh %% installable_after;
        "new **broken** packages",
        fresh -- installable_after;
        "removed broken packages",
        broken_before -- packages_after;
        "removed installable packages",
        installable_before -- packages_after;
      ]
    )

  let add_status st1 st2 = match st1, st2 with
    | `Errors a, `Errors b -> `Errors (a@b)
    | `Errors _ as e, _ | _, (`Errors _ as e) -> e
    | `Warnings a, `Warnings b -> `Warnings (a@b)
    | `Warnings _ as w, _ | _, (`Warnings _ as w) -> w
    | `Passed, `Passed -> `Passed

  let notice_misc_files = function
    | [] -> ""
    | l -> "\n\n---\n\n##### :sun_behind_small_cloud: " ^ (string_of_int @@ List.length l ) ^ " ignored non-opam files:\n\n" ^
           OpamStd.Format.itemize ~bullet:"* " (fun s -> s) l

  let msg_header ~conf pr =
    let%lwt hello_msg =
      begin match pr.pr_user with
        | "AltGr" -> Lwt.return "As you wish, master!"
        | "kit-ty-kate" -> Lwt.return "Good to see you Madam."
        | "thomasblanc" -> Lwt.return "Tom, are you trying to break me again?"
        | user ->
          Lwt.catch
            (fun () ->
               GH.request (Count_previous_posts pr) >>= fun l ->
                 Lwt.return @@
                 if l <= 1
                 then Printf.sprintf "Hello @%s! I believe this is your first contribution here. Please be nice, reviewers!" user
                 else if l <= 50
                 then Printf.sprintf "@%s has posted %d contributions." user l
                 else Printf.sprintf "A pull request by opam-seasoned @%s." user
            )
            (fun _ -> Lwt.return "I made an error retrieving the post by the user, sorry about that")
      end in
    Lwt.return @@ Printf.sprintf "Commit: %s\n\n%s\n\n" pr.head.sha hello_msg

  let run_nogit ~conf pr gitstore msghead =
    let head = pr.head.sha in
    let%lwt ancestor = RepoGit.common_ancestor pr gitstore in
    let%lwt opam_files, other_files = changed_opam_files ancestor head gitstore in
    let misc_files_body = notice_misc_files other_files in
    let%lwt (stlint,msglint) = lint head gitstore opam_files in
    let packages =
      List.fold_left (fun pkgs (f,_) -> match OpamPackage.of_filename f with
          | None -> pkgs
          | Some nv -> OpamPackage.Set.add nv pkgs)
        OpamPackage.Set.empty
        opam_files
    in
    try%lwt
      let%lwt (stinst,msginst) =
        installability_check ancestor head gitstore packages
      in
      Lwt.return (add_status stlint stinst,
                  msghead ^ msglint ^ "\n\n---\n" ^ msginst ^ misc_files_body)
    with e ->
      log "Installability check failed: %s%s" (Printexc.to_string e)
        (Printexc.get_backtrace ());
      Lwt.return (stlint, msghead ^ msglint ^ misc_files_body)

  let run ~conf pr gitstore =
    let%lwt () = RepoGit.fetch_pr pr gitstore in
    let%lwt msghead = msg_header ~conf pr in
    run_nogit ~conf pr gitstore msghead
end

module Github_comment = struct

  open Github_t

  let github_max_descr_length = 140

  (* let make_status ~name ~token pr ?text status =
   *   let status = {
   *     new_status_state = status;
   *     new_status_target_url = None;
   *     new_status_description = text;
   *     new_status_context = Some name;
   *   } in
   *   Github.Status.create
   *     ~token ~user:pr.base.repo.user ~repo:pr.base.repo.name
   *     ~status ~sha:pr.head.sha () *)

  let push_status ~name ~token pr ?text status =
    GH.request (Status ( pr, status, text))

  let make_status = function
    | `Passed ->
      `Success, "All tests passed"
    | `Warnings ps ->
      `Success,
      let m = "Warnings for "^String.concat ", " ps in
      if String.length m <= github_max_descr_length then m else
        Printf.sprintf "Warnings for %d packages" (List.length ps)
    | `Errors ps ->
      `Error,
      let m = "Errors for "^String.concat ", " ps in
      if String.length m <= github_max_descr_length then m else
        Printf.sprintf "Errors for %d packages" (List.length ps)


  let push_report ~name ~token ~report:(status,body) pr =
    let comment () =
      log "Commenting...";
      GH.request @@ Comment (pr,body) >>= fun cmturl ->
      Lwt.return @@ log "Comment posted on %s" cmturl
    in
    let push_status () =
      log "Pushing status...";
      let state, text = match status with
        | `Passed ->
          `Success, "All tests passed"
        | `Warnings ps ->
          `Success,
          let m = "Warnings for "^String.concat ", " ps in
          if String.length m <= github_max_descr_length then m else
            Printf.sprintf "Warnings for %d packages" (List.length ps)
        | `Errors ps ->
          `Error,
          let m = "Errors for "^String.concat ", " ps in
          if String.length m <= github_max_descr_length then m else
            Printf.sprintf "Errors for %d packages" (List.length ps)
      in
      push_status ~name ~token pr ~text state
    in
    comment () >>= fun () ->
    push_status () >>= fun () ->
    Lwt.return (log "Comment posted back to PR #%d" pr.number)

  (* let pull_request ~name ~token repo branch target_branch ?message title =
   *   let open Github.Monad in
   *   log "Pull-requesting...";
   *   let pr () =
   *     let rec find_pr stream =
   *       Github.Stream.next stream >>= function
   *       | Some (pr, s) ->
   *         if pr.pull_head.branch_ref = branch &&
   *            pr.pull_base.branch_ref = target_branch
   *         then return (Some pr)
   *         else find_pr s
   *       | None -> return None
   *     in
   *     find_pr (Github.Pull.for_repo
   *                ~token ~state:`Open ~user:repo.user ~repo:repo.name ())
   *     >>= function
   *     | None ->
   *       let pull = {
   *         new_pull_title = title;
   *         new_pull_body = message;
   *         new_pull_base = target_branch;
   *         new_pull_head = branch;
   *       } in
   *       Github.Pull.create ~token ~user:repo.user ~repo:repo.name ~pull ()
   *     | Some pr ->
   *       let update_pull = {
   *         update_pull_title = Some title;
   *         update_pull_body = message;
   *         update_pull_state = None;
   *         update_pull_base = None;
   *       } in
   *       Github.Pull.update ~token ~user:repo.user ~repo:repo.name
   *         ~num:pr.pull_number ~update_pull ()
   *   in
   *   GH.run (fun () ->
   *       pr () >>= fun resp ->
   *       return (log "Filed pull-request #%d" resp#value.pull_number)
   *     ) *)

end

module Webhook_handler = struct


  module Camelus_conf = Conf
  open Cohttp
  open Cohttp_lwt_unix

  let uri_path = "/opam-ci"
  let exp_method = `POST
  let exp_ua_prefix = "GitHub-Hookshot/"

  (** Check that the request is well-formed and originated from GitHub *)
  let check_github ~secret req body =
    let headers = Request.headers req in
    Uri.path (Request.uri req) = uri_path &&
    Request.meth req = exp_method &&
    OpamStd.Option.Op.(
      (Header.get headers "user-agent" >>|
       OpamStd.String.starts_with ~prefix:exp_ua_prefix) +! false) &&
    Header.get_media_type headers = Some "application/json" &&
    Header.get headers "x-github-event" <> None &&
    OpamStd.Option.Op.(
      Header.get headers "x-hub-signature" >>|
      OpamStd.String.remove_prefix ~prefix:"sha1=" >>|
      Nocrypto.Uncommon.Cs.of_hex >>|
      Cstruct.equal (Nocrypto.Hash.mac `SHA1 ~key:secret (Cstruct.of_string body))
    ) = Some true

  module JS = struct
    let (-.-) json key = match json with
      | `Assoc dic -> List.assoc key dic
      | _ -> log "field %s not found" key; raise Not_found
    let to_string = function
      | `String s -> s
      | `Null -> ""
      | j ->
        log "JSON error: not a string %s" (Yojson.Safe.to_string j);
        raise Not_found
    let to_int = function
      | `Int i -> i
      | j ->
        log "JSON error: not an int %s" (Yojson.Safe.to_string j);
        raise Not_found
  end

  let pull_request_of_json base_branch json =
    let open JS in
    match json -.- "action" |> to_string with
    | "opened" | "reopened" | "synchronize" ->
      let number = json -.- "number" |> to_int in
      let pr = json -.- "pull_request" in
      let full_repo r = {
        repo = { user = r -.- "user" -.- "login" |> to_string;
                 name = r -.- "repo" -.- "name" |> to_string;
                 auth = None; };
        ref = r -.- "ref" |> to_string;
        sha = r -.- "sha" |> to_string;
      } in
      let base = full_repo (pr -.- "base") in
      let head = full_repo (pr -.- "head") in
      if base.ref <> base_branch then
        (log "Ignoring PR to %S (!= %S)" base.ref base_branch; None)
      else
      let pr_user = pr -.- "user" -.- "login" |> to_string in
      let message =
        pr -.- "title" |> to_string,
        pr -.- "body" |> to_string
      in
      Some { number; base; head; pr_user; message }
    | a ->
      log "Ignoring %s PR action" a;
      None

  let push_event_of_json base_branch json =
    let open JS in
    let ref = json -.- "ref" |> to_string in
    let push_head = json -.- "after" |> to_string  in
    let push_ancestor = json -.- "before" |> to_string in
    let r = json -.- "repository" in
    let push_repo = {
      user = r -.- "owner" -.- "name" |> to_string;
      name = r -.- "name" |> to_string;
      auth = None;
    } in
    if (* RepoGit.GitStore.Reference.equal *)
      ((* RepoGit.GitStore.Reference.of_string *) ref) =
      (RepoGit.branch_reference base_branch)
    then
      Some { push_head; push_ancestor; push_repo }
    else
      (log "Ignoring push to %s" ref; None)

  let server ~(conf:Camelus_conf.t) ~handler =
    let port = conf.port and secret = conf.secret
    and base_branch = conf.base_branch in
    let callback (conn, _) req body =
      let%lwt body = Cohttp_lwt.Body.to_string body in
      if not (check_github ~secret req body) then
        (log "Ignored invalid request:\n%s"
           (OpamStd.Format.itemize (fun s -> s)
              (Uri.path (Request.uri req) ::
               Code.string_of_method (Request.meth req) ::
               OpamStd.Option.to_string (fun x -> x)
                 (Header.get_media_type (Request.headers req)) ::
               OpamStd.List.filter_map (Header.get (Request.headers req))
                 ["user-agent"; "content-type";
                  "x-hub-signature";  "x-github-event"; ]));
         Server.respond_not_found ())
      else
      match Yojson.Safe.from_string body with
      | exception Failure err ->
        log "Error: invalid json (%s):\n%s" err body;
        Server.respond_error ~body:"Invalid JSON" ()
      | json ->
        match Header.get (Request.headers req) "x-github-event" with
        | Some "pull_request" ->
          (match pull_request_of_json base_branch json with
           | Some pr ->
             let%lwt () = handler (`Pr pr) in
             Server.respond ~status:`OK ~body:Cohttp_lwt.Body.empty ()
           | None ->
             Server.respond ~status:`OK ~body:Cohttp_lwt.Body.empty ()
           | exception Not_found ->
             log "Error: could not get PR data from JSON:\n%s"
               (Yojson.Safe.to_string json);
             Server.respond_error ~body:"Invalid format" ())
        | Some "push" ->
          (match push_event_of_json base_branch json with
           | Some push ->
             let%lwt () = handler (`Push push) in
             Server.respond ~status:`OK ~body:Cohttp_lwt.Body.empty ()
           | None ->
             Server.respond ~status:`OK ~body:Cohttp_lwt.Body.empty ()
           | exception Not_found ->
             log "Error: could not get push event data from JSON:\n%s"
               (Yojson.Safe.to_string json);
             Server.respond_error ~body:"Invalid format" ())
        | Some a ->
          log "Ignored %s event" a;
          Server.respond ~status:`OK ~body:Cohttp_lwt.Body.empty ()
        | None ->
          Server.respond_error ~body:"Invalid format" ()
    in
    log "Listening on port %d" port;
    Server.create
      ~on_exn:(fun e -> log "Server exn: %s" (Printexc.to_string e))
      ~mode:(`TCP (`Port port))
      (Server.make ~callback ())
end

module Fork_handler = struct

  let forktable : (int,unit Lwt.t * Lwt_process.process_out) Hashtbl.t =
    Hashtbl.create 111

  let fork_sema = Semaphore.make 20

  let linearize_repo { user; name; auth; } =
    match auth with
    | None -> [| user; name; ""; "" |]
    | Some (a,b) -> [| user; name; a; b |]
  let delinearize_repo a offset =
    { user = a.(offset); name = a.(offset+1);
      auth =
        match a.(offset+2), a.(offset+3) with
        | "","" ->  None
        | a,b -> Some (a,b) }

  let l_repo = 4

  let linearize_full_ref { repo; ref; sha } =
    Array.append (linearize_repo repo) [| ref; sha; |]
  let delinearize_full_ref a offset =
    { repo = delinearize_repo a offset;
      ref = a.(offset+l_repo);
      sha = a.(offset+l_repo+1);
    }
  let l_fr = l_repo + 2

  let linearize_pr { number; base; head; pr_user; message = a,b; } =
    Array.concat [
      [| string_of_int number |];
      (linearize_full_ref base);
      (linearize_full_ref head);
      [|pr_user;a;b|]
    ]
  let delinearize_pr a offset =
    {
      number = int_of_string a.(offset);
      base = delinearize_full_ref a (succ offset);
      head = delinearize_full_ref a (succ offset + l_fr);
      pr_user = a.(succ offset + l_fr + l_fr);
      message = (a.(offset + l_fr + l_fr + 2), a.(offset + l_fr + l_fr + 3));
    }

  let gen_args conf pr =
    let%lwt msghead = PrChecks.msg_header ~conf pr in
    Lwt.return @@
    (conf.Conf.camelus_child_loc,
     Array.concat [ [|conf.Conf.camelus_child_loc|];
                   linearize_pr pr;
                   [|msghead|]; ] )

  let process ~conf pr =
    let commit = pr.head.sha in
    let num = pr.number in
    begin
      match Hashtbl.find_opt forktable num with
      | None -> Lwt.return_unit
      | Some (old_promise, old_process) ->
        old_process#terminate;
        Lwt.catch (fun () -> old_promise) (fun _ -> Lwt.return_unit)
    end >>= fun () ->
    let%lwt args = gen_args conf pr in
    let%lwt () = Semaphore.obtain fork_sema in
    let process =
      Lwt_process.open_process_out
        ~stdout:`Keep ~stderr:`Keep
        args
    in
    let waiter,wakener = Lwt.wait () in
    let promise =
      Lwt.finalize
        (fun () -> waiter >>=
          fun () -> process#status >>=
          fun stat -> (
            match stat with
            | Unix.WEXITED e ->
              log "pr %d commit %s handled, exit status %d" num commit e
            | Unix.WSIGNALED s ->
              log "pr %d commit %s killed, signal %d" num commit s
            | Unix.WSTOPPED s ->
              log "pr %d commit %s stopped, signal %d" num commit s
          );
          Lwt.return_unit)
        (fun () ->
           Hashtbl.remove forktable num;
           Semaphore.release fork_sema;
           Lwt.return_unit)
    in
    Hashtbl.replace forktable num (promise,process);
    Lwt.wakeup_later wakener ();
    Lwt.return_unit

  let pending_processes () =
    Hashtbl.fold (fun _ (p,_) l -> p::l) forktable []

end
