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

let log fmt = OpamConsole.msg (fmt ^^ "\n%!")

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

module RepoGit = struct

  module M = OpamStd.String.Map

  type t = repo

  let github_repo repo =
    Uri.of_string
      (Printf.sprintf "https://%sgithub.com/%s/%s.git"
         (match repo.auth with
          | None -> ""
          | Some (user, token) -> Printf.sprintf "%s:%s@" user token)
         repo.user repo.name)

  let local_mirror repo =
    Fpath.v (Fmt.strf "./%s%%%s.git" repo.user repo.name)

  let git ?(can_fail=false) ?(verbose=false) repo ?env ?input args =
    let save_dir = Sys.getcwd () in
    let cmd = Array.of_list ("git" :: args) in
    let str_cmd =
      OpamStd.List.concat_map " "
        (fun s -> if String.contains s ' ' then Printf.sprintf "%S" s else s)
        (Array.to_list cmd) in
    if verbose then log "+ %s" str_cmd;
    Sys.chdir (Fpath.to_string (local_mirror repo));
    let env =
      match env with
      | None -> None
      | Some e -> Some (Array.append (Unix.environment ()) e)
    in
    begin
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
        log "ERROR: command %s returned %d" str_cmd i;
        if can_fail then Lwt.return r else Lwt.fail (Failure str_cmd)
      | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
        log "ERROR: command %s interrupted" str_cmd;
        Lwt.fail (Failure str_cmd)
    end[%lwt.finally
      Sys.chdir save_dir;
      Lwt.return_unit
    ]

  let get repo =
    OpamSystem.mkdir (Fpath.to_string (local_mirror repo));
    let%lwt _ = git repo ["init"] in
    let%lwt _ = git repo ["config"; "receive.denyCurrentBranch"; "ignore"] in
    Lwt.return (Ok repo)

  let get_file t sha path =
    try%lwt
      git t ~verbose:false ["show"; sha ^":"^ path]
      >|= OpamStd.Option.some
    with Failure _ -> Lwt.return None

  let get_file_exn t sha path =
    match%lwt get_file t sha path with
    | Some f -> Lwt.return f
    | None ->
      log "GET_FILE %s: not found" path;
      Lwt.fail Not_found

  let branch_reference name = "refs/heads/" ^ name

  let get_branch t branch =
    git t ["rev-parse"; branch_reference branch]
    >|= String.trim

  let set_branch t name commit_hash =
    git t ["branch"; "-f"; branch_reference name; commit_hash]
    >|= ignore

  let fetch t ?(branches=[]) repo =
    let remote b = "refs/remotes/" ^ repo.user ^ "/" ^ b in
    let b =
      List.map (fun b -> "+" ^ branch_reference b ^ ":" ^ remote b) branches
    in
    let%lwt _ = git t ("fetch" :: Uri.to_string (github_repo repo) :: b) in
    Lwt_list.map_s (fun b -> git t ["rev-parse"; remote b] >|= String.trim)
      branches

  let fetch_pr pull_request t =
    let%lwt _ =
      fetch t ~branches:[pull_request.base.ref] pull_request.base.repo
    in
    log "fetched upstream";
    let%lwt _head_fetch =
      fetch t ~branches:[pull_request.head.ref] pull_request.head.repo
    in
    log "fetched user repo";
    Lwt.return_unit

  let push t branch repo =
    git t ["push"; Uri.to_string (github_repo repo);
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

  let opam_file_re =
    Re.(compile @@ seq [
        bos;
        str "packages/";
        rep1 @@ diff any (char '/');
        opt @@ seq [char '/'; rep1 @@ diff any (char '/')];
        str "/opam";
        eos;
      ])

  let opam_files t sha =
    git t ["ls-tree"; "-r"; "--name-only"; sha; "packages/"]
    >|= (fun s -> OpamStd.String.split s '\n')
    >|= List.filter (Re.execp opam_file_re)
    >>= Lwt_list.fold_left_s (fun acc f ->
        let filename = OpamFile.make (OpamFilename.of_string f) in
        try%lwt
          let%lwt opam = get_file_exn t sha f in
          Lwt.return (OpamFile.OPAM.read_from_string ~filename opam :: acc)
        with _ -> Lwt.return acc)
      []

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
        OpamFile.Comp.to_package ~package:nv comp descr |>
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
    >|= fun hash -> hash, message

  (** We have conflicts if [onto] was changed in the meantime, i.e. the rewrite
      of [ancestor] doesn't match what we have at the current [onto]. This is
      the case where we don't want to force an overwrite *)
  let check_for_conflicts changed_files ancestor onto gitstore =
    let%lwt _packages, _removed, rewritten_ancestor_tree =
      get_updated_subtree ancestor gitstore changed_files
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
      let%lwt commit_hash, msg =
        gen_upgrade_commit ~merge:true
          changed_files head_hash onto_hash gitstore author message
      in
      let dest_branch =
        if conflicts <> [] then "camelus-"^(String.sub head_hash 0 8)
        else onto_branch
      in
      let%lwt _ = RepoGit.set_branch gitstore dest_branch commit_hash in
      log "Pushing new commit %s onto %s (there are %sconflicts)"
        ((* S.Hash.to_hex  *)commit_hash) dest_branch
        (if conflicts <> [] then "" else "no ");
      let%lwt () = RepoGit.push gitstore dest_branch repo in
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

  let lint ancestor head gitstore =
    let%lwt files = RepoGit.changed_files ancestor head gitstore in
    let opam_files =
      OpamStd.List.filter_map (fun (s,c) ->
          match c with
          | Some c when
              OpamStd.String.starts_with ~prefix:"/packages/" s &&
              OpamStd.String.ends_with ~suffix:"/opam" s
            -> Some (s, c)
          | _ -> None)
        files
    in
    let lint =
      List.map (fun (file,contents) ->
          let r, opamopt =
            OpamFileTools.lint_string
              (OpamFile.make (OpamFilename.of_string file))
              contents
          in
          file, r, opamopt)
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
        (OpamPackage.(of_filename OpamFilename.(of_string f))
         >>| pkg_to_string)
        +! f)
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

  let installable gitstore sha =
    let%lwt opams = RepoGit.opam_files gitstore sha in
    log "opam files at %s: %d" sha (List.length opams);
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
    let packages =
      OpamPackage.Set.of_list (OpamPackage.Map.keys m)
    in
    let env nv v =
      match OpamVariable.Full.scope v,
            OpamVariable.(to_string (Full.variable v))
      with
      | (OpamVariable.Full.Global | OpamVariable.Full.Self), "name" ->
        Some (S (OpamPackage.Name.to_string nv.name))
      | (OpamVariable.Full.Global | OpamVariable.Full.Self), "version" ->
        Some (S (OpamPackage.Version.to_string nv.version))
      | OpamVariable.Full.Global, "opam-version" ->
        Some (S OpamVersion.(to_string current))
      | OpamVariable.Full.Global, "with-test" -> Some (B false)
      | OpamVariable.Full.Global, "with-doc" -> Some (B false)
      | OpamVariable.Full.Global, "dev" -> Some (B false)
      | _ -> None
    in
    let universe = {
      u_packages = packages;
      u_action = Query;
      u_installed = OpamPackage.Set.empty;
      u_available = packages; (* Todo: check for different constraints *)
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
    } in
    let%lwt installable =
      Lwt_preemptive.detach OpamSolver.installable universe
    in
    log "... of which %d installable" (OpamPackage.Set.cardinal installable);
    Lwt.return (packages, installable)

  let installability_check ancestor head gitstore =
    let%lwt packages_before, installable_before =
      installable gitstore ancestor
    in
    let%lwt packages_after, installable_after =
      installable gitstore head
    in
    let open OpamPackage.Set.Op in
    let fresh = packages_after -- packages_before in
    let broken_before = packages_before -- installable_before in
    let broken_after = packages_after -- installable_after in
    let breaks = broken_after -- broken_before in
    let repairs = broken_before -- broken_after in
    let no_breaks = OpamPackage.Set.is_empty breaks in
    let title =
      Printf.sprintf "\n\n##### :%s: Installability check (%d &rarr; %d)\n\n"
        (if no_breaks then "sunny" else "sun_behind_small_cloud")
        (OpamPackage.Set.cardinal installable_before)
        (OpamPackage.Set.cardinal installable_after)
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

  let run pr gitstore =
    let%lwt () = RepoGit.fetch_pr pr gitstore in
    let head = pr.head.sha in
    let%lwt ancestor = RepoGit.common_ancestor pr gitstore in
    let%lwt (stlint,msglint) = lint ancestor head gitstore in
    try%lwt
      let%lwt (stinst,msginst) = installability_check ancestor head gitstore in
      Lwt.return (add_status stlint stinst,
                  msglint ^ "\n\n---\n" ^ msginst)
    with e ->
        log "Installability check failed: %s%s" (Printexc.to_string e)
          (Printexc.get_backtrace ());
        Lwt.return (stlint, msglint)

end

module Github_comment = struct

  open Github.Monad
  open Github_t

  let github_max_descr_length = 140

  let make_status ~name ~token pr ?text status =
    let status = {
      new_status_state = status;
      new_status_target_url = None;
      new_status_description = text;
      new_status_context = Some name;
    } in
    Github.Status.create
      ~token ~user:pr.base.repo.user ~repo:pr.base.repo.name
      ~status ~sha:pr.head.sha ()

  let push_status ~name ~token pr ?text status =
    run (make_status ~name ~token pr ?text status)

  let push_report ~name ~token ~report:(status,body) pr =
    let user = pr.base.repo.user in
    let repo = pr.base.repo.name in
    let num = pr.number in
    let comment () =
      log "Commenting...";
      let rec find_comment stream =
        Github.Stream.next stream >>= function
        | Some (c, s) ->
          if c.issue_comment_user.user_login = name then return (Some c)
          else find_comment s
        | None -> return None
      in
      find_comment (Github.Issue.comments ~token ~user ~repo ~num ())
      >>= function
      | None ->
        Github.Issue.create_comment ~token ~user ~repo ~num ~body ()
      | Some { issue_comment_id = id; _ } ->
        Github.Issue.update_comment ~token ~user ~repo ~id ~body ()
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
      make_status ~name ~token pr ~text state
    in
    run (
      comment () >>= fun _ ->
      push_status () >>= fun _ ->
      return (log "Comment posted back to PR #%d" pr.number);
    )

  let pull_request ~name ~token repo branch target_branch ?message title =
    log "Pull-requesting...";
    let pr () =
      let pull = {
        new_pull_title = title;
        new_pull_body = message;
        new_pull_base = target_branch;
        new_pull_head = branch;
      } in
      Github.Pull.create ~token ~user:repo.user ~repo:repo.name ~pull ()
    in
    run (
      pr () >>= fun resp ->
      return (log "Filed pull-request #%d" resp#value.pull_number)
    )

end

module Webhook_handler = struct

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

  let server ~port ~secret ~handler base_branch dest_branch =
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
         OpamPp.of_pair "secret" Cstruct.(of_string ?allocator:None, to_string));
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
    ]

    let pp =
      OpamFormat.I.map_file @@
      OpamFormat.I.fields ~name:internal ~empty fields -|
      OpamFormat.I.show_errors ~name:internal ~strict:true ()
  end
  include C
  include OpamFile.SyntaxFile(C)
end

let () =
  Logs.(set_reporter (format_reporter ()); set_level (Some Info));
  let conf =
    let f = if Array.length Sys.argv > 1 then Sys.argv.(1) else "opam-ci.conf" in
    let f = OpamFile.make (OpamFilename.of_string f) in
    try Conf.read f with e ->
      Printf.eprintf "Invalid conf file %s:\n%s\n"
        (OpamFile.to_string f) (Printexc.to_string e);
      exit 3
  in
  let auth = conf.Conf.name, Github.Token.to_string conf.Conf.token in
  let event_stream, event_push = Lwt_stream.create () in
  let pr_checker = List.mem `Pr_checker conf.Conf.roles in
  let push_upgrader = List.mem `Push_upgrader conf.Conf.roles in
  let rec check_loop gitstore =
    (* The checks are done sequentially *)
    let%lwt () =
      try%lwt
        match%lwt Lwt_stream.next event_stream with
        | `Pr pr when pr_checker ->
          (log "=> PR #%d received \
                (onto %s/%s#%s from %s/%s#%s, commit %s over %s)"
             pr.number
             pr.base.repo.user pr.base.repo.name pr.base.ref
             pr.head.repo.user pr.head.repo.name pr.head.ref
             pr.head.sha pr.base.sha;
           try%lwt
             let%lwt report = PrChecks.run pr gitstore in
             Github_comment.push_report
               ~name:conf.Conf.name
               ~token:conf.Conf.token
               ~report
               pr
           with exn ->
             log "Check failed: %s" (Printexc.to_string exn);
             let%lwt _ =
               Github_comment.push_status
                 ~name:conf.Conf.name ~token:conf.Conf.token pr
                 ~text:"Could not complete" `Failure
             in
             Lwt.return_unit)
        | `Push p when push_upgrader ->
          (log "=> Push received (head %s onto %s)"
             p.push_head p.push_ancestor;
           let%lwt pr_branch =
             try%lwt
               FormatUpgrade.run conf.Conf.base_branch conf.Conf.dest_branch
                 p.push_ancestor p.push_head gitstore
                 { p.push_repo with auth = Some auth }
             with exn ->
               log "Upgrade commit failed: %s" (Printexc.to_string exn);
               Lwt.return None
           in
           match pr_branch with
           | None -> Lwt.return_unit
           | Some (branch, msg) ->
             let title, message =
               match OpamStd.String.cut_at msg '\n' with
               | Some (t, m) -> t, Some (String.trim m)
               | None -> "Merge changes from 1.2 format repo", None
             in
             try%lwt
               Github_comment.pull_request
                 ~name:conf.Conf.name ~token:conf.Conf.token conf.Conf.repo
                 branch conf.Conf.dest_branch
                 ?message title
             with exn ->
               log "Pull request failed: %s" (Printexc.to_string exn);
               Lwt.return_unit)
        | _ -> Lwt.return_unit
      with
      | Lwt_stream.Empty -> exit 0
      | exn ->
        log "Event handler failed: %s" (Printexc.to_string exn);
        Lwt.return_unit
    in
    check_loop gitstore
  in
  let handler event =
    let%lwt () =
      match event with
      | `Pr pr ->
        let%lwt _ =
          Github_comment.push_status
            ~name:conf.Conf.name ~token:conf.Conf.token pr
            ~text:"In progress" `Pending
        in
        Lwt.return_unit
      | _ ->
        Lwt.return_unit
    in
    Lwt.return (event_push (Some event))
  in
  Lwt_main.run (Lwt.join [
      (match%lwt RepoGit.get conf.Conf.repo with
       | Ok r -> check_loop r
       | Error e -> Lwt.fail (Failure "Repository loading failed"));
      Webhook_handler.server
        ~port:conf.Conf.port
        ~secret:conf.Conf.secret
        ~handler
        conf.Conf.base_branch
        conf.Conf.dest_branch;
    ])
