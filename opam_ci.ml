(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix

let log fmt = OpamConsole.msg (fmt ^^ "\n%!")

type repo = {
  user: string;
  name: string;
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
}

module RepoGit = struct

  module GitStore = Git_unix.FS
  module GitSync = Git_unix.Sync.Make(GitStore)
  module M = OpamStd.String.Map

  let github_repo repo =
    Git.Gri.of_string
      (Printf.sprintf "git://github.com/%s/%s" repo.user repo.name)

  let local_mirror pr =
    Printf.sprintf "./%s%%%s.git" pr.base.repo.user pr.base.repo.name

  let get_commit t sha =
    GitStore.read_exn t sha >>= function
    | Git.Value.Commit c -> Lwt.return c
    | x ->
      let kind = Git.Object_type.to_string (Git.Value.type_of x) in
      Lwt.fail (Failure (Printf.sprintf "Not a commit (%s: %s)"
                           (Git.SHA.to_hex sha) kind))

  let get_tree t sha =
    GitStore.read_exn t sha >>= function
    | Git.Value.Tree t -> Lwt.return t
    | x ->
      let kind = Git.Object_type.to_string (Git.Value.type_of x) in
      Lwt.fail (Failure (Printf.sprintf "Not a tree (%s: %s)"
                           (Git.SHA.to_hex sha) kind))

  let tree_of_commit_sha t sha =
    get_commit t sha >>= fun c ->
    get_tree t (Git.SHA.of_tree c.Git.Commit.tree)

  let fetch pull_request =
    GitStore.create ~root:(local_mirror pull_request) () >>= fun t ->
    (* Fetching upstream is actually unneeded (the remote repo should include
       the commits) -- that is, until we check that the PR is really from a
       parent of the origin's master or signed commit.
       {[
         GitSync.fetch t upstream_repo >>= fun _ ->
         log "fetched upstream\n";
       ]}
    *)
    GitSync.fetch t (github_repo pull_request.head.repo) >>= fun head_fetch ->
    log "fetched user repo";
    Lwt.return t

  let common_ancestor pull_request t =
    let module S = Git.SHA.Set in
    let all_parents shas =
      S.fold (fun sha acc ->
          get_commit t sha >>= fun c ->
          acc >|= fun s ->
          List.fold_left (fun s x -> S.add (Git.SHA.of_commit x) s)
            s c.Git.Commit.parents)
        shas (Lwt.return S.empty)
    in
    let rec up parents_base parents_head bases heads =
      if S.is_empty bases && S.is_empty heads then
        Lwt.fail (Failure "No common ancestor found")
      else
      let common =
        S.union (S.inter parents_base heads) (S.inter parents_head bases)
      in
      if not (S.is_empty common) then Lwt.return (S.choose common) else
        all_parents bases >>= fun bases ->
        all_parents heads >>= fun heads ->
        up (S.union parents_base bases) (S.union parents_head heads)
          bases heads
    in
    let bases = S.singleton (Git.SHA.of_hex pull_request.base.sha) in
    let heads = S.singleton (Git.SHA.of_hex pull_request.head.sha) in
    up bases heads bases heads

  let changed_files base head t =
    tree_of_commit_sha t base >>= fun base_tree ->
    tree_of_commit_sha t head >>= fun head_tree ->
    let tree_to_map path t =
      List.fold_left (fun m e ->
          M.add (path ^ "/" ^ e.Git.Tree.name) e.Git.Tree.node m)
        M.empty t
    in
    let rec changed_new acc path left_tree right_tree =
      let left_map = tree_to_map path left_tree in
      List.fold_left (fun acc { Git.Tree.name; node; _ } ->
          let path = path ^ "/" ^ name in
          GitStore.read_exn t node >>= fun right ->
          (try GitStore.read t (M.find path left_map)
           with Not_found -> Lwt.return None)
          >>= fun left_opt ->
          (if left_opt = Some right then acc
           else match right with
             | Git.Value.Blob b ->
               acc >|= M.add path (Git.Blob.to_raw b)
             | Git.Value.Tree right_subtree ->
               let left_subtree =
                 match left_opt with
                 | Some (Git.Value.Tree t) -> t
                 | _ -> []
               in
               changed_new acc path left_subtree right_subtree
             | Git.Value.Tag _ | Git.Value.Commit _ ->
               Lwt.fail (Failure "Corrupted git state")))
        acc right_tree
    in
    changed_new (Lwt.return M.empty) "" base_tree head_tree >>= fun diff ->
    Lwt.return (M.bindings diff)

  let opam_files t sha =
    let rec find_opams acc path sha =
      GitStore.read t sha >>= function
      | None | Some (Git.Value.Commit _) | (Some Git.Value.Tag _)-> acc
      | Some (Git.Value.Tree d) ->
        List.fold_left (fun acc entry ->
            find_opams acc (entry.Git.Tree.name :: path)
              entry.Git.Tree.node)
          acc d
      | Some (Git.Value.Blob b) ->
        match path with
        | "opam"::_ ->
          let filename =
            OpamFilename.of_string (String.concat "/" (List.rev path))
          in
          let opam =
            try Some (OpamFile.OPAM.read_from_string ~filename
                        (Git.Blob.to_raw b))
            with _ -> None
          in
          (match opam with
           | Some o -> acc >|= fun opams -> o::opams
           | None -> acc)
        | _ -> acc
    in
    tree_of_commit_sha t sha >>= fun root ->
    List.find (fun tr -> tr.Git.Tree.name = "packages") root |> fun en ->
    find_opams (Lwt.return []) ["packages"] en.Git.Tree.node

end

module PrChecks = struct

  let pkg_to_string p = Printf.sprintf "`%s`" (OpamPackage.to_string p)

  let lint ancestor head gitstore =
    RepoGit.changed_files ancestor head gitstore >|= fun files ->
    let opam_files =
      List.filter (fun (s,_) ->
          OpamStd.String.starts_with ~prefix:"/packages/" s &&
          OpamStd.String.ends_with ~suffix:"/opam" s)
        files
    in
    let lint =
      List.map (fun (file,contents) ->
          let r, opamopt =
            OpamFile.OPAM.validate_string (OpamFilename.of_string file) contents
          in
          file, r, opamopt)
        opam_files
    in
    let unwanted_warns = [21] in
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
        "##### :x: opam-lint errors"
      else if warnings <> [] then
        "##### :exclamation: opam-lint warnings"
      else if passed <> [] then
        "##### :white_check_mark: All lint checks passed"
      else
        "##### :white_check_mark: No new or changed opam files"
    in
    let title =
      Printf.sprintf "%s <small>%s</small>\n\n"
        title (Git.SHA.to_hex head)
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
      OpamStd.List.concat_map "\n\n"
        (fun ((_, warns, _) as fe) ->
           Printf.sprintf "* **%s** has some warnings:\n\n%s\n"
             (pkgname fe)
             (OpamStd.Format.itemize ~bullet:"  * "
                (fun (num,_,msg) -> Printf.sprintf "**warning %d**: %s" num msg)
                warns))
        warnings
    in
    let errs =
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
    status, String.concat "" [title; errs; warns; pass]

  let installable gitstore sha =
    RepoGit.opam_files gitstore sha >>= fun opams ->
    log "opam files at %s: %d" (Git.SHA.to_hex sha) (List.length opams);
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
    let universe = {
      u_packages = packages;
      u_action = Depends;
      u_installed = OpamPackage.Set.empty;
      u_available = packages; (* Todo: check for different constraints *)
      u_depends = OpamPackage.Map.map OpamFile.OPAM.depends m;
      u_depopts = OpamPackage.Map.map OpamFile.OPAM.depopts m;
      u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts m;
      u_installed_roots = OpamPackage.Set.empty;
      u_pinned = OpamPackage.Set.empty;
      u_base = OpamPackage.Set.empty;
      u_test = false;
      u_doc = false;
    } in
    Lwt.return (packages, OpamSolver.installable universe)

  let installability_check ancestor head gitstore =
    installable gitstore ancestor
    >>= fun (packages_before, installable_before) ->
    installable gitstore head
    >>= fun (packages_after, installable_after) ->
    let open OpamPackage.Set.Op in
    let fresh = packages_after -- packages_before in
    let broken_before = packages_before -- installable_before in
    log "Broken: %s" (OpamPackage.Set.to_string broken_before);
    let broken_after = packages_after -- installable_after in
    log "Broken (after): %s" (OpamPackage.Set.to_string broken_after);
    let breaks = broken_after -- broken_before in
    let repairs = broken_before -- broken_after in
    let no_breaks = OpamPackage.Set.is_empty breaks in
    let title =
      Printf.sprintf "\n\n##### :%s: Installability check (%d &rarr; %d)\n\n"
        (if no_breaks then "white_check_mark" else "exclamation")
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

  let run pr =
    RepoGit.fetch pr >>= fun gitstore ->
    let head = Git.SHA.of_hex pr.head.sha in
    RepoGit.common_ancestor pr gitstore >>= fun ancestor ->
    lint ancestor head gitstore >>= fun (stlint,msglint) ->
    installability_check ancestor head gitstore >>= fun (stinst,msginst) ->
    Lwt.return (add_status stlint stinst,
                msglint ^ "\n\n---\n" ^ msginst)

end

module Github_comment = struct

  let push_report ~name ~token ~report:(status,body) pr =
    let open Github.Monad in
    let open Github_t in
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
      | Some c -> (* not in ocaml-github API yet *)
        let body =
          Github_j.string_of_new_issue_comment { new_issue_comment_body = body }
        in
        let num = Int64.to_int c.issue_comment_id in
        let uri = Github.URI.issue_comment ~user ~repo ~num in
        Github.API.patch ~expected_code:`OK ~token ~body ~uri
          (fun b -> Lwt.return (Github_j.issue_comment_of_string b))
    in
    (* requires write access to the repository from the bot
       {[
         let push_status () =
           log "Pushing status...";
           let status =
             let new_status_state, new_status_description =
               match status with
               | `Passed ->
                 `Success, Some "All opam-lint tests passed"
               | `Warnings ps ->
                 `Success, Some ("Packages with warnings: "^String.concat " " ps)
               | `Errors ps ->
                 `Error, Some ("Packages with errors: "^String.concat " " ps)
             in
             { Github_t.
               new_status_state;
               new_status_target_url = None;
               new_status_description;
             }
           in
           Github.Status.create
             ~token ~user:pr.base.repo.user ~repo:pr.base.repo.name
             ~status ~sha:pr.head.sha ()
           >>= fun _ ->
         in
       ]}
    *)
    run (
      comment () >>= fun _ ->
      (* push_status () >>= fun _ -> *)
      return (log "Posted back to PR #%d." pr.number)
    )

end

module Webhook_handler = struct

  open Cohttp
  open Cohttp_lwt_unix

  let uri_path = "/opam-ci"
  let exp_method = `POST
  let exp_ua_prefix = "GitHub-Hookshot/"
  let exp_event = "pull_request"
  let exp_actions = ["opened"; "reopened"; "synchronize"]

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
      Cstruct.equal
        (Nocrypto.Hash.mac `SHA1 ~key:secret (Cstruct.of_string body))
    ) = Some true

  let server ~port ~secret ~handler =
    let callback (conn, _) req body =
      let (-.-) json key = match json with
        | `Assoc dic -> List.assoc key dic
        | _ -> log "field %s not found" key; raise Not_found
      in
      let to_string = function
        | `String s -> s
        | _ -> raise Not_found
      in
      let to_int = function
        | `Int i -> i
        | _ -> raise Not_found
      in
      Cohttp_lwt_body.to_string body >>= fun body ->
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
      if Header.get (Request.headers req) "x-github-event" <> Some exp_event
      then
        (log "Ignored non pull-request action";
         Server.respond ~status:`OK ~body:Cohttp_lwt_body.empty ())
      else
      let json =
        try Some (Yojson.Safe.from_string body) with Failure _ -> None
      in
      match json with
      | None ->
        log "Error: invalid json :\n%s" body;
        Server.respond_error ~body:"Invalid JSON" ()
      | Some json ->
        let action =
          try Some (json -.- "action" |> to_string)
          with Not_found -> None
        in
        match action with
        | Some a when List.mem a exp_actions ->
          let pr =
            try
              let number = json -.- "number" |> to_int in
              let pr = json -.- "pull_request" in
              let full_repo r = {
                repo = { user = r -.- "user" -.- "login" |> to_string;
                         name = r -.- "repo" -.- "name" |> to_string; };
                ref = r -.- "ref" |> to_string;
                sha = r -.- "sha" |> to_string;
              } in
              let base = full_repo (pr -.- "base") in
              let head = full_repo (pr -.- "head") in
              Some { number; base; head }
            with Not_found -> None
          in
          begin match pr with
            | Some pr ->
              handler pr >>= fun () ->
              Server.respond ~status:`OK ~body:Cohttp_lwt_body.empty ()
            | None ->
              log "Error: invalid Json structure:\n%s"
                (Yojson.Safe.to_string json);
              Server.respond_error ~body:"Invalid format" ()
          end
        | Some _ ->
          Server.respond ~status:`OK ~body:Cohttp_lwt_body.empty ()
        | None ->
          log "Error: could not get action from JSON:\n%s"
            (Yojson.Safe.to_string json);
          Server.respond_error ~body:"Invalid format" ()
    in
    log "Listening on port %d" port;
    Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
end

module Conf = struct
  module C = struct
    let internal = ".opam-ci"

    type t = { port: int; name: string; token: Github.Token.t; secret: Cstruct.t }

    let empty = {
      port = 8122;
      name = "opam-ci";
      token = Github.Token.of_string "";
      secret = Cstruct.of_string "";
    }

    open OpamFormat

    let of_syntax s =
      let f = s.OpamTypes.file_contents in
      {
        port = assoc_default 8122 f "port" parse_int;
        name = assoc_default "opam-ci" f "name" parse_string;
        token = Github.Token.of_string (assoc f "token" parse_string);
        secret = Cstruct.of_string (assoc f "secret" parse_string);
      }

    let of_channel filename ic =
      of_syntax (OpamFile.Syntax.of_channel filename ic)

    let of_string filename str =
      of_syntax (OpamFile.Syntax.of_string filename str)

    let to_string _ _ = assert false
  end
  include C
  include OpamFile.Make(C)
end

let () =
  let conf =
    try Conf.read (OpamFilename.of_string "opam-ci.conf") with _ ->
      prerr_endline "A file opam-ci.conf with fields `token' and `secret' (and \
                     optionally `name' and `port') is required.";
      exit 3
  in
  let pr_stream, pr_push = Lwt_stream.create () in
  let rec check_loop () =
    (* The checks are done sequentially *)
    Lwt.try_bind
      (fun () ->
         Lwt_stream.next pr_stream >>= fun pr ->
         log "=> PR #%d received \
              (onto %s/%s#%s from %s/%s#%s, commit %s over %s)"
           pr.number
           pr.base.repo.user pr.base.repo.name pr.base.ref
           pr.head.repo.user pr.head.repo.name pr.head.ref
           pr.head.sha pr.base.sha;
         PrChecks.run pr >>= fun report ->
         Github_comment.push_report
           ~name:conf.Conf.name
           ~token:conf.Conf.token
           ~report
           pr)
      (fun () -> Lwt.return_unit)
      (fun exn ->
         log "Check failed: %s" (Printexc.to_string exn);
         Lwt.return_unit)
    >>= check_loop
  in
  Lwt_main.run (Lwt.join [
      check_loop ();
      Webhook_handler.server
        ~port:conf.Conf.port
        ~secret:conf.Conf.secret
        ~handler:(fun pr -> Lwt.return (pr_push (Some pr)));
    ])
