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

  module GitStore = Git_unix.FS
  module GitSync = Git_unix.Sync.Make(GitStore)
  module M = OpamStd.String.Map

  let github_repo repo =
    Git.Gri.of_string
      (Printf.sprintf "https://%sgithub.com/%s/%s.git"
         (match repo.auth with
          | None -> ""
          | Some (user, token) -> Printf.sprintf "%s:%s@" user token)
         repo.user repo.name)

  let local_mirror repo =
    Printf.sprintf "./%s%%%s.git" repo.user repo.name

  let get_commit t sha =
    match%lwt GitStore.read t sha with
    | Some (Git.Value.Commit c) -> Lwt.return c
    | Some x ->
      let kind = Git.Object_type.to_string (Git.Value.type_of x) in
      Lwt.fail (Failure (Printf.sprintf "Not a commit (%s: %s)"
                           (Git.Hash.to_hex sha) kind))
    | None ->
      Lwt.fail (Failure (Printf.sprintf "Commit %s not found"
                           (Git.Hash.to_hex sha)))

  let get_tree t sha =
    match%lwt GitStore.read_exn t sha with
    | Git.Value.Tree t -> Lwt.return t
    | x ->
      let kind = Git.Object_type.to_string (Git.Value.type_of x) in
      Lwt.fail (Failure (Printf.sprintf "Not a tree (%s: %s)"
                           (Git.Hash.to_hex sha) kind))

  let tree_of_commit_sha t sha =
    let%lwt c = get_commit t sha in
    get_tree t (Git.Hash.of_tree c.Git.Commit.tree)

  let get repo =
    GitStore.create ~root:(local_mirror repo) ()

  let get_file t sha path =
    let rec follow_path path sha = match path with
      | [] -> (match%lwt GitStore.read t sha with
          | Some (Git.Value.Blob b) -> Lwt.return (Some (Git.Blob.to_raw b))
          | _ -> Lwt.return None)
      | dir :: path -> (match%lwt GitStore.read t sha with
          | Some (Git.Value.Tree tr) ->
            let en_opt =
              OpamStd.Option.of_Not_found
                (List.find (fun en -> en.Git.Tree.name = dir)) tr
            in
            (match en_opt with
             | Some en -> follow_path path en.Git.Tree.node
             | None -> Lwt.return None)
          | _ -> Lwt.return None)
    in
    let%lwt c = get_commit t sha in
    follow_path (OpamStd.String.split path '/')
      (Git.Hash.of_tree c.Git.Commit.tree)

  let get_file_exn t sha path =
    match%lwt get_file t sha path with
    | Some f -> Lwt.return f
    | None -> Lwt.fail Not_found

  let branch_reference name =
    Git.Reference.of_raw ("refs/heads/" ^ name)

  let get_branch t name =
    GitStore.read_reference_exn t (branch_reference name)

  let set_branch t name commit_hash =
    GitStore.write_reference t (branch_reference name) commit_hash

  let fetch t repo =
    GitSync.fetch t ~unpack:true (github_repo repo)

  let fetch_pr pull_request t =
    let%lwt _ = fetch t pull_request.base.repo in
    log "fetched upstream";
    let%lwt _head_fetch = fetch t pull_request.head.repo in
    log "fetched user repo";
    Lwt.return_unit

  let push t branch repo =
    match%lwt
      GitSync.push t ~branch:(branch_reference branch) (github_repo repo)
    with
    | { Git.Sync.Result.result = `Ok; _ } -> Lwt.return_unit
    | { Git.Sync.Result.result = `Error s; _ } ->
      log "ERROR: could not push to %s: %s" branch s;
      Lwt.return_unit

  let common_ancestor pull_request t =
    log "Looking up common ancestor...";
    let seen = Hashtbl.create 137 in
    let module S = Git.Hash.Set in
    let (++), (--), (%%) = S.union, S.diff, S.inter in
    let all_parents s =
      Lwt_list.fold_left_s (fun acc sha ->
          try Lwt.return (acc ++ Hashtbl.find seen sha) with Not_found ->
            let%lwt c = get_commit t sha in
            let p = S.of_list Git.(List.map Hash.of_commit c.Commit.parents) in
            Hashtbl.add seen sha p;
            Lwt.return (acc ++ p))
        S.empty (S.elements s)
    in
    let seen_ancestors s =
      let rec aux acc s =
        if S.is_empty s then acc else
        let parents =
          S.fold (fun sha acc ->
              try acc ++ Hashtbl.find seen sha with Not_found -> acc)
            s S.empty
        in
        aux (acc ++ parents) (parents -- acc)
      in
      aux s s
    in
    let rec all_common descendents current descendents' current' =
      let current = current -- descendents' in
      if S.is_empty current then
        let common = descendents %% descendents' in
        if S.subset current' (seen_ancestors common) then Lwt.return common
        else all_common descendents' current' descendents current
      else
      let%lwt current = all_parents current in
      all_common descendents' current'
        (descendents ++ current) (current -- descendents)
    in
    let base = S.singleton (Git_unix.Hash_IO.of_hex pull_request.base.sha) in
    let head = S.singleton (Git_unix.Hash_IO.of_hex pull_request.head.sha) in
    let%lwt common = all_common base base head head in
    let rec remove_older ancestors = fun s ->
      if S.is_empty s || S.is_empty ancestors ||
         S.subset s (S.singleton (S.choose s))
      then Lwt.return s
      else
        let%lwt ancestors = all_parents ancestors in
        remove_older (ancestors -- s) (s -- ancestors)
    in
    let%lwt found = remove_older common common in
    log "found: %s"
      (OpamStd.List.concat_map " " Git.Hash.to_hex (S.elements found));
    Lwt.return (S.choose found)

  let changed_files base head t =
    let%lwt base_tree = tree_of_commit_sha t base in
    let%lwt head_tree = tree_of_commit_sha t head in
    let tree_to_map path t =
      List.fold_left (fun m e ->
          M.add (path ^ "/" ^ e.Git.Tree.name) e.Git.Tree.node m)
        M.empty t
    in
    let rec changed_new acc path left_tree right_tree =
      let left_map = tree_to_map path left_tree in
      let rec read acc = function
        | [] -> Lwt.return acc
        | { Git.Tree.name; node; _ } :: right_tree ->
          let path = path ^ "/" ^ name in
          let%lwt right = GitStore.read_exn t node in
          let%lwt left_opt =
            try GitStore.read t (M.find path left_map)
            with Not_found -> Lwt.return None
          in
          if left_opt = Some right then read acc right_tree
          else match right with
            | Git.Value.Blob b ->
              read (M.add path (Git.Blob.to_raw b) acc) right_tree
            | Git.Value.Tree right_subtree ->
              let left_subtree =
                match left_opt with
                | Some (Git.Value.Tree t) -> t
                | _ -> []
              in
              let%lwt acc = changed_new acc path left_subtree right_subtree in
              read acc right_tree
            | Git.Value.Tag _ | Git.Value.Commit _ ->
              Lwt.fail (Failure "Corrupted git state")
      in
      read acc right_tree
    in
    let%lwt diff = changed_new M.empty "" base_tree head_tree in
    Lwt.return (M.bindings diff)

  let opam_files t sha =
    let rec find_opams acc path sha =
      match%lwt GitStore.read t sha with
      | None | Some (Git.Value.Commit _) | Some (Git.Value.Tag _) ->
        Lwt.return acc
      | Some (Git.Value.Tree dir) ->
        Lwt_list.fold_left_s
          (fun acc entry ->
             find_opams acc (entry.Git.Tree.name :: path) entry.Git.Tree.node)
          acc dir
      | Some (Git.Value.Blob b) ->
        match path with
        | "opam"::_ ->
          let filename =
            OpamFile.make
              (OpamFilename.of_string (String.concat "/" (List.rev path)))
          in
          let opam =
            try Some (OpamFile.OPAM.read_from_string ~filename
                        (Git.Blob.to_raw b))
            with _ -> None
          in
          (match opam with
           | Some o -> Lwt.return (o::acc)
           | None -> Lwt.return acc)
        | _ -> Lwt.return acc
    in
    let%lwt root = tree_of_commit_sha t sha in
    List.find (fun tr -> tr.Git.Tree.name = "packages") root |> fun en ->
    find_opams [] ["packages"] en.Git.Tree.node

end

module FormatUpgrade = struct

  let onto_branch = "2.0.0"

  let git_identity () = {
    Git.User.
    name = "Camelus";
    email = "opam-commits@lists.ocaml.org";
    date = Int64.of_float (Unix.time ()), None;
  }

  let get_updated_opam commit gitstore nv =
    let opam_dir =
      Printf.sprintf "/packages/%s/%s/"
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
    let opam_str =
      OpamFile.OPAM.to_string_with_preserved_format
        ~format_from_string:opam_str
        (OpamFile.make (OpamFilename.of_string opam_file))
        opam
    in
    Lwt.return opam_str

  let get_compiler_opam commit gitstore comp_name =
    let bname =
      Printf.sprintf "/compilers/%s/%s/%s"
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
    let opam = OpamFile.Comp.to_package comp descr in
    let opam_str = OpamFile.OPAM.write_to_string opam in
    Lwt.return (OpamFile.OPAM.package opam, opam_str)

  let get_updated_subtree commit gitstore changed_files =
    let compilers, packages, files =
      List.fold_left (fun (compilers, packages, files) (f, contents) ->
          try Scanf.sscanf f "/compilers/%_s@/%s@/"
                (fun s -> OpamStd.String.Set.add s compilers, packages, files)
          with Scanf.Scan_failure _ -> try
              Scanf.sscanf f "/packages/%_s@/%s@/%s@/"
                (fun s -> function
                   | "opam" | "url" | "descr" ->
                     compilers,
                     OpamPackage.Set.add (OpamPackage.of_string s) packages,
                     files
                   | "files" ->
                     compilers, packages, OpamStd.String.Map.add f contents files
                   | _ -> compilers, packages, files)
            with Scanf.Scan_failure _ -> compilers, packages, files)
        (OpamStd.String.Set.empty,
         OpamPackage.Set.empty,
         OpamStd.String.Map.empty)
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
          let%lwt opam = get_updated_opam commit gitstore nv in
          Lwt.return (OpamPackage.Map.add nv opam acc))
        compiler_packages
        (OpamPackage.Set.elements packages)
    in
    Lwt.return @@
    OpamPackage.Map.fold (fun nv opam acc ->
        let f =
          Printf.sprintf "/packages/%s/%s/opam"
            (OpamPackage.name_to_string nv)
            (OpamPackage.to_string nv)
        in
        OpamStd.String.Map.add f opam acc)
      upgraded_packages
      files

  let rec add_file_to_tree gitstore tree path contents =
    match path with
    | [] -> Lwt.fail (Failure "Empty path")
    | [file] ->
      let%lwt hash =
        RepoGit.GitStore.write gitstore
          (Git.Value.blob (Git.Blob.of_raw contents))
      in
      let entry = { Git.Tree.perm = `Normal; name = file; node = hash } in
      Lwt.return (entry :: List.filter (fun e -> e.Git.Tree.name <> file) tree)
    | dir::path ->
      let subtree =
        try
          Some (List.find
                  (fun e -> e.Git.Tree.name = dir && e.Git.Tree.perm = `Dir)
                  tree).Git.Tree.node
        with Not_found -> None
      in
      let%lwt subtree =
        match subtree with
        | Some h -> RepoGit.get_tree gitstore h
        | None -> Lwt.return []
      in
      let%lwt subtree = add_file_to_tree gitstore subtree path contents in
      let%lwt hash = RepoGit.GitStore.write gitstore (Git.Value.tree subtree) in
      let entry = { Git.Tree.perm = `Dir; name = dir; node = hash } in
      Lwt.return (entry :: List.filter (fun e -> e.Git.Tree.name <> dir) tree)

  let gen_upgrade_commit ancestor head onto gitstore author message =
    let%lwt files = RepoGit.changed_files ancestor head gitstore in
    let%lwt replace_files = get_updated_subtree head gitstore files in
    let%lwt onto_tree = RepoGit.tree_of_commit_sha gitstore onto in
    let%lwt new_tree =
      Lwt_list.fold_left_s (fun tree (path,contents) ->
          let path = OpamStd.String.split path '/' in
          add_file_to_tree gitstore tree path contents)
        onto_tree (OpamStd.String.Map.bindings replace_files)
    in
    let%lwt hash = RepoGit.GitStore.write gitstore (Git.Value.tree new_tree) in
    let commit =
      { Git.Commit.
        tree = Git.Hash.to_tree hash;
        parents = [Git.Hash.to_commit onto];
        author = author;
        committer = git_identity ();
        message = message;
      }
    in
    RepoGit.GitStore.write gitstore (Git.Value.commit commit)

  let run ancestor_s head_s gitstore repo =
    let ancestor = Git_unix.Hash_IO.of_hex ancestor_s in
    let head = Git_unix.Hash_IO.of_hex head_s in
    let%lwt fetch = RepoGit.fetch gitstore repo in
    let remote_onto =
      Git.Reference.Map.find (RepoGit.branch_reference onto_branch)
        (Git.Sync.Result.references fetch)
    in
    let%lwt () = RepoGit.set_branch gitstore onto_branch remote_onto in
    log "Fetched new commits";
    try%lwt
      let%lwt onto_head = RepoGit.get_branch gitstore onto_branch in
      let%lwt head_commit = RepoGit.get_commit gitstore head in
      let author = head_commit.Git.Commit.author in
      let message =
        Printf.sprintf
          "%s\n\n_translated to 2.0 format by Camelus_\n"
          head_commit.Git.Commit.message
      in
      log "Rewriting commit %s (and possible parents) by %s"
        head_s head_commit.Git.Commit.author.Git.User.name;
      let%lwt commit_hash =
        gen_upgrade_commit ancestor head onto_head gitstore author message
      in
      let%lwt () =
        RepoGit.set_branch gitstore onto_branch commit_hash
      in
      log "Pushing new commit %s onto %s"
        (Git.Hash.to_hex commit_hash) onto_branch;
      let%lwt () = RepoGit.push gitstore onto_branch repo in
      log "Upgrade done";
      Lwt.return_unit
    with e ->
      log "Upgrade and push to branch %s failed: %s\n%s" onto_branch
        (Printexc.to_string e)
        (Printexc.get_backtrace ());
      Lwt.return_unit


(*
    >>= fun commit_hash ->
    

    
    let compilers, packages =
      List.fold_left (fun (compilers, packages) (f, _) ->
          try Scanf.sscanf f "/compilers/%_s@/%s@/"
                (fun s -> OpamStd.String.Set.add s compilers, packages)
          with Scanf.Scan_failure _ ->
            Scanf.sscanf f "/packages/%_s@/%s@/"
              (fun s ->
                 compilers,
                 OpamPackage.Set.add (OpamPackage.of_string s) packages))
        (OpamStd.String.Set.empty, OpamPackage.Set.empty)
        files
    in
    let compiler_packages =
      Lwt_list.fold_left_s (fun acc comp_name ->
          get_compiler_opam head gitstore comp_name >|= fun (nv,opam) ->
          OpamPackage.Map.add nv opam acc)
        OpamPackage.Map.empty
        (OpamStd.String.Set.elements compilers)
    in
    let modified_packages =
      compiler_packages >|= fun comps ->
      OpamPackage.Set.union (OpamPackage.keys comps) packages
    in
    let ugraded_packages =
      Lwt_list.fold_left_s (fun acc nv ->
          get_updated_opam head gitstore nv >|= fun opam ->
          OpamPackage.Map.add nv opam acc)
        OpamPackage.Map.empty
        (OpamPackage.Set.elements packages)
    in
    let new_subtree =
      (* compiler packages + updated packages + modified package files/ *)
    let 

          RepoGit.get_file gitstore head comp_file >>= fun comp_str ->
          RepoGit.get_file gitstore head
            (Filename.chop_extension comp_file ^ ".descr") >>= fun descr_str ->
          match comp_str with
          | Some comp_str ->
            let comp =
              OpamFile.Comp.read_from_string
                ~filename:(OpamFile.make (OpamFilename.of_string comp_file))
                comp_str
            in
            let descr =
              OpamStd.Option.map OpamFile.Descr.read_from_string descr_str
            in
            let opam = OpamFile.Comp.to_package comp descr in
            Lwt.return
              (OpamPackage.Map.add (OpamFile.OPAM.package opam) opam acc)
          | None -> Lwt.return acc)
        OpamPackage.Map.empty
        (OpamStd.String.Set.elements packages)
    in
    
      
            
      
    let opam_files =
      List.filter (fun (s,_) ->
          OpamStd.String.starts_with ~prefix:"/packages/" s &&
          OpamStd.String.ends_with ~suffix:"/opam" s)
        files
    in
    
*)

end

module PrChecks = struct

  let pkg_to_string p = Printf.sprintf "`%s`" (OpamPackage.to_string p)

  let max_items_in_post = 50

  let lint ancestor head gitstore =
    let%lwt files = RepoGit.changed_files ancestor head gitstore in
    let opam_files =
      List.filter (fun (s,_) ->
          OpamStd.String.starts_with ~prefix:"/packages/" s &&
          OpamStd.String.ends_with ~suffix:"/opam" s)
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
        title (Git.Hash.to_hex head)
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
    log "opam files at %s: %d" (Git.Hash.to_hex sha) (List.length opams);
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
    let installable = OpamSolver.installable universe in
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

  let run pr gitstore =
    let%lwt () = RepoGit.fetch_pr pr gitstore in
    let head = Git_unix.Hash_IO.of_hex pr.head.sha in
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
      | _ -> raise Not_found
    let to_int = function
      | `Int i -> i
      | _ -> raise Not_found
  end

  let pull_request_of_json json =
    let open JS in
    match json -.- "action" |> to_string with
    | "opened" | "reopened" | "syncronize" ->
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
      let pr_user = pr -.- "user" -.- "login" |> to_string in
      let message =
        pr -.- "title" |> to_string,
        pr -.- "body" |> to_string
      in
      Some { number; base; head; pr_user; message }
    | a ->
      log "Ignoring %s PR action" a;
      None

  let push_event_of_json json =
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
    match ref with
    | "refs/heads/master" ->
      Some { push_head; push_ancestor; push_repo }
    | ref ->
      log "Ignoring push to %s" ref;
      None

  let server ~port ~secret ~handler =
    let callback (conn, _) req body =
      let%lwt body = Cohttp_lwt_body.to_string body in
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
          (match pull_request_of_json json with
           | Some pr ->
             let%lwt () = handler (`Pr pr) in
             Server.respond ~status:`OK ~body:Cohttp_lwt_body.empty ()
           | None ->
             Server.respond ~status:`OK ~body:Cohttp_lwt_body.empty ()
           | exception Not_found ->
             log "Error: could not get PR data from JSON:\n%s"
               (Yojson.Safe.to_string json);
             Server.respond_error ~body:"Invalid format" ())
        | Some "push" ->
          (match push_event_of_json json with
           | Some push ->
             let%lwt () = handler (`Push push) in
             Server.respond ~status:`OK ~body:Cohttp_lwt_body.empty ()
           | None ->
             Server.respond ~status:`OK ~body:Cohttp_lwt_body.empty ()
           | exception Not_found ->
             log "Error: could not get push event data from JSON:\n%s"
               (Yojson.Safe.to_string json);
             Server.respond_error ~body:"Invalid format" ())
        | Some a ->
          log "Ignored %s event" a;
          Server.respond ~status:`OK ~body:Cohttp_lwt_body.empty ()
        | None ->
          Server.respond_error ~body:"Invalid format" ()
    in
    log "Listening on port %d" port;
    Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
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
    }

    let empty = {
      port = 8122;
      name = "opam-ci";
      token = Github.Token.of_string "";
      secret = Cstruct.of_string "";
      repo = { user="ocaml"; name="opam-repository"; auth=None };
      roles = [ `Pr_checker ];
    }

    open OpamPp.Op

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
  let conf =
    let f = OpamFile.make (OpamFilename.of_string "opam-ci.conf") in
    try Conf.read f with _ ->
      prerr_endline "A file opam-ci.conf with fields `token' and `secret' (and \
                     optionally `name' and `port') is required.";
      exit 3
  in
  let auth = conf.Conf.name, Github.Token.to_string conf.Conf.token in
  let event_stream, event_push = Lwt_stream.create () in
  let rec check_loop gitstore =
    (* The checks are done sequentially *)
    let%lwt () =
      try%lwt
        match%lwt Lwt_stream.next event_stream with
        | `Pr pr ->
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
             in Lwt.return_unit)
        | `Push p ->
          log "=> Push received (head %s onto %s)"
            p.push_head p.push_ancestor;
          try%lwt
            FormatUpgrade.run
              p.push_ancestor p.push_head gitstore
              { p.push_repo with auth = Some auth }
          with exn ->
            log "Upgrade commit failed: %s" (Printexc.to_string exn);
            Lwt.return_unit
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
      RepoGit.get conf.Conf.repo >>= check_loop;
      Webhook_handler.server
        ~port:conf.Conf.port
        ~secret:conf.Conf.secret
        ~handler;
    ])
