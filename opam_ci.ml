let log fmt = OpamConsole.msg (fmt ^^ "%!")

type pull_request = {
  number: int;
  head_repo: string;
  head_ref: string;
  head_sha: string;
  base_sha: string;
}

module RepoGit = struct

  open Lwt
  module GitStore = Git_unix.FS
  module GitSync = Git_unix.Sync.Make(GitStore)

  let upstream = "ocaml/opam-repository"

  let github_repo slug =
    Git.Gri.of_string (Printf.sprintf "git://github.com/%s" slug)

  let upstream_repo =
    github_repo upstream

  let local_mirror = "./opam-repository.git"

  let tree_of_sha t sha =
    log "Reading %s\n" (Git.SHA.to_hex sha);
    GitStore.read t sha >>= function
    | Some (Git.Value.Commit c) ->
      GitStore.read t (Git.SHA.of_tree c.Git.Commit.tree) >>= (function
          | Some (Git.Value.Tree t) -> return t
          | _ -> fail (Failure "invalid base tree"))
    | None ->
      fail (Failure "base is not a commit")
    | Some _ ->
      fail (Failure "base is not a commit")

  let changed_files pull_request =
    GitStore.create ~root:local_mirror () >>= fun t ->
    GitSync.fetch t upstream_repo >>= fun _ ->
    log "fetched upstream\n";
    tree_of_sha t (Git.SHA.of_hex pull_request.base_sha) >>= fun base_tree ->
    log "got base tree\n";
    GitSync.fetch t (github_repo pull_request.head_repo) >>= fun _ ->
    log "fetched user\n";
    tree_of_sha t (Git.SHA.of_hex pull_request.head_sha) >>= fun head_tree ->
    log "got head tree\n";
    let module M = OpamStd.String.Map in
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
           with Not_found -> return None)
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
               fail (Failure "Corrupted git state")))
        acc right_tree
    in
    changed_new (return M.empty) "" base_tree head_tree >>= fun diff ->
    return (M.bindings diff)

end

let check_pull_request pr =
  let open Lwt in
  log "PR received: #%d (%s from %s/%s over %s)\n"
    pr.number pr.head_sha pr.head_repo pr.head_ref pr.base_sha;
  RepoGit.changed_files pr >|= fun files ->
  let opam_files =
    List.filter (fun (s,_) ->
        OpamStd.String.starts_with ~prefix:"/packages/" s &&
        OpamStd.String.ends_with ~suffix:"/opam" s)
      files
  in
  let lint =
    List.map (fun (file,contents) ->
        let opam = OpamSystem.temp_file "opam" in
        OpamSystem.write opam contents;
        let r, opamopt =
          OpamFile.OPAM.validate_file (OpamFilename.of_string opam)
        in
        OpamSystem.remove_file opam;
        file, r, opamopt)
      opam_files
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
      "### :x: opam-lint errors\n\n"
    else if warnings <> [] then
      "### :exclamation: opam-lint warnings\n\n"
    else
      "### :white_check_mark: all lint checks passed\n\n"
  in
  let pkgname = function
    | f,_,Some o ->
      OpamPackage.(to_string
                     (create OpamFile.OPAM.(name o) OpamFile.OPAM.(version o)))
    | f,_,None ->
      OpamStd.Option.Op.(
        (OpamPackage.(of_dirname OpamFilename.(dirname (of_string f)))
         >>| OpamPackage.to_string)
        +! f)
  in
  let pass =
    OpamStd.List.concat_map ", "
      ~nil:""
      ~left:"These packages passed lint tests: %s\n\n"
       pkgname passed
  in
  let warns =
    OpamStd.List.concat_map "\n\n"
      (fun ((_, warns, _) as fe) ->
         Printf.sprintf "#### **%s** has some warnings:\n\n%s\n"
           (pkgname fe)
           (OpamStd.Format.itemize ~bullet:"* "
              (fun (num,_,msg) -> Printf.sprintf "**warning %d**: %s" num msg)
              warns))
      warnings
  in
  let errs =
    OpamStd.List.concat_map "\n\n"
      (fun ((_, we, _) as fe) ->
         Printf.sprintf "#### **%s** has errors:\n\n%s\n"
           (pkgname fe)
           (OpamStd.Format.itemize ~bullet:"* "
              (fun (num,kind,msg) ->
                 let kind = match kind with
                   | `Warning -> "warning"
                   | `Error -> "error"
                 in
                 Printf.sprintf "**%s %d:** %s" kind num msg)
              we))
      errors
  in
  title ^ errs ^ warns ^ pass

let github_api =
  Uri.of_string "https://api.github.com"

let push_report token pr report =
  let open Lwt in
  let uri =
    Uri.with_path github_api
      (Printf.sprintf "/repos/%s/issues/%d/comments"
         RepoGit.upstream pr.number)
  in
  let json = OpamJson.to_string (`O ["body",`String report]) in
  let body = Cohttp_lwt_body.of_string json in
  let headers =
    let t = Cohttp.Header.init () in
    let t = Cohttp.Header.add_authorization t (`Basic ("opam-ci",token)) in
    Cohttp.Header.prepend_user_agent t "Opam Continuous Integration Bot"
  in
  log "%s\n" report;
  Cohttp_lwt_unix.Client.post ~body ~headers uri >>= fun (resp,body) ->
  if Cohttp.Code.(is_success (code_of_status (Cohttp_lwt_unix.Client.Response.status resp)))
  then return (log "Posted back to PR #%d.\n" pr.number)
  else Cohttp_lwt_body.to_string body >|= log "Error posting back: %s"

module Webhook_handler = struct

  open Lwt
  open Cohttp
  open Cohttp_lwt_unix

  let port = 8122
  let uri_path = "/opam-ci"
  let exp_method = `POST
  let exp_ua_prefix = "GitHub-Hookshot/"
  let exp_event = "pull_request"
  let exp_actions = ["opened"; "reopened"; "synchronize"]

  let check_github req =
    (* todo: use a secret and check github's hmac payload signature *)
    let headers = Request.headers req in
    Uri.path (Request.uri req) = uri_path &&
    Request.meth req = exp_method &&
    OpamStd.Option.Op.(
      (Header.get headers "user-agent" >>|
       OpamStd.String.starts_with ~prefix:exp_ua_prefix) +! false) &&
    Header.get_media_type headers = Some "application/json" &&
    Header.get headers "x-github-event" = Some exp_event

  let server handl =
    let callback (conn, _) req body =
      let remote_ip = match conn with
        | Conduit_lwt_unix.TCP { Conduit_lwt_unix.ip } -> Ipaddr.to_string ip
        | _ -> "<>"
      in
      if not (check_github req) then
        (log "Ignored invalid request from %s:\n%s" remote_ip
           (OpamStd.Format.itemize (fun s -> s)
              (Uri.path (Request.uri req) ::
               Code.string_of_method (Request.meth req) ::
               OpamStd.Option.to_string (fun x -> x) (Header.get_media_type (Request.headers req)) ::
               OpamStd.List.filter_map (Header.get (Request.headers req))
                 ["user-agent"; "content-type"; "x-github-event"]));
         Server.respond_not_found ())
      else
      let (-.-) json key = match json with
        | `O dic -> List.assoc key dic
        | _ -> log "field %s not found\n" key; raise Not_found
      in
      let to_string = function
        | `String s -> s
        | _ -> raise Not_found
      in
      let to_int = function
        | `Float f -> int_of_float f
        | _ -> log "bad float\n"; raise Not_found
      in
      Cohttp_lwt_body.to_string body >>= fun body ->
      let act_pr =
        try
          let json = OpamJson.of_string body in
          let action = json -.- "action" |> to_string in
          let number = json -.- "number" |> to_int in
          let pr = json -.- "pull_request" in
          let head = pr -.- "head" in
          let head_ref = head -.- "ref" |> to_string in
          let head_sha = head -.- "sha" |> to_string in
          let head_repo = head -.- "repo" -.- "full_name" |> to_string in
          let base_sha = pr -.- "base" -.- "sha" |> to_string in
          Some (action, { number; head_ref; head_sha; head_repo; base_sha })
        with
        | Not_found ->
          log "Error: invalid json structure from %s:\n%s"
            remote_ip
            (OpamJson.to_string (OpamJson.of_string body));
          None
        | Failure _ ->
          log "Error: invalid json from %s:\n%s"
            remote_ip body;
          None
      in
      match act_pr with
      | Some (act,pr) ->
        (if List.mem act exp_actions then handl pr else return_unit) >>= fun () ->
        Server.respond ~status:`OK ~body:Cohttp_lwt_body.empty ()
      | None ->
        Server.respond_error ~body:"Bad request" ()
    in
    log "Listening on port %d\n" port;
    Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
end

let () =
  let token = Sys.argv.(1) in
  let open Lwt in
  let pr_stream, pr_push = Lwt_stream.create () in
  let rec check_loop () =
    (* The checks are done sequentially *)
    Lwt.try_bind
      (fun () ->
         Lwt_stream.next pr_stream >>= fun pr ->
         check_pull_request pr >>= fun report ->
         push_report token pr report)
      (fun () -> return_unit)
      (fun exn -> log "Check failed: %s\n" (Printexc.to_string exn); return_unit)
    >>= check_loop
  in
  Lwt_main.run (join [
      check_loop ();
      Webhook_handler.server (fun pr -> return (pr_push (Some pr)));
    ])
