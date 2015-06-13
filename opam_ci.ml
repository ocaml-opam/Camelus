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

  let upstream = "AltGr/opam-repository"

  let github_repo slug =
    Git.Gri.of_string (Printf.sprintf "git://github.com/%s" slug)

  let upstream_repo =
    github_repo upstream

  let local_mirror = "./opam-repository.git"

  let tree_of_sha t sha =
    log "Reading %s\n" (Git.SHA.to_hex sha);
    GitStore.read t sha >>= function
    | Some (Git.Value.Commit c) ->
      log "Found commit «%s»\n"
        (List.hd (OpamStd.String.split c.Git.Commit.message '\n'));
      GitStore.read t (Git.SHA.of_tree c.Git.Commit.tree) >>= (function
          | Some (Git.Value.Tree t) -> return t
          | _ ->
            log "Tree fetch failed\n";
            fail (Failure "invalid base tree"))
    | None ->
      log "Commit not found\n";
      fail (Failure "base is not a commit")
    | Some _ ->
      log "Unexpected commit contents\n";
      fail (Failure "base is not a commit")

  let changed_files pull_request =
    GitStore.create ~root:local_mirror () >>= fun t ->
    GitSync.fetch t upstream_repo >>= fun _ ->
    log "fetched upstream\n";
    tree_of_sha t (Git.SHA.of_hex pull_request.base_sha) >>= fun base_tree ->
    log "got base tree\n";
    GitSync.fetch t (github_repo pull_request.head_repo) >>= fun head_fetch ->
    log "fetched user: %s\n" (Git.Sync.Result.pretty_fetch head_fetch);
    (match
       Git.Reference.head_contents head_fetch.Git.Sync.Result.references
         (Git.SHA.Commit.of_hex pull_request.head_sha)
     with
     | Git.Reference.SHA c -> return c
     | Git.Reference.Ref r ->
       GitStore.read_reference_exn t r)
    >>= fun head_commit ->
    log "Found head commit: %s\n" (Git.SHA.Commit.to_hex head_commit);
    tree_of_sha t (Git.SHA.of_commit head_commit) >>= fun head_tree ->
    log "got head tree\n";
    let module M = OpamStd.String.Map in
    let tree_to_map path t =
      List.fold_left (fun m e ->
          M.add (path ^ "/" ^ e.Git.Tree.name) e.Git.Tree.node m)
        M.empty t
    in
    let rec changed_new acc parentpath left_tree right_tree =
      log "Diff at %s/\n" parentpath;
      let left_map = tree_to_map parentpath left_tree in
      let rec diff path right_tree acc = match right_tree with
        | [] -> return acc
        | { Git.Tree.name; node; _ } :: rest ->
          let path = parentpath ^ "/" ^ name in
          GitStore.read_exn t node >>= fun right ->
          (try GitStore.read t (M.find path left_map)
           with Not_found -> return None)
          >>= fun left_opt ->
          (if left_opt = Some right then return acc
           else match right with
             | Git.Value.Blob b ->
               log "%s: file changed\n" path;
               return (M.add path (Git.Blob.to_raw b) acc)
             | Git.Value.Tree right_subtree ->
               log "%s: subtree changed\n" path;
               let left_subtree =
                 match left_opt with
                 | Some (Git.Value.Tree t) -> t
                 | _ -> []
               in
               changed_new acc path left_subtree right_subtree
             | Git.Value.Tag _ | Git.Value.Commit _ ->
               fail (Failure "Corrupted git state"))
          >>= diff parentpath rest
      in
      diff parentpath right_tree acc
    in
    changed_new M.empty "" base_tree head_tree >>= fun diff ->
    return (M.keys diff)

end

let check_pull_request pr =
  let open Lwt in
  log "PR received: #%d (%s from %s/%s over %s)\n"
    pr.number pr.head_sha pr.head_repo pr.head_ref pr.base_sha;
  RepoGit.changed_files pr >|= fun files ->
  log "Checking pr #%d: base %s, head %s#%s (%s):\n  Changed files = %s\n"
    pr.number pr.base_sha pr.head_repo pr.head_ref pr.head_sha
    (String.concat ", " files)

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
  let open Lwt in
  let pr_stream, pr_push = Lwt_stream.create () in
  let rec check_loop () =
    (* The checks are done sequentially *)
    Lwt.try_bind
      (fun () -> Lwt_stream.next pr_stream >>= check_pull_request)
      (fun () -> return_unit)
      (fun exn -> log "Check failed: %s\n" (Printexc.to_string exn); return_unit)
    >>= check_loop
  in
  Lwt_main.run (join [
      check_loop ();
      Webhook_handler.server (fun pr -> return (pr_push (Some pr)));
    ])
