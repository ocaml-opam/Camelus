open Camelus_lib

let conf = Conf.read (OpamFile.make (OpamFilename.of_string "opam-ci.conf"))

let name = conf.Conf.name
let token = conf.Conf.token

let repo = {
  user = "ocaml";
  name = "opam-repository";
  auth = Some (name, Github.Token.to_string token);
}

let base_branch = "master"
let dest_branch = "2.0.0"

let get_pr num =
  Github.Monad.run @@
  let open Github.Monad in
    Github.Pull.get ~user:repo.user ~repo:repo.name ~num () >|=
    Github.Response.value

open Lwt.Infix
open Github_t

let replay_upgrade num =
  let%lwt gitstore = match%lwt RepoGit.get repo with
    | Ok r -> Lwt.return r
    | Error e -> Lwt.fail (Failure "Repository loading failed")
  in
  let%lwt p = get_pr num in
  let merge_sha = match p.pull_merged_at, p.pull_merge_commit_sha with
    | Some _, Some h -> h
    | _ -> failwith "No merge SHA found"
  in
  let merge_parent_sha = merge_sha^"^" in
    log "Upgrading branch from %s to %s"
      merge_parent_sha merge_sha;
    let%lwt new_branch =
      FormatUpgrade.run base_branch dest_branch
        merge_parent_sha merge_sha gitstore
        repo
    in
      match new_branch with
        | None -> Lwt.return_unit
        | Some (branch, msg) ->
            let title, message =
              match OpamStd.String.cut_at msg '\n' with
                | Some (t, m) -> t, Some (String.trim m)
                | None -> "Merge changes from 1.2 format repo", None
            in
              Github_comment.pull_request
                ~name ~token repo
                branch dest_branch
                ?message title

let get_unchecked_pr () =
  let open Github.Monad in
  run @@
  let open_prs = Github.Pull.for_repo ~token ~state:`Open ~user:repo.user ~repo:repo.name () in
  let res_stream =
    Github.Stream.map (fun pr ->
        let stream = Github.Issue.comments ~token ~user:repo.user ~repo:repo.name ~num:pr.pull_number () in
        Github.Stream.find (fun { issue_comment_user = u; _ } -> u.user_login = conf.Conf.name) stream >>=
        function | Some _ -> return []
                 | None -> return [pr.pull_number]
      ) open_prs
  in
  Github.Stream.to_list res_stream

let replay_check nums =
  let%lwt gitstore = match%lwt RepoGit.get repo with
    | Ok r -> Lwt.return r
    | Error e -> Lwt.fail (Failure "Repository loading failed")
  in
  Lwt_list.iter_p (fun num ->
  let%lwt pr = get_pr num >|= fun p ->
    let get_repo b = {
      repo =
        (match b.branch_repo with
         | None -> repo
         | Some gr -> {
             user = gr.repository_owner.user_login;
             name = gr.repository_name;
             auth = None;
           });
      ref = b.branch_ref;
      sha = b.branch_sha;
    } in {
      number = num;
      base = get_repo p.pull_base;
      head = get_repo p.pull_head;
      pr_user = p.pull_user.user_login;
      message = p.pull_title, p.pull_body;
    }
  in
  let%lwt report = PrChecks.run pr gitstore in
  Github_comment.push_report ~name ~token ~report pr )
    nums

let () =
  match Sys.argv.(1) with
  | "upgrade" ->
    let num = int_of_string Sys.argv.(2) in
    Lwt_main.run (replay_upgrade num)
  | "check" ->
    let num = int_of_string Sys.argv.(2) in
    Lwt_main.run (replay_check [num])
  | "check-bunch" ->
    begin
      match Array.to_list Sys.argv with
      | [] | [_] -> assert false
      | _ :: _ :: prs ->
        let nums = List.rev_map int_of_string prs in
        Lwt_main.run (replay_check nums)
    end
  | "auto" -> Lwt_main.run begin get_unchecked_pr () >>= replay_check end
  | _ ->
    OpamConsole.msg "Usage: %s <upgrade|check> PR# or %s check-bunch PR#...\n" Sys.argv.(0) Sys.argv.(0);
    exit 2
