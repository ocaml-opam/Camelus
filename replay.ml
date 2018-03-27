#thread;;

#require "camelus";;

#require "lwt.ppx";;

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

let replay num =
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

let () = Lwt_main.run (replay 11570)
