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

let _ =
  let pr = Fork_handler.delinearize_pr Sys.argv 1 in
  Lwt_main.run begin
    let%lwt gitstore = match%lwt RepoGit.get repo with
      | Ok r -> Lwt.return r
      | Error e -> Lwt.fail (Failure "Repository loading failed")
    in
    let%lwt status,body = PrChecks.run_nogit ~conf pr gitstore in
    Github.(
      Monad.(run @@ begin
          let state,text = Github_comment.make_status status in
          GH.comment conf pr body >>= fun _ ->
          GH.status conf pr state (Some text)
        end)
    )
  end
