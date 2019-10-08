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
open Camelus_lib

let log fmt = OpamConsole.msg (fmt ^^ "\n%!")

let () = Lwt.async_exception_hook :=
    begin fun exn ->
      log "Event async failed: %s" (Printexc.to_string exn)
    end

let handler conf gitstore = function
  | `Pr pr when List.mem `Pr_checker conf.Conf.roles ->
    Lwt.with_value log_tag (Some (string_of_int pr.number))
      (fun () -> log "=> PR #%d received \
                      (onto %s/%s#%s from %s/%s#%s, commit %s over %s)"
          pr.number
          pr.base.repo.user pr.base.repo.name pr.base.ref
          pr.head.repo.user pr.head.repo.name pr.head.ref
          pr.head.sha pr.base.sha;
        try%lwt
          let%lwt report = PrChecks.run ~conf pr gitstore in
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
  | `Push p when List.mem `Push_upgrader conf.Conf.roles ->
    (log "=> Push received (head %s onto %s)"
       p.push_head p.push_ancestor;
     let auth = conf.Conf.name, Github.Token.to_string conf.Conf.token in
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
  let event_stream, event_push = Lwt_stream.create () in
  let rec check_loop gitstore =
    match%lwt Lwt_stream.next event_stream with
    | exception Lwt_stream.Empty -> exit 0
    | exception exn ->
      log "Event handler failed: %s" (Printexc.to_string exn);
      Lwt.return_unit
    | event ->
      (* The checks are done concurrently *)
      Lwt.async (fun () -> handler conf gitstore event);
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
        ~conf
        ~handler;
    ])
