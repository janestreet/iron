open Core
open Async
open Import

let command =
  Command.async'
    ~summary:"grep and display conflicts in the current directory and below"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       let repo_root = ok_exn Repo_root.program_started_in in
       let%map grep_output =
         Hg.grep_conflicts_exn repo_root
           ~below:(Repo_root.relativize_exn repo_root Abspath.program_started_in)
           ~grep_display_option:"-nH"
       in
       Writer.write (force stdout) grep_output)
;;
