open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"clear some bookmarks without feature in the server"
    ~readme:(fun () -> "
Provide the PATH to the repo on the HG machine.  Typically:
  ssh://hg//hg/${REPO}/submissions

This requires admin privileges.
")
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and remote_repo_path =
       anon ("PATH" %: (Arg_type.create Remote_repo_path.of_string))
     in
     fun () ->
       let open! Deferred.Let_syntax in
       Clear_bookmarks_without_feature.rpc_to_server_exn { remote_repo_path }
    )
;;
