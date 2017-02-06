open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"show a log of the changesets in a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and args = dash_dash_rest ~doc:"pass the remaining arguments to 'hg log'"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind repo_root =
         Cmd_workspace.repo_for_hg_operations_exn feature_path ~use:`Clone
       in
       let%bind rev_zero = Hg.create_rev_zero repo_root in
       let%bind { base; tip; _ } =
         Get_feature_revs.rpc_to_server_exn { feature_path; rev_zero = Some rev_zero }
       in
       Hg.whats_new repo_root ~from:base ~to_:tip ~args
    )
;;
