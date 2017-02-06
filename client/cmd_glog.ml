open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"show the revision graph of the tips and bases of some features"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and do_not_pull =
       flag "-do-not-pull" no_arg ~doc:" do not pull, fail if revs are unknown"
     and subtrees =
       feature_path_flagged_listed
         ~label:"subtree" ~doc:"FEATURE show this feature and its descendants"
     and features =
       anon_feature_paths
     and args =
       dash_dash_rest ~doc:" Give remaining arguments to hg glog"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let features = ok_exn features in
       let subtrees = ok_exn subtrees in
       let%bind repo_root =
         Cmd_workspace.repo_for_hg_operations_use_clone_exn features
       in
       let%bind rev_zero = Hg.create_rev_zero repo_root in
       let%bind { remote_repo_path; features } =
         List_feature_revisions.rpc_to_server_exn { rev_zero; features; subtrees }
       in
       let revs =
         List.concat_map features
           ~f:(fun { feature_path = _ ; base ; tip } -> [ base ; tip ])
       in
       let%bind () =
         if do_not_pull
         then return ()
         else Hg.pull ~even_if_unclean:true repo_root ~from:remote_repo_path (`Revs revs)
       in
       let%map log =
         Hg.log ~args:("-G" :: args)
           repo_root (Hg.Revset.or_ (List.map ~f:Hg.Revset.of_rev revs))
       in
       log
       |> ok_exn
       |> print_endline
    )
;;
