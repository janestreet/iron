open! Core.Std
open! Async.Std
open! Import

let internal_add =
  Command.async' ~summary:"add a fully-reviewed edge"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and from = flag "-from" (required rev_arg_type) ~doc:"REV"
     and to_ = flag "-to"   (required rev_arg_type) ~doc:"REV"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let repo_root = ok_exn Repo_root.program_started_in in
       let%bind rev_zero = Hg.create_rev_zero repo_root in
       let%bind from = Raw_rev.resolve_exn from ~in_:(Ok repo_root) in
       let%bind to_ = Raw_rev.resolve_exn to_  ~in_:(Ok repo_root) in
       Add_fully_reviewed_edge.rpc_to_server_exn
         { rev_zero; edge = `From_to (from, to_) }
    )
;;

let internal =
  Command.group ~summary:"deal with the set of fully-reviewed edges"
    [ "add", internal_add
    ]
;;

let tools_add =
  Command.async' ~summary:"add a fully-reviewed edge from a feature's base to tip"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path =
       feature_path_or_current_bookmark
     and even_if_release_is_locked =
       no_arg_flag Switch.even_if_release_is_locked
         ~doc:"succeed even if release is currently locked in the feature"
     and if_feature_is_empty =
       no_arg_flag Switch.do_nothing_if_feature_is_empty
         ~doc:" do not add an edge (instead of failing) when the feature is empty"
       |> map ~f:(fun do_nothing_if_feature_is_empty ->
         if do_nothing_if_feature_is_empty
         then `Do_nothing
         else `Fail)
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind repo_root =
         Cmd_workspace.repo_for_hg_operations_exn feature_path ~use:`Clone
       in
       let%bind rev_zero = Hg.create_rev_zero repo_root in
       Add_fully_reviewed_edge.rpc_to_server_exn
         { rev_zero
         ; edge     = `Feature_base_to_tip { feature_path
                                           ; even_if_release_is_locked
                                           ; if_feature_is_empty
                                           }
         }
    )
;;

let tools_check =
  Command.async' ~summary:"check whether a fully-reviewed edge exists"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and from = rev
     and to_ = rev
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let repo_root = Repo_root.program_started_in in
       let%bind from = Raw_rev.resolve_exn from ~in_:repo_root in
       let%bind to_ = Raw_rev.resolve_exn to_  ~in_:repo_root in
       Is_fully_reviewed_edge.rpc_to_server_exn { from; to_ }
    )
;;

let tools =
  Command.group ~summary:"deal with the set of fully-reviewed edges"
    [ "add"  , tools_add
    ; "check", tools_check
    ]
;;
