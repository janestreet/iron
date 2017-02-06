open! Core
open! Async
open! Import

let copy_feature ~from_ ~to_ ~without_copying_review =
  let%bind repo_root = Cmd_workspace.repo_for_hg_operations_exn from_ ~use:`Clone in
  let%bind repo_is_clean = Hg.status_cleanliness repo_root in
  let repo_is_clean = ok_exn repo_is_clean in
  let%bind () = ensure_can_access_remote_repo ~for_root_of:from_ in
  let%bind rev_zero = Hg.create_rev_zero repo_root in
  let%bind { feature_id; remote_repo_path; tip } =
    Copy_feature.rpc_to_server_exn { from_; to_; rev_zero; without_copying_review }
  in
  let%bind () =
    Hg.create_bookmark_and_update_to_it ~repo_is_clean repo_root remote_repo_path to_ tip
  in
  Cmd_workspace.If_enabled.create_workspace { feature_id; feature_path = to_ }
;;

let command =
  Command.async' ~summary:"copy a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and from_ = feature_path
     and to_ = absolute_feature_path
     and without_copying_review =
       no_arg_flag Switch.without_copying_review
         ~doc:"proceed even though completed review will not be copied"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let from_ = ok_exn from_ in
       let to_  = ok_exn to_    in
       copy_feature ~from_ ~to_ ~without_copying_review
    )
;;
