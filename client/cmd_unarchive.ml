open! Core
open! Async
open! Import

let main  { Fe.Unarchive.Action. feature_path; feature_id } =
  let%bind repo_root =
    Cmd_workspace.repo_for_hg_operations_exn feature_path ~use:`Clone
  in
  let%bind repo_is_clean = Hg.status_cleanliness repo_root in
  let repo_is_clean = ok_exn repo_is_clean in
  let%bind rev_zero = Hg.create_rev_zero repo_root in
  let%bind () =
    if Feature_path.is_root feature_path
    then
      (* The root feature doesn't exist, so we can't look up its remote-repo-path.
         Just proceed. *)
      return ()
    else ensure_can_access_remote_repo ~for_root_of:feature_path
  in
  let%bind feature_id =
    match feature_id with
    | Some feature_id -> return feature_id
    | None ->
      let%map feature =
        Get_feature.Maybe_archived.rpc_to_server_exn
          { what_diff = None
          ; what_feature = { feature_spec = `Feature_path feature_path
                           ; namespace    = `Archived
                           }
          }
      in
      feature.feature_id
  in
  let%bind { feature_tip; remote_repo_path } =
    Unarchive_feature.rpc_to_server_exn { feature_path; rev_zero; feature_id }
  in
  let%bind () =
    Hg.pull repo_root ~repo_is_clean ~from:remote_repo_path (`Rev feature_tip)
  in
  let%bind () =
    Hg.set_bookmark repo_root (Feature feature_path) ~to_:(`Rev feature_tip)
      (`Push_to_and_overwrite remote_repo_path)
  in
  let%bind () =
    Hg.update repo_root (`Feature feature_path) ~clean_after_update:(Yes repo_is_clean)
  in
  Cmd_workspace.If_enabled.create_workspace { feature_id; feature_path }
;;

let command =
  Command.async'
    ~summary:"unarchive a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = archived_feature_path
     and feature_id =
       flag "-id" ~doc:"FEATURE_ID feature id"
         (optional (Arg_type.create Feature_id.of_string))
     in
     fun () ->
       let feature_path = ok_exn feature_path in
       main { feature_path; feature_id }
    )
;;
