open Core
open Async
open Import

let satellites_only_switch = "-satellites-only"

let main (action : Fe.Update.Action.t) =
  match action with
  | Satellites_only ->
    if not (Cmd_workspace.workspaces_are_enabled ())
    then failwithf "[%s] can be used only if workspaces are enabled"
           satellites_only_switch ();
    (* We allow the command to be run even if there is no current bookmark *)
    let repo_root = ok_exn Repo_root.program_started_in in
    Cmd_workspace.If_enabled.update_satellite_repos ~center_repo_root:repo_root
  | Feature feature_path ->
    let%bind repo_root =
      Cmd_workspace.repo_for_hg_operations_exn feature_path ~use:`Share
    in
    let%bind rev_zero = Hg.create_rev_zero repo_root in
    let%bind { tip ; remote_repo_path; _ } =
      Get_feature_revs.rpc_to_server_exn
        { feature_path; rev_zero = Some rev_zero }
    in
    Cmd_review.pull_and_update
      ~repo_root
      ~remote_repo_path
      ~feature_path
      ~review_session_tip:tip
      ~feature_tip:tip;
;;

let command =
  Command.async'
    ~summary:"update the repo to the current active session or to tip"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and () = interactive
     and feature_path_option = feature_path_option
     and satellites_only =
       no_arg_flag satellites_only_switch
         ~doc:" update only satellites repos in the scaffold repo \
               (require workspaces enabled, and cwd = center repo)"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path_option = ok_exn feature_path_option in
       if satellites_only
       then (
         (if Option.is_some feature_path_option
          then failwithf "when [%s] is provided you can't pass a feature argument"
                 satellites_only_switch ());
         main Satellites_only)
       else (
         let%bind feature_path =
           match feature_path_option with
           | Some feature_path -> return feature_path
           | None ->
             let%map feature_path = Command.Param.current_bookmark () in
             ok_exn feature_path
         in
         main (Feature feature_path)))
;;
