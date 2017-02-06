open! Core
open! Async
open! Import

module Hook_name = struct
  module T = struct
    type t =
      | Post_commit
      | Post_push
    [@@deriving enumerate]

    let of_string = function
      | "post-commit" -> Post_commit
      | "post-push"   -> Post_push
      | str           -> raise_s [%sexp "invalid hook name", (str : string)]
    ;;

    let to_string = function
      | Post_commit       -> "post-commit"
      | Post_push         -> "post-push"
    ;;
  end

  include T
  include Sexpable.Of_stringable (T)

  let relpath t = Relpath.of_string (".hg/iron-hooks/" ^ to_string t)
end

let compute_unclean_workspace_and_update_server ~repo_root =
  match Workspace.extract_feature_from_workspace_share_path repo_root with
  | None -> return ()
  | Some current_feature ->
    Cmd_workspace_unclean.compute_and_update_server_exn ~for_:User_name.unix_login
      (`Features [ current_feature ])
;;

let compute_unclean_workspace ~repo_root =
  if Client_config.(get () |> Workspaces.unclean_workspaces_detection_is_enabled)
  then compute_unclean_workspace_and_update_server ~repo_root
  else return ()
;;

let maybe_send_push_event ~repo_root =
  if not Client_config.(get () |> send_push_events_to_server)
  then Deferred.unit
  else (
    let tip = Hg.create_rev repo_root Revset.dot in
    let get_feature_id_from_server ~repo_root =
      let%bind feature_path = Hg.current_bookmark repo_root in
      match Feature_path.of_string (ok_exn (feature_path)) with
      | exception _  -> return `Do_not_send_push_event
      | feature_path ->
        match%map Feature_exists.rpc_to_server_exn feature_path with
        | No             -> `Do_not_send_push_event
        | Yes feature_id -> `Send_push_to feature_id
    in
    match%bind (
      match%bind Workspace_hgrc.extract_info repo_root with
      | Error _ -> get_feature_id_from_server ~repo_root
      | Ok info ->
        match info.kind with
        | `Feature feature  -> return (`Send_push_to feature.feature_id)
        | `Satellite_repo   -> return `Do_not_send_push_event
        | `Clone            -> return `Do_not_send_push_event
        | `Fake_for_testing ->
          if am_functional_testing
          then get_feature_id_from_server ~repo_root
          else return `Do_not_send_push_event)
    with
    | `Do_not_send_push_event -> return ()
    | `Send_push_to feature_id ->
      match%bind tip with
      | Error _ -> return ()
      | Ok tip  -> Push_events.Add.rpc_to_server_exn { feature_id; tip })
;;

let main ~repo_root ~hook_name =
  let maybe_send_push_event =
    match (hook_name : Hook_name.t) with
    | Post_commit -> Deferred.unit
    | Post_push -> maybe_send_push_event ~repo_root
  in
  Deferred.all_unit
    [ compute_unclean_workspace ~repo_root
    ; maybe_send_push_event
    ]
;;

let command =
  Command.basic'
    ~summary:"implement hg hooks for Iron"
    ~readme:(fun () ->
      concat [ "\
[fe tools hg-hooks HOOK-NAME] is invoked by hg hooks in Iron workspaces.

Hooks supported: "
             ; String.concat ~sep:", " (List.map Hook_name.all ~f:Hook_name.to_string)
             ; "\n
The command will fail synchronously if not run from within a repository.
Otherwise, it daemonizes and create a temporary log directory under:

  $(hg root)/.hg/iron-hooks/${HOOK-NAME}/${TMP_RUNDIR}

Upon completion of the background hook, if there is no error the temporary directory
is deleted.  In case of an error, the directory is kept untouched for inspection.
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and hook_name = enum_anon "HOOK-NAME" (module Hook_name)
     and fg = no_arg_flag "-fg" ~doc:" run in foreground, don't daemonize"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let repo_root = ok_exn Repo_root.program_started_in in
       let hookdir =
         Abspath.append (Repo_root.to_abspath repo_root) (Hook_name.relpath hook_name)
       in
       Core.Unix.mkdir_p (Abspath.to_string hookdir);
       let basedir = Core.Unix.mkdtemp (Abspath.to_string hookdir ^/ "run") in
       let main ~basedir:_ =
         match%bind
           Monitor.try_with_or_error ~extract_exn:true (fun () ->
             main ~repo_root ~hook_name)
         with
         | Ok () -> Abspath.rm_rf_exn (Abspath.of_string basedir)
         | Error err ->
           Log.Global.error "%s" (Error.to_string_hum err);
           Shutdown.exit 1
       in
       App_harness.start ~init_stds:false ~log_format:`Text
         ~main ~basedir ~mode:`Prod ~fg ()
    )
;;
