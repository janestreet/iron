open! Core.Std
open! Async.Std
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
  match Feature_share.extract_feature_from_workspace_share_path repo_root with
  | None -> return ()
  | Some current_feature ->
    Cmd_workspace_unclean.compute_and_update_server_exn ~for_:User_name.unix_login
      (`Features [ current_feature ])
;;

let main ~repo_root ~hook_name =
  match (hook_name : Hook_name.t) with
  | Post_commit | Post_push ->
    if Client_config.(get () |> Workspaces.unclean_workspaces_detection_is_enabled)
    then compute_unclean_workspace_and_update_server ~repo_root
    else return ()
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
       Core.Std.Unix.mkdir_p (Abspath.to_string hookdir);
       let basedir = Core.Std.Unix.mkdtemp (Abspath.to_string hookdir ^/ "run") in
       let main ~basedir:_ =
         match%bind
           Monitor.try_with_or_error ~extract_exn:true (fun () ->
             main ~repo_root ~hook_name)
         with
         | Ok () -> Abspath.rm_rf_exn (Abspath.of_string basedir)
         | Error err ->
           Log.Global.error "%s" (Error.to_string_hum err);
           shutdown 1;
           never ()
       in
       App_harness.start ~init_stds:false ~log_format:`Text
         ~main ~basedir ~mode:`Prod ~fg ()
    )
;;
