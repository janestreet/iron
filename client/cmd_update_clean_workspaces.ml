open! Core.Std
open! Async.Std
open! Import

let command =

  Command.async'
    ~summary:"fe update all clean workspaces on the current machine"
    ~readme:(fun () ->
      concat ["\
A workspace is considered clean if it has no uncommitted changes, not outgoing changes,
and no error occurred while trying to get this very information.

This command walks all of one's clean workspaces, and update their hg share to the tip
of the feature's remote bookmark.  It also updates the satellites of scaffold repos when
applicable.

A user may run this command periodically in the background.  Typically it is called by
the script [/j/office/app/fe/prod/etc/update-workspaces] which it called hourly under the
standard setup.

When the switch " ; Switch.do_nothing_if_not_enabled ; " is supplied, the command will
look whether the following attribute is set to [true] in one's ferc file:

  (workspaces (
    (auto_update_clean_workspaces_is_enabled BOOL)
  ))

and will simply exit 0 if the property is set to false.

It is also possible to define a custom exception list of workspaces that should never
be updated during that process, by adding the following in one's ferc file:

  (workspaces (
    (do_not_auto_update (jane/foo bar/baz))
  ))
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and () = interactive
     and do_nothing_if_not_enabled =
       no_arg_flag Switch.do_nothing_if_not_enabled
         ~doc:" exit 0 if the functionality is not enabled by the user"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let client_config = Client_config.get () in
       if do_nothing_if_not_enabled
       && not (Client_config.Workspaces.auto_update_clean_workspaces_is_enabled
                 client_config)
       then
         Interactive.printf "Auto update of clean workspaces is not enabled, and \
                             the switch %s was supplied.\nExiting with code 0\n"
           Switch.do_nothing_if_not_enabled
       else begin
         Client_config.Workspaces.are_enabled_exn client_config;
         let%bind shares =
           let%map shares = Feature_share.list () in
           let do_not_auto_update =
             Client_config.Workspaces.do_not_auto_update client_config
           in
           List.filter shares ~f:(fun share ->
             not (Set.mem do_not_auto_update (Feature_share.feature_path share)))
         in
         let%map updates =
           Deferred.List.map ~how:(`Max_concurrent_jobs 5) shares ~f:(fun share ->
             let feature_path = Feature_share.feature_path share in
             let repo_root = Feature_share.center_repo_root share in
             Monitor.try_with_or_error (fun () ->
               match%bind Feature_share.unclean_status share with
               | Unclean _ -> return ()
               | Clean ->
                 match%bind
                   Get_feature_revs.rpc_to_server { feature_path; rev_zero = None }
                 with
                 | Error _ -> return ()
                 | Ok { tip ; remote_repo_path; _ } ->
                   Cmd_review.pull_and_update
                     ~repo_root
                     ~remote_repo_path
                     ~feature_path
                     ~review_session_tip:tip
                     ~feature_tip:tip;
             ))
         in
         updates
         |> Or_error.combine_errors_unit
         |> ok_exn
       end
    )
;;
