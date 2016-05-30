open! Core.Std
open! Async.Std
open! Import

let make_parent_active_if_feature_is_active repo_root feature_path repo_is_clean =
  match Feature_path.parent feature_path with
  | Error _ -> Deferred.unit
  | Ok parent_path ->
    match%bind Hg.current_bookmark repo_root with
    | Error _ -> Deferred.unit
    | Ok current_bookmark ->
      if String.(=) (Feature_path.to_string feature_path) current_bookmark
      then Hg.update repo_root (`Feature parent_path)
             ~clean_after_update:(Yes repo_is_clean)
      else Deferred.unit
;;

let main { Fe.Release.Action. feature_path; for_; included_features_order } =
  let%bind repo_root =
    Cmd_workspace.repo_for_hg_operations_exn feature_path ~use:`Clone
  in
  let%bind repo_is_clean = Hg.status_cleanliness repo_root in
  let repo_is_clean = ok_exn repo_is_clean in
  let%bind rev_zero = Hg.create_rev_zero repo_root in
  let%bind feature =
    Get_feature.rpc_to_server_exn { feature_path; rev_zero  = Some rev_zero }
  in
  let remote_repo_path = feature.remote_repo_path in
  (* We call [ensure_can_access_remote_repo] before calling [Release.rpc_to_server_exn]
     in the hope of aborting early due to hg problems (e.g. permissions) before side
     effecting the state.  Being able to do the hg operations after the
     [Release.rpc_to_server_exn] is an essential part of the semantics. *)
  let%bind () = Hg.ensure_can_access_remote_repo remote_repo_path in
  let%bind () =
    Hg.pull ~repo_is_clean repo_root ~from:remote_repo_path (`Rev feature.tip)
  in
  let%bind tagged_tip =
    (* Can't rely on the worker for this tagging, because we won't receive any update
       about it. *)
    Hg.infer_tag repo_root ~root:(Feature_path.root feature_path) feature.tip
  in
  let%bind { disposition; send_release_email_to } =
    Release.rpc_to_server_exn { feature_path; for_; rev_zero; tagged_tip }
  in
  let%bind () =
    match disposition with
    | `Not_released__push_to_hydra ->
      Hg.set_bookmark repo_root (Release feature_path) ~to_:(`Rev feature.tip)
        (`Push_to_and_overwrite remote_repo_path)
    | `Released_and_cleared _ | `Released_and_archived as disposition ->
      let%bind () =
        begin match Feature_path.parent feature_path with
        | Error _ -> return ()
        | Ok parent_path ->
          Hg.set_bookmark repo_root (Feature parent_path) ~to_:(`Rev feature.tip)
            (`Push_to_and_advance remote_repo_path)
        end
      in
      let%bind () =
        begin match disposition with
        | `Released_and_cleared _ -> Deferred.unit
        | `Released_and_archived ->
          let%bind () =
            make_parent_active_if_feature_is_active repo_root feature_path repo_is_clean
          in
          Hg.delete_bookmarks repo_root [ Feature feature_path ]
            (`Push_to remote_repo_path)
        end
      in
      if am_functional_testing
      || not (Set.mem feature.send_email_upon Send_email_upon.release)
      then return ()
      else begin
        let%map iron_config = force Iron_config.as_per_IRON_CONFIG in
        let reply_to =
          (* This prevents followup discussion on the release from going to as-hydra.
             We have to put one name in reply_to, we can't just put no one, otherwise
             it gets ignored. *)
          if User_name.equal User_name.unix_login iron_config.hydra_user
          then Some [ List.hd_exn feature.owners |> User_name.to_string ]
          else None
        in
        Core_extended.Sendmail.send
          (Cmd_show.render_email_body feature ~included_features_order)
          ?reply_to
          ~subject:(sprintf !"feature was released: %{Feature_path}" feature_path)
          ~recipients:(List.map (Set.to_list send_release_email_to)
                         ~f:Email_address.to_string)
      end
  in
  let%bind () =
    match disposition with
    | `Not_released__push_to_hydra ->
      (* keep workspace around in case hydra rejects the push *)
      Deferred.unit
    | `Released_and_cleared _ -> Deferred.unit
    | `Released_and_archived ->
      Cmd_workspace.If_enabled.delete_workspace feature_path
  in
  let%bind () =
    Interactive.printf "%s\n"
    (match disposition with
     | `Not_released__push_to_hydra -> "Submitted to hydra for continuous release."
     | `Released_and_cleared reasons ->
       concat [ "Released, but didn't archive -- "
              ; Release.Reasons_for_not_archiving.to_string_hum
                  reasons
              ]
     | `Released_and_archived -> "Released and archived.")
  in
  begin
    match disposition with
    | `Not_released__push_to_hydra -> Deferred.unit
    | `Released_and_cleared _ | `Released_and_archived ->
      if not (Cmd_workspace.workspaces_are_enabled ())
      then Deferred.unit
      else
        match Feature_path.parent feature_path with
        | Error _ -> Deferred.unit
        | Ok parent_path ->
          match%bind Feature_share.find parent_path with
          | None -> Deferred.unit
          | Some parent_workspace ->
            if true
            then Deferred.unit
            else
            let parent_repo_root = Feature_share.center_repo_root parent_workspace in
            Interactive.Job.run "Updating parent workspace to new released tip"
              ~f:(fun () ->
                Cmd_review.pull_and_update
                  ~repo_root:parent_repo_root
                  ~remote_repo_path
                  ~feature_path:parent_path
                  ~review_session_tip:feature.tip
                  ~feature_tip:feature.tip)
  end
;;

let command =
  Command.async'
    ~summary:"release a feature, and archive it if possible"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and for_ = for_
     and included_features_order = included_features_order
     and () = interactive
     in
     fun () ->
       let feature_path = ok_exn feature_path in
       main { feature_path; for_; included_features_order }
    )
;;
