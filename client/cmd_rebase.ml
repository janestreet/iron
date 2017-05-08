open! Core
open! Async
open! Import


let main { Fe.Rebase.Action.
           feature_path
         ; allow_non_cr_clean_new_base
         ; for_
         ; new_base
         ; abort_on_merge_conflicts
         ; post_merge_validation_hook
         } =
  let%bind repo_root =
    Cmd_workspace.repo_for_hg_operations_exn feature_path
      ~use:`Share_or_clone_if_share_does_not_exist
  in
  let client_config = Client_config.get () in
  let merge_tool = Client_config.Cmd.Rebase.merge_tool client_config in
  let%bind repo_is_clean = Hg.status_cleanliness repo_root in
  let repo_is_clean = ok_exn repo_is_clean in
  let%bind rev_zero = Hg.create_rev_zero repo_root in
  let%bind new_base = Raw_rev.resolve_opt_exn new_base ~in_:(Ok repo_root) in
  let%bind () =
    match new_base with
    | None -> Deferred.unit
    | Some new_base ->
      let parent_path = ok_exn (Feature_path.parent feature_path) in
      let%bind parent =
        Get_feature_revs.rpc_to_server_exn
          { feature_path = parent_path; rev_zero = Some rev_zero }
      in
      let%bind () =
        Hg.pull ~repo_is_clean repo_root ~from:parent.remote_repo_path
          (`Rev parent.tip)
      in
      let%bind is_ancestor =
        Hg.is_ancestor repo_root ~ancestor:new_base ~descendant:parent.tip
      in
      (* to guarantee that rebasing again after this rebase will make sense *)
      if not is_ancestor
      then failwithf !"new base (%{Rev#12}) must be an ancestor of the parent feature's \
                       tip (%{Rev#12})"
             new_base parent.tip ();
      let%bind feature =
        Get_feature_revs.rpc_to_server_exn
          { feature_path; rev_zero = Some rev_zero }
      in
      let%map is_ancestor =
        Hg.is_ancestor repo_root ~ancestor:feature.base ~descendant:new_base
      in
      (* try to fail right away with a better message than the gca check below *)
      if not is_ancestor
      then failwithf !"new base (%{Rev#12}) must descend from the feature's current base \
                       (%{Rev#12})"
             new_base feature.base ()
  in
  let%bind { Prepare_to_rebase.Reaction.
             old_tip
           ; old_base
           ; new_base
           ; remote_repo_path
           ; feature_id
           } =
    Prepare_to_rebase.rpc_to_server_exn
      { feature_path; rev_zero; allow_non_cr_clean_new_base; for_; new_base }
  in
  let%bind () =
    Hg.pull ~repo_is_clean repo_root ~from:remote_repo_path
      (`Feature feature_path)
  in
  let%bind local_feature =
    Hg.create_rev repo_root (Revset.feature_tip feature_path)
  in
  let local_feature = ok_exn local_feature in
  if not (Rev.equal_node_hash old_tip local_feature)
  then raise_s
         [%sexp "local bookmark is not the same as Iron server's"
              , { local       = (local_feature : Rev.t)
                ; iron_server = (old_tip       : Rev.t)
                }];
  let%bind () =
    Hg.pull ~repo_is_clean repo_root ~from:remote_repo_path (`Rev new_base)
  in
  let%bind gca = Hg.greatest_common_ancestor repo_root old_tip new_base in
  if not (Rev.equal_node_hash old_base gca)
  then
    if Rev.equal_node_hash old_tip gca
    then
      raise_s
        [%sexp
          "feature's tip is already an ancestor of new base",
          { feature_tip = (old_tip  : Rev.t)
          ; new_base    = (new_base : Rev.t)
          }
        ]
    else
      raise_s
        [%sexp
          "feature's base is unexpectedly not the greatest-common ancestor of the \
           feature and the new base",
          { feature                           = (old_tip  : Rev.t)
          ; new_base                          = (new_base : Rev.t)
          ; expected_greatest_common_ancestor = (old_base : Rev.t)
          ; actual_greatest_common_ancestor   = (gca      : Rev.t)
          }
        ];
  let%bind () =
    if Rev.equal_node_hash old_tip old_base
    then (
      (* The feature is empty -- just set the bookmark. *)
      let%bind () =
        Hg.set_bookmark repo_root (Feature feature_path) ~to_:(`Rev new_base)
          `Do_not_push
      in
      Hg.update repo_root (`Feature feature_path)
        ~clean_after_update:(Yes repo_is_clean))
    else (
      let post_merge_validation_hook =
        Option.map post_merge_validation_hook ~f:(fun { prog; args } ->
          fun () ->
            Process.run
              ~working_dir:(repo_root |> Repo_root.to_abspath |> Abspath.to_string)
              ~prog ~args ()
            >>|? fun (_stdout : string) -> ())
      in
      let%map result =
        Hg.rebase ~abort_on_merge_conflicts ?post_merge_validation_hook
          repo_root feature_path ~old_tip ~old_base ~new_base ~feature_id
          ?merge_tool ~repo_is_clean
      in
      ok_exn result)
  in
  (* We push before setting the base on the server, because the push is slow and the set
     is fast.  If we did things in the other order, there is a race that happens in
     practice, where the hydra worker retries the nonsensical feature with the old tip and
     the new base.  In this order, we have a race if the pull is slow to return, hydra can
     compute the diff old base -> new tip, which may have a fully reviewed edge, and cause
     a bunch of unintended things to end up in people's brains (and even without the fully
     reviewed edge, the potentially flickering review is not ideal). So we use
     [expect_next_base_update] to avoid both races. *)
  let%bind () =
    Expect_next_base_update.rpc_to_server_exn
      { feature_path; for_; expected_base = new_base }
  in
  let%bind () =
    let%map result =
      Hg.push repo_root [ Feature feature_path ] ~to_:remote_repo_path
        ~overwrite_bookmark:false
    in
    ok_exn result
  in
  let%bind () =
    match%map try_with (fun () ->
      Cmd_hg_hooks.maybe_send_push_event ~repo_root)
    with
    | Ok () | Error (_ : Exn.t) -> ()
  in
  let%bind () =
    if not am_functional_testing
    then Deferred.unit
    else (
      match Sys.getenv "IRON_FUNCTIONAL_TESTING_REBASE_RACE" with
      | None -> Deferred.unit
      | Some cmd ->
        let%bind process =
          Process.create ~prog:"/bin/bash" ~args:[ "-c" ; cmd ] ()
        in
        let%map output = Process.collect_output_and_wait (ok_exn process) in
        Print.printf !"%{sexp:Process.Output.t}\n" output)
  in
  let%bind () =
    Cmd_change.change_feature ~feature_path ~updates:[ `Set_base new_base ] ()
  in
  Cmd_workspace.If_enabled.update_satellite_repos ~center_repo_root:repo_root
;;

let command =
  Command.async'
    ~summary:"rebase a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and allow_non_cr_clean_new_base =
       no_arg_flag "allow-non-cr-clean-new-base"
         ~doc:"rebase even though the new base isn't CR clean"
     and for_ = for_
     and new_base = new_base
     and abort_on_merge_conflicts =
       no_arg_flag Switch.abort_on_merge_conflicts
         ~doc:" abort the rebase and fail if a merge is performed and creates \
               conflicts markers"
     and () =
       interactive
     in
     fun () ->
       let feature_path = ok_exn feature_path in
       main { feature_path
            ; allow_non_cr_clean_new_base
            ; for_
            ; new_base
            ; abort_on_merge_conflicts
            ; post_merge_validation_hook = None
            }
    )
;;
