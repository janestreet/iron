open! Core
open! Async
open! Import

(* For robustness, this code is structured like:

   1. An RPC to find out what would need to happen to do all the renames.
   2. hg operations (pulls, bookmark adds and deletes)
   3. An RPC to change state on Iron server.
   4. move workspace, if any

   That way, if there's an hg problem, we haven't yet changed Iron server.
*)

let main { Fe.Rename.Action.
           from
         ; to_
         ; skip_gca_check
         } =
  let%bind repo_root = Cmd_workspace.repo_for_hg_operations_exn from ~use:`Clone in
  let%bind repo_is_clean = Hg.status_cleanliness repo_root in
  let repo_is_clean = ok_exn repo_is_clean in
  let%bind rev_zero = Hg.create_rev_zero repo_root in
  let%bind { from_feature_path
           ; from_base
           ; from_tip
           ; to_parent_feature_path
           ; to_parent_tip
           ; renames
           ; remote_repo_path
           } =
    Prepare_to_rename.rpc_to_server_exn { from; to_; rev_zero }
  in
  let%bind () =
    match skip_gca_check with
    | true -> Deferred.unit
    | false ->
      let%bind () =
        Hg.pull ~repo_is_clean repo_root ~from:remote_repo_path
          (`Revs [ from_tip; to_parent_tip ])
      in
      let%map gca = Hg.greatest_common_ancestor repo_root from_tip to_parent_tip in
      if not (Rev.equal_node_hash gca from_base)
      then
        raise_s
          [%sexp
            "greatest common ancestor of feature and its new parent is not equal \
             to the feature's base",
            { feature    = (from_feature_path      : Feature_path.t)
            ; new_parent = (to_parent_feature_path : Feature_path.t)
            ; expected_greatest_common_ancestor = (from_base : Rev.t)
            ; actual_greatest_common_ancestor = (gca : Rev.t)
            }
          ]
  in
  let%bind () = ensure_can_access_remote_repo ~for_root_of:from in
  let%bind () = Hg.rename repo_root remote_repo_path renames ~repo_is_clean in
  let%bind () = Rename_feature.rpc_to_server_exn { from; to_ } in
  Cmd_workspace.If_enabled.rename_workspaces renames
;;

let command =
  let new_parent_switch = "-new-parent" in
  Command.async'
    ~summary:"rename a feature (and all features below it)"
    ~readme:(fun () -> concat [ "\
The target feature is not restricted to be a sibling of the original feature, i.e.
renaming [jane/a/b] to [jane/c] (or vice versa) is permissible.

Using [-skip-gca-check] skips the check that the greatest common ancestor of the current
tip and the new base is the current base.  Skipping the check may mean future rebases and
renames will be rejected.

When the basename of the feature is not meant to change, one can use [" ;
                                new_parent_switch ; "].
For example, to rename [jane/a/keep-this-basename] into [jane/b/keep-this-basename]:

  $ fe rename jane/a/keep-this-basename -new-parent jane/b
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and from = feature_path
     and to_  =
       let%map rename_to_absolute_feature_path = absolute_feature_path_option
       and rename_to_child_of =
         flag new_parent_switch (optional (feature_arg_type ~match_existing_feature:true))
           ~doc:"FEATURE Keep the same basename for the feature, rename it as a child of \
                 the supplied parent feature."
       in
       Or_error.try_with (fun () ->
         match ok_exn rename_to_absolute_feature_path, rename_to_child_of with
         | Some absolute_feature_path, None
           -> `Rename_to_absolute_feature_path absolute_feature_path
         | None, Some new_parent
           -> `Rename_to_child_of (ok_exn new_parent)
         | None, None -> failwith "The new feature-path needs to be specified."
         | Some _, Some _ ->
           failwithf "One cannot supply %s when the target FEATURE name is supplied."
             new_parent_switch ()
       )
     and skip_gca_check = flag "-skip-gca-check" no_arg ~doc:" <see above>"
     and () = even_if_locked
     in
     fun () ->
       let from = ok_exn from in
       let to_  =
         match ok_exn to_ with
         | `Rename_to_absolute_feature_path to_ -> to_
         | `Rename_to_child_of new_parent
           -> Feature_path.extend new_parent (Feature_path.basename from)
       in
       main { from
            ; to_
            ; skip_gca_check
            }
    )
;;
