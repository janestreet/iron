open! Core
open! Async
open! Import

let main { Fe.Compress.Action.
           feature_path
         ; for_
         } =
  let%bind repo_root =
    Cmd_workspace.repo_for_hg_operations_exn feature_path ~use:`Clone
  in
  let%bind repo_is_clean = Hg.status_cleanliness repo_root in
  let repo_is_clean = ok_exn repo_is_clean in
  let%bind rev_zero = Hg.create_rev_zero repo_root in
  let%bind { feature_tip; parent_tip; renames; remote_repo_path } =
    Prepare_to_compress.rpc_to_server_exn { feature_path; for_; rev_zero }
  in
  let%bind () =
    Hg.pull repo_root ~repo_is_clean ~from:remote_repo_path
      (`Revs [ feature_tip; parent_tip ])
  in
  let%bind is_ancestor =
    Hg.is_ancestor repo_root ~ancestor:feature_tip ~descendant:parent_tip
  in
  if not is_ancestor
  then failwithf !"compressed feature's tip (%{Rev#12}) must be ancestor of its \
                   parent's tip (%{Rev#12})"
         feature_tip parent_tip ();
  let%bind () =
    Hg.delete_bookmarks repo_root [ Feature feature_path ] (`Push_to remote_repo_path)
  in
  let%bind () = Hg.rename repo_root remote_repo_path renames ~repo_is_clean in
  let%bind () = Compress.rpc_to_server_exn { feature_path; for_ } in
  let%bind () = Cmd_workspace.If_enabled.rename_workspaces renames in
  Cmd_workspace.If_enabled.delete_workspace feature_path
;;

let command =
  Command.async'
    ~summary:"archive an empty feature, renaming its descendants"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path
     and for_ = for_
     and () = even_if_locked
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       main { feature_path
            ; for_
            }
    )
;;
