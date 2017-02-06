open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"create the bookmark, set to the feature tip"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       (* We [~use:`Share_or_clone_if_share_does_not_exist] rather than [~use:`Clone]
          because someone involved with the recovery of a bookmark may have a local
          bookmark in their share that points to some unpushed change, in which case we
          want them to get an error so they can their local bookmark with the feature tip
          as known by the server. *)
       let%bind repo_root =
         Cmd_workspace.repo_for_hg_operations_exn feature_path
           ~use:`Share_or_clone_if_share_does_not_exist
       in
       let%bind rev_zero = Hg.create_rev_zero repo_root in
       let%bind { tip; remote_repo_path } =
         Prepare_to_restore_bookmark.rpc_to_server_exn { feature_path; rev_zero }
       in
       let%bind () = Hg.pull repo_root (`Rev tip) ~from:remote_repo_path in
       let bookmark = Hg.Bookmark.Feature feature_path in
       let%bind () =
         match%bind Hg.create_rev repo_root (Hg.Revset.bookmark bookmark) with
         | Error _ -> Deferred.unit
         | Ok local_rev ->
           match%bind
             Hg.is_ancestor repo_root ~ancestor:tip ~descendant:local_rev
           with
           | true -> Deferred.unit
           | false ->
             failwith "\
Your local bookmark doesn't descend from the tip that Iron knows.
You must decide which revision to restore the bookmark at."
       in
       Hg.set_bookmark repo_root bookmark ~to_:(`Rev tip)
         (`Push_to_and_overwrite remote_repo_path)
    )
;;
