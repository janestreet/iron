open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"set reviewer brains to the goal if there is already a fully-reviewed edge"
    ~readme:(fun () ->
      "\
This command is useful when Iron's notion of review goal changes, causing features that
were previously fully reviewed to become unreviewed.  In that situation, we typically want
the review-goal change to apply to unreviewed features, but not reviewed ones.  So, we
can use this command to adjust the reviewed ones.

This command does not generate catchup review.
"
    )
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     in
     fun () ->
       let feature_path = ok_exn feature_path in
       Set_brains_to_goal_if_edge.rpc_to_server_exn { feature_path }
    )
;;
