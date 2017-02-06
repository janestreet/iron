open! Core
open! Async
open! Import

let enable_command =
  Command.async'
    ~summary:"enable review of a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and add_whole_feature_reviewers =
       users_option ~switch:Switch.add_whole_feature_reviewers
     in
     fun () ->
       let feature_path = ok_exn feature_path in
       Enable_review.rpc_to_server_exn
         { feature_path
         ; add_whole_feature_reviewers
         }
    )
;;

let disable_command =
  Command.async'
    ~summary:"disable review of a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and and_crs = no_arg_flag "-and-crs" ~doc:"disable CRs as well"
     in
     fun () ->
       let feature_path = ok_exn feature_path in
       Cmd_change.change_feature ~feature_path
         ~updates:
           (if and_crs
            then [ `Set_review_is_enabled false; `Set_crs_are_enabled false ]
            else [ `Set_review_is_enabled false ])
         ()
    )
;;
