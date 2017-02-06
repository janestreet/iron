open! Core
open! Async
open! Import

let check_cached_feature_attributes =
  Command.async'
    ~summary:"check cached attributes in features.  Correct the inconsistent ones"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and which_features =
       which_features ~allow_empty_selection:false ~default_to_current_bookmark:false ()
     and ignore_diffs_in_errors =
       flag "-ignore-diffs-in-errors" no_arg
         ~doc:" do not report differences between two error results"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind which_features = force which_features in
       Check_cached_feature_attributes.rpc_to_server_exn
         { which_features
         ; ignore_diffs_in_errors
         }
    )
;;

let force_set_cached_feature_attributes =
  Command.async'
    ~summary:"set arbitrary cached attributes for a feature \
              (for testing only, fails in prod)"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path =
       feature_path_or_current_bookmark
     and skip_post_RPC_check =
       flag "-skip-post-rpc-check" no_arg ~doc:" do not perform post RPC cached check"
     and next_steps =
       flag "-next-steps" (required string) ~doc:"SEXP specify value for next steps"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let next_steps =
         try
           next_steps
           |> Sexp.of_string
           |> [%of_sexp: Next_step.For_command_line.t list]
         with exn -> raise_s [%sexp "error while parsing next steps", (exn : Exn.t)]
       in
       Force_set_cached_feature_attributes.rpc_to_server_exn
         { feature_path
         ; skip_post_RPC_check
         ; next_steps
         }
    )
;;

let invalidate_cached_feature_attributes =
  Command.async'
    ~summary:"force invalidation of cached attributes in features"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and which_features =
       which_features ~allow_empty_selection:false ~default_to_current_bookmark:false ()
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind which_features = force which_features in
       Invalidate_cached_feature_attributes.rpc_to_server_exn which_features
    )
;;

let clear_cached_attributes_errors =
  Command.async'
    ~summary:"clear cached-attribute errors on server"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       With_cached_attributes_errors.rpc_to_server_exn Clear
    )
;;

let get_cached_attributes_errors =
  Command.async'
    ~summary:"get cached-attribute errors on server"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       With_cached_attributes_errors.rpc_to_server_exn Get
    )
;;

let errors_group =
  Command.group ~summary:"deal with cached-attribute errors"
    [ "clear" , clear_cached_attributes_errors
    ; "get"   , get_cached_attributes_errors
    ]
;;

let command =
  Command.group ~summary:"deal with cached attributes in features"
    [ "check"      , check_cached_feature_attributes
    ; "force-set"  , force_set_cached_feature_attributes
    ; "invalidate" , invalidate_cached_feature_attributes
    ; "errors"     , errors_group
    ]
;;
