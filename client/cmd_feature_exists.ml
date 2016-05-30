open! Core.Std
open! Async.Std
open! Import

let command =
  Command.async'
    ~summary:"print [true] or [false], depending on whether the feature exists"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = absolute_feature_path
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%map bool = Feature_exists.rpc_to_server_exn feature_path in
       printf "%b\n" bool
    )
;;
