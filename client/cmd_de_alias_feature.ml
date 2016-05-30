open Core.Std
open Async.Std
open Import

let command =
  Command.async'
    ~summary:"de-alias a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind reaction =
         De_alias_feature.rpc_to_server_exn { feature_path }
       in
       printf "%s\n"
         (Sexp.to_string_hum
            (reaction |> [%sexp_of: De_alias_feature.Reaction.t]));
       return ()
    )
;;
