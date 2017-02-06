open Core
open Async
open Import

module Reaction_hum = struct
  type t = De_alias_feature.Reaction.t =
    { de_aliased
      : User_name.Set.t [@sexp_drop_if Set.is_empty]
    ; did_not_de_alias_due_to_review_session_in_progress
      : User_name.Set.t [@sexp_drop_if Set.is_empty]
    ; nothing_to_do
      : User_name.Set.t [@sexp_drop_if Set.is_empty]
    }
  [@@deriving sexp_of]
end

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
       printf "%s\n" (Sexp.to_string_hum [%sexp (reaction : Reaction_hum.t)]);
       return ()
    )
;;
