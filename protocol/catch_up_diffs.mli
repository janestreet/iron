open! Core
open! Import

module Action : sig
  type t =
    { feature_path         : Feature_path.t
    ; for_                 : User_name.t
    ; catch_up_session_id  : Session_id.t
    ; diff4_in_session_ids : Diff4_in_session.Id.t list
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
