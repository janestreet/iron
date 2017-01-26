open! Core
open! Import

module Action : sig
  type t =
    { feature_path                            : Feature_path.t
    ; for_                                    : User_name.t
    ; reason                                  : string
    ; create_catch_up_for_me                  : bool
    ; even_if_some_files_are_already_reviewed : bool
    ; review_session_id                       : Session_id.t
    ; diff4_in_session_ids                    : Diff4_in_session.Id.t list
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
