open! Core
open! Import

module Action : sig
  type t =
    { feature_path           : Feature_path.t
    ; whom_to_mark           : User_name.Or_all_or_all_but.t
    ; reason                 : string
    ; create_catch_up_for_me : bool
    ; base                   : Rev.t option
    ; tip                    : Rev.t option
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
