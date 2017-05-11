open! Core
open! Import

module Action : sig
  (** [for_] is for testing only.  Prod Iron will reject locking on behalf of someone
      else. *)
  type t =
    { feature_path : Feature_path.t
    ; for_         : User_name.t
    ; lock_names   : Lock_name.t list
    ; reason       : string
    ; is_permanent : bool
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t = (Lock_name.t * unit Or_error.t) list
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
