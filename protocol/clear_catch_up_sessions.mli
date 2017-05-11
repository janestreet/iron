(** Remove and return all catch-up sessions for a user for a feature. *)

open! Core
open! Import

module Action : sig
  (** [for_] is for admin only.  Clearing catch-up for other is rejected if requested by
      non special users. *)
  type t =
    { feature_path           : Feature_path.t
    ; for_                   : User_name.t
    ; ok_if_nothing_cleared  : bool
    ; only_those_reviewed_by : Unresolved_name.t Blang.t
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
