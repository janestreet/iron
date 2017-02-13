open! Core
open! Import

module Action : sig
  type t =
    { root_feature   : Feature_name.t
    ; for_or_all     : User_name.Or_all.t
    ; include_active : bool
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t = Cr_soon_multiset.t [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
