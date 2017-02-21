open! Core
open! Import

module Sent_upon : sig
  type t =
    | Archive
    | Release
    | Release_into
  [@@deriving enumerate, sexp_of]
end

module Action : sig
  type t =
    { feature_path : Feature_path.t
    ; sent_upon    : Sent_upon.t
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t = { recipients : Email_address.Set.t }
  [@@deriving fields, sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
