open! Core
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val create : unit -> t

module Parties : sig
  type t =
    | Owners
    | Whole_feature_followers
  [@@deriving enumerate, sexp_of]
end

val set_users : t -> Feature_id.t -> Parties.t -> User_name.Set.t -> unit

val remove_feature : t -> Feature_id.t -> unit

val find : t -> User_name.t -> Parties.t -> Feature_id.t list
