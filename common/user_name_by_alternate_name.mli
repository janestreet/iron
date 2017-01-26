(** A map from alternate name to user name. *)

open! Core
open! Import

type t [@@deriving compare, sexp_of]

include Invariant.S with type t := t

val not_available : t

val to_user_name      : t -> Unresolved_name.t -> User_name.t
val to_user_name_opt  : t -> Unresolved_name.t -> User_name.t option

val remove_if_present : t -> alternate_name:Alternate_name.t -> t option

(** Trying to add a binding from an alternate name to a user name fails if there is
    already a binding for that alternate name to a _different_ user name. *)
val add_exn
  :  t
  -> alternate_name:Alternate_name.t
  -> user_name:User_name.t
  -> on_error:(Alternate_name.t -> User_name.t list -> never_returns)
  -> t

val iteri : t -> f:(key:Alternate_name.t -> data:User_name.t -> unit) -> unit

val merge_exn
  :  t
  -> t
  -> on_error:(Alternate_name.t -> User_name.t list -> never_returns)
  -> t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
