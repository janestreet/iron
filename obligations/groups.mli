open! Core
open! Import

type t = Unresolved_name.Set.t Group_name.Map.t
[@@deriving sexp_of]

val check_users : t -> known_users:Unresolved_name.Set.t -> unit Or_error.t

val eval_exn : Error_context.t -> t -> Group_name.t -> Unresolved_name.Set.t

val extend : t -> add_or_override_with:t -> t

val get_users_exn : t -> Group_name.t Blang.t -> Unresolved_name.Set.t

module Stable : sig
  module V1 : sig
    include Stable_without_comparator with type t = t
    val hash : t -> int
  end
end
