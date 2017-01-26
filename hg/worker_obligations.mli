open! Core
open! Import

module Stable : sig
  module V5 : Stable_without_comparator
  module Model : module type of struct include V5 end
end

type t =
  { obligations_are_valid : Rev_facts.Obligations_are_valid.t
  ; obligations           : Obligations.t Or_error.t
  ; obligations_version   : Obligations_version.t Or_error.t
  }
[@@deriving sexp_of]

val of_stable : Stable.Model.t -> t
val to_stable : t -> Stable.Model.t

module On_server : sig

  (** This type is morally [type nonrec t = t], but its representation on server is
      different to allow for a more aggressive sharing across revisions and features.  *)
  type t [@@deriving sexp_of]

  include Invariant.S with type t := t

  val of_stable : Stable.Model.t -> t
  val to_stable : t -> Stable.Model.t

  (** This is used to double check the validity of the cached obligations for a given rev
      by comparing this output with a fresh and local computation of [fe obl report] at
      the right revision. *)
  val dump_obligations : t -> Sexp.t

end
