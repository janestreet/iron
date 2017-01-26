open! Core
open! Import

type t =
  { rev_facts   : Rev_facts.t
  ; crs         : Cr_comment.t list Or_error.t
  ; cr_soons    : Cr_soon.t    list Or_error.t
  }
[@@deriving compare, sexp_of]

include Invariant.S with type t := t

val for_sorted_output : t -> t

module Concise : sig
  type nonrec t = t [@@deriving sexp_of]
end

module Stable : sig
  module V1 : sig
    type nonrec t = t =
      { rev_facts   : Rev_facts.Stable.V1.t
      ; crs         : Cr_comment.Stable.V1.t list Or_error.Stable.V1.t
      ; cr_soons    : Cr_soon.Stable.V1.t    list Or_error.Stable.V1.t
      }
    include Stable_without_comparator with type t := t
  end
  module Model : module type of struct include V1 end
end

module On_server : sig

  (** This type is morally [type nonrec t = t], but its representation on server is
      different to allow for a more aggressive sharing across revisions and features.  *)
  type t [@@deriving sexp_of]

  include Invariant.S with type t := t

  val of_stable : Stable.Model.t -> t
  val to_stable : t -> Stable.Model.t
end
