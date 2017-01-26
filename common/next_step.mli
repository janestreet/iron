open! Core
open! Import

(* Dedicated type for locks in next steps, since all locks do not make sense as a next
   step.  Example: [Unlock Rename] is not a valid next step. *)
module Lock_name : sig
  type t =
    | Rebase
    | Release
    | Release_into
  [@@deriving sexp_of]

  val to_lock_name : t -> Lock_name.t
end

type t =
  | Add_code
  | Add_whole_feature_reviewer
  | Ask_seconder
  | CRs
  | Enable_review
  | Fix_problems
  | In_parent of t
  | Rebase
  | Release
  | Report_iron_bug
  | Restore_bookmark
  | Review
  | Unlock of Lock_name.t
  | Wait_for_hydra
  | Widen_reviewing
[@@deriving sexp_of]

module For_command_line : sig
  type nonrec t = t [@@deriving sexp]
end

include Equal.S with type t := t

val to_string_hum : t -> string

val to_attrs_and_string
  :  t list
  -> review_is_enabled : bool
  -> Iron_ascii_table.Attr.t list * string

module Stable : sig
  module Model : T with type t = t

  module V5 : sig
    include Stable_without_comparator with type t = Model.t
    val of_model : Model.t -> t
  end

  module V4 : sig
    include Stable_without_comparator
    val of_model : Model.t -> t
  end

  module V3 : sig
    include Stable_without_comparator
    val of_model : Model.t -> t
  end
end
