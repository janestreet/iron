open! Core
open! Import

(** Dedicated type for locks in next steps, since all locks do not make sense as a next
    step.  Example: [Unlock Rename] is not a valid next step. *)
module Lock_name : sig
  type t =
    | Rebase
    | Release
    | Release_into
    | Second
  [@@deriving sexp_of]

  val to_lock_name : t -> Lock_name.t
end

type t =
  | Add_code
  | Add_whole_feature_reviewer
  | Archive
  | Ask_seconder
  | Compress
  | CRs
  | Enable_review
  | Fix_build
  | Fix_problems
  | In_parent of t
  | Rebase
  | Release
  | Report_iron_bug
  | Restore_base
  | Restore_bookmark
  | Review
  | Unlock of Lock_name.t
  | Wait_for_continuous_release
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

val to_attrs
  :  t list
  -> review_is_enabled : bool
  -> Iron_ascii_table.Attr.t list

module Assigned : sig
  type nonrec t = t list [@@deriving sexp_of]

  val to_attrs_and_string
    :  t
    -> review_is_enabled : bool
    -> Iron_ascii_table.Attr.t list * string
end

module Stable : sig
  module Model : T with type t = t

  module V6 : sig
    include Stable_without_comparator with type t = Model.t
  end

  module V5 : sig
    include Stable_without_comparator
    val to_v6    : t -> V6.t
    val of_v6    : V6.t -> t
  end
end
