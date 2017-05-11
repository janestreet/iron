(**
   B1 = old_base
   B2 = new_base
   F1 = old_tip
   F2 = new_tip

   Time moves bottom-to-top.

   {v
        F2
        / \
       /   \
      /     \
     F1      B2
      \     /
       \   /
        \ /
         B1
   v}

   An equivalence between these revisions is used to show more intelligent diffs.
*)

open! Core

type t =
  [ `b1_b2_f1_f2
  | `b1_b2_f1
  | `b1_b2_f2
  | `b1_b2__f1_f2
  | `b1_b2
  | `b1_f1_f2
  | `b1_f1__b2_f2
  | `b1_f1
  | `b1_f2__b2_f1
  | `b1_f2
  | `b2_f1_f2
  | `b2_f1
  | `b2_f2
  | `f1_f2
  | `conflict
  ]
[@@deriving sexp, compare, enumerate]

(** sexp is used in .patdiff4 config files *)

include Comparable with type t := t
include Hashable   with type t := t

module Shown_class : sig
  type class_ = t
  type t =
    [ `b1_b2_f1
    | `b1_b2_f2
    | `b1_b2
    | `b1_f1_f2
    | `b1_f1
    | `b1_f2__b2_f1
    | `b1_f2
    | `b2_f1
    | `b2_f2
    | `f1_f2
    | `conflict
    ]
  [@@deriving sexp, compare, enumerate]

  include Comparable with type t := t
  include Hashable   with type t := t

  val to_class : t -> class_
  val of_class : class_ -> t option
end

val classify : equal:('a -> 'a -> bool) -> b1:'a -> b2:'a -> f1:'a -> f2:'a -> t

val is_shown : t -> bool

val to_groups : t -> [ `b1 | `b2 | `f1 | `f2 ] list list

(** Outputs a human representation of the equivalence sets Example:

    {[
      | B1_f2__b2_f1  -> "{ B1 F2 } { B2 F1 }"
    ]}
*)
val to_string : t -> string

val is_forget : t -> bool
