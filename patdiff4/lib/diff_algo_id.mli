open! Core
open Iron_common.Std

type t =
  [ `old_base_to_old_tip
  | `old_base_to_new_base
  | `old_base_to_new_tip
  | `old_tip_to_new_tip
  | `new_base_to_new_tip
  | `feature_ddiff
  | `base_ddiff
  | `story
  | `conflict_resolution
  | `metadata of string
  ]
[@@deriving sexp, enumerate]

include Stringable with type t := t
include Comparable with type t := t
include Hashable   with type t := t

val simple_diff : from:Diamond.Node.t -> to_:Diamond.Node.t -> t

val is_simple_diff : t -> bool
