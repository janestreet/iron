open! Core
open! Import

type t =
  { assignee : User_name.t
  ; assigned : Next_step.t list
  }
[@@deriving sexp_of]

val create : Feature.t -> parent:Feature.t option -> t option
