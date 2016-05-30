open! Core.Std
open! Import

type t =
  { executable : Abspath.t
  ; args       : string
  }
[@@deriving sexp]
