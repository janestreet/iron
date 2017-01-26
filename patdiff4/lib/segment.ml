open! Core
open! Import

type t =
  { slice       : Slice.t Diamond.t
  ; diff4_class : Diff4_class.t
  }
[@@deriving sexp_of]

let is_shown t = Diff4_class.is_shown t.diff4_class

let prepend lines t =
  let slice = Diamond.map t.slice ~f:(fun slice ->
    Slice.prepend lines slice)
  in
  { slice
  ; diff4_class = t.diff4_class
  }
;;

let append t lines =
  let slice = Diamond.map t.slice ~f:(fun slice ->
    Slice.append slice lines)
  in
  { slice
  ; diff4_class = t.diff4_class
  }
;;
