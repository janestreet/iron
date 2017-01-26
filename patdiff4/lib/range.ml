open! Core
open! Import

type t =
  { source     : string
  ; line_start : int
  ; line_end   : int
  }
[@@deriving compare, fields, sexp_of]

let merge t1 t2 =
  assert (String.equal t1.source t2.source);
  { source     = t1.source
  ; line_start = Int.min t1.line_start t2.line_start
  ; line_end   = Int.max t1.line_end   t2.line_end
  }
;;

let to_header ~other_names t =
  { Header.Source.
    name        = t.source
  ; other_names
  ; range       = Some (t.line_start, t.line_end)
  }
;;

let prepend lines t = { t with line_start = Int.max 0 (t.line_start - lines) }
let append t lines  = { t with line_end = t.line_end + lines }
