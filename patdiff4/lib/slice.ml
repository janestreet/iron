open! Core
open! Import

type t =
  { range : Range.t
  ; lines : string list
  }
[@@deriving compare, fields, sexp_of]

let create ~source line contents =
  { range =
      { Range.
        source     = source
      ; line_start = line
      ; line_end   = line + List.length contents
      }
  ; lines = contents
  }
;;

let prepend lines t =
  { range = Range.prepend (List.length lines) t.range
  ; lines = lines @ t.lines
  }
;;

let append t lines =
  { range = Range.append t.range (List.length lines)
  ; lines = t.lines @ lines
  }
;;
