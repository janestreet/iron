open Core.Std

type t =
  { range : Range.t
  ; lines : string list
  }
[@@deriving sexp_of, fields, compare]

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
  { range = t.range
  ; lines = lines @ t.lines
  }
;;

let append t lines =
  { range = t.range
  ; lines = t.lines @ lines
  }
;;
