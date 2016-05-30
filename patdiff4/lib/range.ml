open Core.Std

type t = {
  source     : string;
  line_start : int; (* inclusive *)
  line_end   : int; (* exclusive *)
} [@@deriving sexp_of, fields, compare]

let merge t0 t1 =
  assert(t0.source = t1.source);
  { source     = t0.source
  ; line_start = Int.min t0.line_start t1.line_start
  ; line_end   = Int.max t0.line_end   t1.line_end
  }
;;

let to_header ~other_names t =
  let name = t.source in
  let range = Some (t.line_start, t.line_end) in
  { Header.Source.
    name
  ; other_names
  ; range
  }
;;

let prepend lines t = { t with line_start = t.line_start - lines }
let append t lines  = { t with line_end = t.line_end + lines }
