open! Core.Std
open! Async.Std
open! Import

module Action = struct
  type t =
    { files : string Diamond.t
    ; lines_required_to_separate_ddiff_hunks : int
    }
  [@@deriving sexp]
end

module Reaction = struct
  type t =
    { num_lines_in_diff4 : int
    }
  [@@deriving sexp]
end

let internal_group = "process-num-lines-in-diff4"

let compute input =
  let prog = Sys.executable_name in
  let args = [ "internal" ; internal_group ] in
  let input_sexp = Action.sexp_of_t input in
  match%map Process.run ~stdin:(Sexp.to_string input_sexp) ~prog ~args () with
  | Error error ->
    failwiths "line count computation error"
      (input_sexp, error)
      [%sexp_of: Sexp.t * Error.t]
  | Ok stdout ->
    try Sexp.of_string_conv_exn (String.strip stdout) Reaction.t_of_sexp
    with exn ->
      failwiths "line count process parse sexp error"
        (input_sexp, stdout, exn)
        [%sexp_of: Sexp.t * string * Exn.t]
;;
