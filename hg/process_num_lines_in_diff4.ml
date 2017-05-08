open! Core
open! Async
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
  let args = [ "internal" ; internal_group; Sexp.to_string (Action.sexp_of_t input) ] in
  match%map Process.run ~prog ~args () with
  | Error error ->
    raise_s [%sexp "line count computation error"
                 , (input : Action.t), (error : Error.t)]
  | Ok stdout ->
    try Sexp.of_string_conv_exn stdout Reaction.t_of_sexp
    with exn ->
      raise_s [%sexp "line count process parse sexp error"
                   , (input  : Action.t)
                   , (stdout : string)
                   , (exn    : Exn.t)]
;;
