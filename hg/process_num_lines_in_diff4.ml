module Stable = struct
  open! Core.Stable
  open! Iron_common.Stable

  module V2 = struct
    module Action = struct
      type t =
        { files : string Diamond.V1.t
        }
      [@@deriving sexp]
    end

    module Reaction = struct
      type t =
        { num_lines_in_diff4 : int
        }
      [@@deriving sexp]
    end
  end
end

open! Core.Std
open! Async.Std

module Action = Stable.V2.Action
module Reaction = Stable.V2.Reaction

let internal_group = "process-num-lines-in-diff4"

let compute input =
  let prog = Sys.executable_name in
  let args = [ "internal" ; internal_group ] in
  Process.create ~prog ~args ()
  >>| ok_exn
  >>= fun process ->
  let writer = Process.stdin process in
  let input_sexp = Action.sexp_of_t input in
  Writer.write_sexp writer input_sexp;
  Writer.newline writer;
  Writer.flushed writer >>= fun () ->
  Process.collect_stdout_and_wait process
  >>| function
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
