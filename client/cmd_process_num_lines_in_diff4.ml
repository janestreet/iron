open! Core
open! Async
open! Import

module Action = Process_num_lines_in_diff4.Action
module Reaction = Process_num_lines_in_diff4.Reaction

let compute { Action.files; lines_required_to_separate_ddiff_hunks } =
  let%map contents =
    Diamond.Deferred.all (Diamond.map files ~f:Reader.file_contents)
  in
  let num_lines_in_diff4 =
    Pdiff4.Std.Patdiff4.num_lines_to_review
      ~lines_required_to_separate_ddiff_hunks ~contents
  in
  { Reaction.num_lines_in_diff4 }
;;

let command =
  Process_num_lines_in_diff4.internal_group,
  Command.async'
    ~summary:"compute num lines of a diff4"
    (let open Command.Let_syntax in
     let%map_open input = anon ("ACTION-SEXP" %: string) in
     fun () ->
       let open! Deferred.Let_syntax in
       let action = Sexp.of_string_conv_exn (String.strip input) Action.t_of_sexp in
       let%map reaction = compute action in
       Writer.write_sexp (Lazy.force Writer.stdout)
         (Reaction.sexp_of_t reaction) ~terminate_with:Newline;
    )
;;
