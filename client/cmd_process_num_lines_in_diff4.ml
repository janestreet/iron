open! Core.Std
open! Async.Std
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
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       let stdin  = Lazy.force Reader.stdin in
       let stdout = Lazy.force Writer.stdout in
       match%bind Reader.read_sexp stdin with
       | `Eof -> failwith "Eof. sexp expected"
       | `Ok sexp ->
         let%bind () =
           let%map v = compute (Action.t_of_sexp sexp) in
           v
           |> Reaction.sexp_of_t
           |> Writer.write_sexp stdout
         in
         Writer.newline stdout;
         Writer.flushed stdout
    )
;;
