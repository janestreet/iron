open Core
open! Import

module By_diff2 = Diff2.Ignoring_rev.Table

type t =
  { by_input  : Diff4.t list By_diff2.t
  ; by_output : Diff4.t list By_diff2.t
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    let check_table by =
      check (fun table ->
        Hashtbl.iteri table ~f:(fun ~key:diff2 ~data:diff4s ->
          List.iter diff4s ~f:(fun diff4 ->
            [%test_result: Diff2.Ignoring_rev.t]
              (by diff4 ~num_lines_in_diff:1) ~expect:diff2)));
    in
    Fields.iter
      ~by_input: (check_table Diff4.input)
      ~by_output:(check_table Diff4.output))
;;

let create diff4s =
  let table ~by =
    By_diff2.of_alist_multi (List.map diff4s ~f:(fun a -> (by a ~num_lines_in_diff:1, a)))
  in
  { by_input  = table ~by:Diff4.input
  ; by_output = table ~by:Diff4.output
  }
;;
