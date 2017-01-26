module Stable = struct
  open! Core.Core_stable

  module V1 = struct
    type one_reason =
      | Uncommitted_changes
      | Unpushed_changesets
      | Unsatisfied_invariant
      | Error of Error.V1.t
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: one_reason];
      [%expect {| 176bfd8262a2573b6c2a6f5c1845a1f4 |}]
    ;;

    type t = one_reason list
    [@@deriving bin_io, compare, sexp]


    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| d2a7c7ab6c6b36e6946b39bb54fb3f14 |}]
    ;;

    let of_model m = m
  end

  module Model = V1
end

open! Core
open! Import

include Stable.Model

module One_reason = struct
  type t = one_reason
  [@@deriving compare]

  let invariant = function
    | Uncommitted_changes
    | Unpushed_changesets
    | Unsatisfied_invariant
    | Error _
      -> ()
  ;;

  let to_string_hum = function
    | Uncommitted_changes   -> "uncommitted changes"
    | Unpushed_changesets   -> "unpushed changesets"
    | Unsatisfied_invariant -> "unsatisfied invariant"
    | Error err -> "error: " ^ Error.to_string_hum err
  ;;

  let to_ascii_table_column_text = function
    | ( Uncommitted_changes
      | Unpushed_changesets
      | Unsatisfied_invariant ) as t -> to_string_hum t
    | Error _ -> "error"
  ;;
end

let equal t1 t2 = 0 = compare t1 t2

let check_exn = function
  | [] -> failwith "Unclean_workspace_reason.t is unexpectedly empty"
  | _ :: _ -> ()
;;

let invariant t =
  List.iter t ~f:One_reason.invariant;
  check_exn t;
;;

let to_string_hum t = String.concat ~sep:", " (List.map t ~f:One_reason.to_string_hum)

let to_ascii_table_column_text t =
  String.concat ~sep:", " (List.map t ~f:One_reason.to_ascii_table_column_text)
;;

let dedup_and_sort t =
  t
  |> List.sort ~cmp:One_reason.compare
  |> List.dedup ~compare:One_reason.compare
;;

let create = function
  | [] -> None
  | (_::_) as t -> Some (dedup_and_sort t)
;;

let add t1 t2 = dedup_and_sort (List.rev_append t1 t2)

let error t = [ Error t ]
