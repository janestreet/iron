module Stable = struct
  open! Core.Stable

  module V1 = struct
    type one_reason =
      | Uncommitted_changes
      | Unpushed_changesets
      | Unsatisfied_invariant
      | Error of Error.V1.t
    [@@deriving bin_io, compare, sexp]

    type t = one_reason list
    [@@deriving bin_io, compare, sexp]

    let of_model m = m
  end

  module Model = V1
end

open! Core.Std
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
