module Stable = struct
  open! Core.Core_stable

  module V2 = struct
    type t = Sexp.t list
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| e7e521c8a42548654d65ed179c902d88 |}];
    ;;
  end

  module V1 = struct
    module One_reason = struct
      type t =
        | Uncommitted_changes
        | Unpushed_changesets
        | Unsatisfied_invariant
        | Error of Error.V1.t
      [@@deriving bin_io, compare, sexp]
    end

    type t = One_reason.t list
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: One_reason.t];
      [%expect {| 176bfd8262a2573b6c2a6f5c1845a1f4 |}];
      print_endline [%bin_digest: t];
      [%expect {| d2a7c7ab6c6b36e6946b39bb54fb3f14 |}];
    ;;

    open! Core
    open! Import

    let of_v2 sexps = List.map sexps ~f:(fun sexp ->
      match (sexp : Sexp.t) with
      | List _ as sexp -> One_reason.Error (Error.create_s sexp)
      | Atom str ->
        match str with
        | "invalid current bookmark" -> Unsatisfied_invariant
        | "uncommitted changes"      -> Uncommitted_changes
        | "unpushed changesets"      -> Unpushed_changesets
        | _ -> One_reason.Error (Error.create_s sexp))
    ;;

    let to_v2 (t : t) = List.map t ~f:(function
      | Uncommitted_changes   -> [%sexp "uncommitted changes"]
      | Unpushed_changesets   -> [%sexp "unpushed changesets"]
      | Unsatisfied_invariant -> [%sexp "invalid current bookmark"]
      | Error err             ->
        match [%sexp (err : Error.t)] with
        | Atom _ as sexp -> sexp
        | List _ as sexp -> [%sexp (Error (sexp : Sexp.t))])
    ;;
  end

  module Model = V2
end

open! Core
open! Import

include Stable.Model

let equal t1 t2 = 0 = compare t1 t2

let check_exn = function
  | [] -> failwith "Unclean_workspace_reason.t is unexpectedly empty"
  | _ :: _ -> ()
;;

let invariant t =
  check_exn t;
;;

let to_token (t : Sexp.t) =
  match t with
  | Atom s -> s
  | List _ -> Sexp.to_string t
;;

let to_string_hum t =
  String.concat ~sep:", " (List.map t ~f:to_token)
;;

let truncated_suffix = " ... (truncated)"
let truncated_suffix_length = String.length truncated_suffix

let to_ascii_table_column_text t =
  let to_short_string s =
    let s = to_token s in
    if String.length s < 90
    then s
    else String.sub s ~pos:0 ~len:(90 - truncated_suffix_length) ^ truncated_suffix
  in
  String.concat ~sep:", " (List.map t ~f:to_short_string)
;;

let dedup_and_sort t =
  t
  |> List.sort ~cmp:Sexp.compare
  |> List.dedup_and_sort ~compare:Sexp.compare
;;

let add t1 t2 = dedup_and_sort (List.rev_append t1 t2)

module One_reason = struct
  type t =
    | Error of Error.t
    | Invalid_current_bookmark
    | Pending_rename
    | Shelved_changes
    | Uncommitted_changes
    | Unpushed_changesets
  [@@deriving sexp_of]

  let to_sexp = function
    | Error err                -> [%sexp (err : Error.t)]
    | Invalid_current_bookmark -> [%sexp "invalid current bookmark"]
    | Pending_rename           -> [%sexp "pending rename"]
    | Shelved_changes          -> [%sexp "shelved changes"]
    | Uncommitted_changes      -> [%sexp "uncommitted changes"]
    | Unpushed_changesets      -> [%sexp "unpushed changesets"]
  ;;
end

let create = function
  | [] -> None
  | (_::_) as t -> Some (dedup_and_sort (List.map t ~f:One_reason.to_sexp))
;;

let error t = [ One_reason.to_sexp (Error t) ]
