module Stable = struct
  open Core.Core_stable
  module V1 = struct
    type t =
      | Num of int
      | Commit
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 1c26b45bd38bbd0513775572150ce6db |}]
    ;;

    let to_model m = m
    let of_model m = m
  end
  module Model = V1
end

open! Core
open! Import

include Stable.Model

let count = function
  | Num i -> i
  | Commit -> 1
;;

let to_string_hum = function
  | Num i  -> Int.to_string_hum i
  | Commit -> "commit session"
;;

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    match t with
    | Num i -> assert (i >= 0)
    | Commit -> ())
;;
