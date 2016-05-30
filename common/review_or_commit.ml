module Stable = struct
  open Core.Stable
  module V1 = struct
    type t =
      | Num of int
      | Commit
    [@@deriving bin_io, compare, sexp]
    let to_model m = m
    let of_model m = m
  end
  module Model = V1
end

open! Core.Std
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
