module Stable = struct
  module V1 = struct
    type t =
      | My_owners
      | My_owners_and_child_owners
    [@@deriving bin_io, compare, enumerate, sexp]
  end
end

open! Core.Std
open! Import

module T = Stable.V1
include T
include Comparable.Make (T)

let to_string_hum_as_parent = function
  | My_owners -> "my owners"
  | My_owners_and_child_owners -> "my owners and child owners"
;;
