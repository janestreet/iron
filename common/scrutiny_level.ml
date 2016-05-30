module Stable = struct
  open Core.Stable

  module V1 = struct
    type t = int [@@deriving bin_io, compare, sexp]

    open Core.Std

    let invariant t =
      if t < 0 || t > 100
      then failwiths "a scrutiny level must be in [0,100]" t [%sexp_of: int];
    ;;

    let t_of_sexp sexp =
      try
        let t = t_of_sexp sexp in
        invariant t;
        t
      with exn -> Sexplib.Conv.of_sexp_error_exn exn sexp
    ;;

    let hash = Int.hash
  end
end

open! Core.Std
open! Import

module Syntax = Stable.V1

include Stable.V1
include Comparable.Make (Stable.V1)

let ignored = 0

let of_int n = invariant n; n
let to_int n = n

let hash = Int.hash

let to_string_hum n = Int.to_string_hum n
