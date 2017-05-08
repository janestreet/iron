module Stable = struct
  open Core.Core_stable

  module V1 = struct
    type t = int [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 698cfa4093fe5e51523842d37b92aeac |}]
    ;;

    open Core

    let invariant t =
      if t < 0 || t > 100
      then raise_s [%sexp "a scrutiny level must be in [0,100]", (t : int)];
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

open! Core
open! Import

module Syntax = Stable.V1

include Stable.V1
include Comparable.Make (Stable.V1)

let ignored = 0

let of_int n = invariant n; n
let to_int n = n

let hash = Int.hash

let to_string_hum n = Int.to_string_hum n
