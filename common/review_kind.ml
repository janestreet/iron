module Stable = struct
  open! Core.Core_stable
  module V1 = struct
    type t =
      | Must_review
      | Follow
      | May_review
      | Ownership_change
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| b34f6279a5552fa2ee671694c270a860 |}]
    ;;
    let to_model m = m
    let of_model m = m
  end
  module Model = V1
end

open! Core
open! Import

include Stable.Model

let invariant = function
  | Must_review
  | Follow
  | May_review
  | Ownership_change
    -> ()
;;

let equal t1 t2 = compare t1 t2 = 0

let to_string_hum = function
  | Must_review -> "Required review."
  | Follow -> "\
Follow review.
Your pending review on these changes does not prevent releasability.
These files are shown to you just so you can follow along."
  | May_review -> "\
Optional review.
The review obligations of these changes have been satisfied at this point.
However, you may review them anyway if you desire."
  | Ownership_change -> "Ownership changes."
;;
