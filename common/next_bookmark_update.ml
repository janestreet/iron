module Stable = struct
  open Core.Core_stable

  module Iron_time = Iron_time.Stable

  module V1 = struct
    type t =
      | Update_expected_since of Iron_time.V1_round_trippable.t
      | No_update_expected
      | No_update_expected_due_to_iron_bug of Error.V1.t
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 557b27b5105e2c0b36dda475e0f924e8 |}]
    ;;
  end
end

open! Core
open! Import

type t = Stable.V1.t =
  | Update_expected_since of Time.t
  | No_update_expected
  | No_update_expected_due_to_iron_bug of Error.t
[@@deriving compare, sexp_of]

let am_expecting_bookmark_update = function
  | Update_expected_since _ -> true
  | No_update_expected | No_update_expected_due_to_iron_bug _ -> false
;;

let to_or_error_or_pending : t -> unit Or_error.t Or_pending.t = function
  | Update_expected_since time               -> Pending_since time
  | No_update_expected                       -> Known (Ok ())
  | No_update_expected_due_to_iron_bug error -> Known (Error error)
;;

let same_variant t1 t2 =
  match t1, t2 with
  | Update_expected_since _             , Update_expected_since _
  | No_update_expected                  , No_update_expected
  | No_update_expected_due_to_iron_bug _, No_update_expected_due_to_iron_bug _ ->
    true
  | (Update_expected_since _
    | No_update_expected
    | No_update_expected_due_to_iron_bug _), _
    -> false
;;

let is_transition_to_update_expected ~from ~to_ =
  not (am_expecting_bookmark_update from) && am_expecting_bookmark_update to_
;;
