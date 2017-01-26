module Stable = struct
  open! Import_stable

  module Rev = Rev.Stable

  module Update_expected = struct
    module V1 = struct
      type t =
        { rev            : Rev.V1.t
        ; by             : User_name.V1.t
        ; expected_since : Time.V1_round_trippable.t
        }
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 3253ba947387eb40e50fa322f36b1382 |}]
      ;;
    end

    module Model = V1
  end

  module V1 = struct
    type t =
      | No_update_expected
      | Update_expected of Update_expected.V1.t
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 280d5b0b316b760b790abb0658184795 |}]
    ;;
  end

  module Model = V1
end

open! Core
open! Import

module Update_expected = struct
  type t = Stable.Update_expected.Model.t =
    { rev            : Rev.t
    ; by             : User_name.t
    ; expected_since : Time.t (* has shorter sexp_of than [Time.V1_round_trippable.t] *)
    }
  [@@deriving fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~rev:(check Rev.invariant)
        ~by:(check User_name.invariant)
        ~expected_since:ignore)
  ;;
end

type t = Stable.Model.t =
  | No_update_expected
  | Update_expected of Update_expected.t
[@@deriving sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    match t with
    | No_update_expected -> ()
    | Update_expected update_expected -> Update_expected.invariant update_expected)
;;
