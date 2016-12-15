module Stable = struct
  open! Core.Stable

  module Feature_path = Feature_path.Stable
  module Unclean_workspace_reason = Unclean_workspace_reason.Stable

  module V1 = struct
    type t =
      { feature_path : Feature_path.V1.t
      ; reason       : Unclean_workspace_reason.V1.t
      }
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 4aae2be975e767394fbdae0cef96f58c |}]
    ;;

    let of_model m = m
  end

  module Model = V1
end

open! Core.Std
open! Import

type t = Stable.Model.t =
  { feature_path : Feature_path.t
  ; reason       : Unclean_workspace_reason.t
  }
[@@deriving compare, fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~feature_path:(check Feature_path.invariant)
      ~reason:(check Unclean_workspace_reason.invariant))
;;
