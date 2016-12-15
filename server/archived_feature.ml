module Stable = struct
  open Core.Stable
  open Import_stable

  module V1 = struct
    type t =
      { feature_id   : Feature_id.V1.t
      ; feature_path : Feature_path.V1.t
      ; rev_zero     : Rev.V1.t
      ; owners       : User_name.V1.t list
      ; archived_at  : Time.V1_round_trippable.t
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| c2edcb70f8cbc38682589a9ded8ed28c |}]
    ;;
  end
end

open! Core.Std
open! Import

include Stable.V1

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~feature_id:(check Feature_id.invariant)
      ~feature_path:(check Feature_path.invariant)
      ~rev_zero:(check Rev.invariant)
      ~owners:(check (fun owners -> List.iter owners ~f:User_name.invariant))
      ~archived_at:ignore)
;;

let create feature ~archived_at =
  { feature_id   = Feature.feature_id feature
  ; feature_path = Feature.feature_path feature
  ; rev_zero     = Feature.rev_zero feature
  ; owners       = Feature.owners feature
  ; archived_at
  }
;;

let to_list_protocol { feature_id
                     ; feature_path
                     ; owners
                     ; archived_at
                     ; _
                     } =
  { Iron_protocol.List_features.Reaction.
    feature_path
  ; feature_id
  ; owners
  ; review_is_enabled = false
  ; num_lines         = Known (error_string "no num lines for archived features")
  ; next_steps        = []
  ; status            = `Was_archived_at archived_at
  }
;;
