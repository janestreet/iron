module Stable = struct

  open! Core.Core_stable
  module Time = Iron_time.Stable

  module V1 = struct

    module Time = Time.V1_round_trippable

    type 'a t =
      { query_uuid                  : Uuid.V1.t
      ; server_received_query_at    : Time.t
      ; server_computed_reaction_at : Time.t
      ; reaction                    : 'a
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: Bin_digest_type_variable.tick_a t];
      [%expect {| 4088bfc5cd94e1124570ce03001114ae |}]
    ;;

    let map t ~f = { t with reaction = f t.reaction }

  end
end

open! Core
open! Import

include Stable.V1

let invariant invariant_reaction t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~query_uuid:(check Uuid.invariant)
      ~server_received_query_at:ignore
      ~server_computed_reaction_at:ignore
      ~reaction:(check invariant_reaction))
;;

let create reaction ~query_uuid ~server_received_query_at =
  { query_uuid
  ; server_received_query_at
  ; server_computed_reaction_at = Iron_time.now ()
  ; reaction
  }
;;

let server_took t =
  Iron_time.diff t.server_computed_reaction_at t.server_received_query_at
;;

let map t ~f = { t with reaction = f t.reaction }
