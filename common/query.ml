module Stable = struct
  open! Core.Core_stable
  module Time = Import.Time.Stable
  module User_name = User_name.Stable
  module V1 = struct

    type 'a t =
      { uuid               : Uuid.V1.t
      ; by                 : User_name.V1.t
      ; at                 : Time.V1_round_trippable.t
      ; hostname           : string
      ; machine_zone       : Core.Core_stable.Time.Zone.V1.t
      ; executable         : string
      ; executable_version : string
      (* [action] is last because it might be big, and the sexp is easier to read with
         the small stuff at the front. *)
      ; action             : 'a
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline ([%bin_digest: Bin_digest_type_variable.tick_a t]);
      [%expect {| 9508c0333c154eb22fe310b182cb93a6 |}]
    ;;

    let map t ~f = { t with action = f t.action }

  end
end

open! Core
open! Import

include Stable.V1

let invariant invariant_action t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~uuid:(check Uuid.invariant)
      ~by:(check User_name.invariant)
      ~at:ignore
      ~machine_zone:ignore
      ~hostname:ignore
      ~executable:ignore
      ~executable_version:ignore
      ~action:(check invariant_action))
;;

let with_action t action = { t with action }

let hostname = Unix.gethostname ()

let executable = Sys.argv.(0)

let create ?(by = User_name.unix_login) ?(at = Time.now ()) action =
  { uuid               = Uuid.create ()
  ; by
  ; at
  ; hostname
  ; machine_zone       = Core.(force Time.Zone.local)
  ; executable
  ; executable_version = Version_util.version
  ; action
  }
;;
