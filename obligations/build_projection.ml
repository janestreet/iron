module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module Scrutiny = Scrutiny.Stable

  module V2 = struct
    module Unshared = struct
      type t =
        { name                    : Build_projection_name.V1.t
        ; default_scrutiny        : Scrutiny.V2.t
        ; require_low_review_file : bool
        }
      [@@deriving bin_io, compare, fields, sexp]

      let module_name = "Iron_obligations.Build_projection"
      let hash (t:t) = Hashtbl.hash t
    end
    include Unshared
    include Hash_consing.Make_stable_public (Unshared) ()

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 9f0b4ca9edb13b76386ca9e2338d7118 |}]
    ;;
  end

  module Model = V2
end

open! Core
open! Import

type t = Stable.Model.t =
  { name                    : Build_projection_name.t
  ; default_scrutiny        : Scrutiny.t
  ; require_low_review_file : bool
  }
[@@deriving compare, fields, sexp_of]

let shared_t = Stable.Model.shared_t

let create ~name ~default_scrutiny ~require_low_review_file =
  shared_t
    { name
    ; default_scrutiny
    ; require_low_review_file
    }
;;
