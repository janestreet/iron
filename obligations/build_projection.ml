module Stable = struct
  open! Core.Stable
  open! Import_stable

  module Scrutiny = Scrutiny.Stable

  module V1 = struct
    module Unshared = struct
      type t =
        { name                    : Build_projection_name.V1.t
        ; default_scrutiny        : Scrutiny.V1.t
        ; require_low_review_file : bool
        }
      [@@deriving bin_io, compare, fields, sexp]

      let module_name = "Iron_obligations.Build_projection"
      let hash (t:t) = Hashtbl.hash t
    end
    include Unshared
    include Hash_consing.Make_stable_public (Unshared) ()
  end

  module Model = V1
end

open! Core.Std
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
