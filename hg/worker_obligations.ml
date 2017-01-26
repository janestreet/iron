module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module Obligations_are_valid = Rev_facts.Stable.Obligations_are_valid

  module V5 = struct
    type t =
      { obligations_are_valid : Obligations_are_valid.V1.t
      ; obligations           : Obligations.V5.t Or_error.V2.t
      ; obligations_version   : Obligations_version.V1.t Or_error.V2.t
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 2ad71b49ce7257bfa0a800348c7d4456 |}]
    ;;
  end
  module Model = V5
end

open! Core
open! Import

module Obligations_are_valid = Rev_facts.Obligations_are_valid

type t =
  { obligations_are_valid : Obligations_are_valid.t
  ; obligations           : Obligations.t Or_error.t
  ; obligations_version   : Obligations_version.t Or_error.t
  }
[@@deriving sexp_of]

let of_stable { Stable.Model.
                obligations_are_valid
              ; obligations
              ; obligations_version
              } =
  { obligations_are_valid
  ; obligations           = Or_error.map ~f:Obligations.Stable.V5.to_model obligations
  ; obligations_version
  }
;;

let to_stable { obligations_are_valid
              ; obligations
              ; obligations_version
              } =
  { Stable.Model.
    obligations_are_valid
  ; obligations           = Or_error.map ~f:Obligations.Stable.V5.of_model obligations
  ; obligations_version
  }
;;

module On_server = struct

  module Obligations = struct
    module Obligations_consing = File_tree_consing.Obligations

    type t =
      { obligations_repo : Obligations_repo.t
      ; by_path          : Obligations_consing.t
      }
    [@@deriving fields, sexp_of]

    let invariant t =
      Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
        let check f = Invariant.check_field t f in
        Fields.iter
          ~obligations_repo:ignore_worker_invariant_on_server
          ~by_path:(check Obligations_consing.invariant))
    ;;

    let to_stable { obligations_repo
                  ; by_path
                  } =
      { Obligations.Stable.V5.
        obligations_repo
      ; by_path = Obligations_consing.to_alist by_path
      }
    ;;

    let of_stable { Obligations.Stable.V5.
                    obligations_repo
                  ; by_path
                  } =
      { obligations_repo
      ; by_path = Obligations_consing.of_alist by_path
      }
    ;;
  end

  type t =
    { obligations_are_valid : Obligations_are_valid.t
    ; obligations           : Obligations.t Or_error.t
    ; obligations_version   : Obligations_version.t Or_error.t
    }
  [@@deriving fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~obligations_are_valid:ignore_worker_invariant_on_server
        ~obligations:(check (Or_error.invariant Obligations.invariant))
        ~obligations_version:ignore_worker_invariant_on_server)
  ;;

  let of_stable { Stable.Model.
                  obligations_are_valid
                ; obligations
                ; obligations_version
                } =
    { obligations_are_valid
    ; obligations           = Or_error.map ~f:Obligations.of_stable obligations
    ; obligations_version
    }
  ;;

  let to_stable { obligations_are_valid
                ; obligations
                ; obligations_version
                } =
    { Stable.Model.
      obligations_are_valid
    ; obligations           = Or_error.map ~f:Obligations.to_stable obligations
    ; obligations_version
    }
  ;;

  let dump_obligations t =
    t.obligations
    |> ok_exn
    |> Obligations.to_stable
    |> Iron_obligations.Obligations.Stable.V5.to_model
    |> [%sexp_of: Iron_obligations.Obligations.t]
  ;;
end
