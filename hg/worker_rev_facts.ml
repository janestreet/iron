module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module Cr_comment = Cr_comment. Stable
  module Cr_soon    = Cr_soon.    Stable
  module Rev_facts  = Rev_facts.  Stable

  module V1 = struct
    type t =
      { rev_facts   : Rev_facts.V1.t
      ; crs         : Cr_comment.V1.t list Or_error.V1.t
      ; cr_soons    : Cr_soon.V1.t    list Or_error.V1.t
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 45d77a3a5c234746241671a6b6498f22 |}]
    ;;
  end
  module Model = V1
end

open! Core
open! Import

include Stable.Model

let invariant = ignore_worker_invariant_on_server

let for_sorted_output { rev_facts; crs; cr_soons } =
  { rev_facts
  ; crs      = Or_error.map crs      ~f:(List.sort ~cmp:Cr_comment.For_sorted_output.compare)
  ; cr_soons = Or_error.map cr_soons ~f:(List.sort ~cmp:Cr_soon.For_sorted_output.compare)
  }
;;

module Concise = struct
  type nonrec t = t

  let sexp_of_crs_count_or_error crs =
    (match crs with
     | Error _ -> "<error>"
     | Ok list -> sprintf "%s items" (Int.to_string_hum (List.length list)))
    |> [%sexp_of: string]
  ;;

  let sexp_of_t
        { rev_facts
        ; crs
        ; cr_soons
        } =
    [%sexp
      { rev_facts : Rev_facts.t
      ; crs       : crs_count_or_error
      ; cr_soons  : crs_count_or_error
      }
    ]
  ;;

end

module On_server = struct
  module Crs_consing = struct

    module type X = sig
      type t [@@deriving sexp_of]
      module Structurally_compared : sig
        type nonrec t = t [@@deriving compare, sexp_of]
      end
      val path : t -> Relpath.t
    end

    let of_stable (type a) (module X : X with type t = a) (crs : a list Or_error.t) ~f =
      Or_error.map crs ~f:(fun crs ->
        let by_file = Path_in_repo.Table.create () in
        List.iter crs ~f:(fun cr ->
          let key = Path_in_repo.of_relpath (X.path cr) in
          Hashtbl.add_multi by_file ~key ~data:cr);
        Hashtbl.fold by_file ~init:[] ~f:(fun ~key ~data alist ->
          let deterministic_data = List.sort ~cmp:X.Structurally_compared.compare data in
          (key, deterministic_data) :: alist)
        |> f
      )
    ;;

    let to_stable consing ~f:fold =
      Or_error.map consing ~f:(fun consing ->
        fold consing ~init:[] ~f:(fun ~key:_path_in_repo ~data:crs acc ->
          List.rev_append crs acc))
    ;;
  end

  type t =
    { rev_facts   : Rev_facts.t
    ; crs         : File_tree_consing.Cr_comments.t Or_error.t
    ; cr_soons    : File_tree_consing.Cr_soons.t    Or_error.t
    }
  [@@deriving fields, sexp_of]

  let of_stable { Stable.V1. rev_facts; crs; cr_soons } =
    { rev_facts
    ; crs
      = Crs_consing.of_stable (module Cr_comment) crs
          ~f:File_tree_consing.Cr_comments.of_alist
    ; cr_soons
      = Crs_consing.of_stable (module Cr_soon) cr_soons
          ~f:File_tree_consing.Cr_soons.of_alist
    }
  ;;

  let to_stable { rev_facts; crs; cr_soons } =
    { Stable.V1.
      rev_facts
    ; crs      = Crs_consing.to_stable crs      ~f:File_tree_consing.Cr_comments.fold
    ; cr_soons = Crs_consing.to_stable cr_soons ~f:File_tree_consing.Cr_soons.fold
    }
  ;;

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~rev_facts:ignore
        ~crs:(check (Or_error.invariant (File_tree_consing.Cr_comments.invariant)))
        ~cr_soons:(check (Or_error.invariant (File_tree_consing.Cr_soons.invariant))))
  ;;
end
