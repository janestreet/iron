module Stable = struct
  open! Core.Core_stable

  module Property = Property.Stable

  module V1 = struct
    type t = Sexp.t Property.V1.Map.t [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| e487ad4cb165823e620d0b83bbc238ee |}]
    ;;
  end
end

open! Core
open! Import

type t = Sexp.t Property.Map.t [@@deriving sexp_of]

let invariant t = ignore (t : t)
;;

let empty = Property.Map.empty
;;

let to_rows t =
  Map.to_alist t
  |> List.map ~f:(fun (key, value) -> (key, Sexp.to_string value))
;;
