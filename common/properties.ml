module Stable = struct
  open! Core.Stable

  module Property = Property.Stable

  module V1 = struct
    type t = Sexp.t Property.V1.Map.t [@@deriving bin_io, compare, sexp]
  end
end

open! Core.Std
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
