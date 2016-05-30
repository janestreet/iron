module Stable = struct
  open! Core.Stable
  open! Import_stable

  module Diff2s           = Diff2s.           Stable
  module Released_feature = Released_feature. Stable

  module V1 = struct
    type t =
      { released_feature      : Released_feature.V3.t
      ; diff_from_base_to_tip : Diff2s.V2.t
      }
    [@@deriving bin_io, compare, fields, sexp]
    let of_model m = m
    let to_model m = m
  end

  module Model = V1
end

open! Core.Std
open! Async.Std
open! Import

include Stable.Model

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~released_feature:(check Released_feature.invariant)
      ~diff_from_base_to_tip:(check Diff2s.invariant)
  )
;;
