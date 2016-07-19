module Stable = struct

  open! Core.Stable

  module Feature_path = Feature_path.Stable

  module V1 = struct
    type t =
      | Any_root
      | Feature  of Feature_path.V1.t
    [@@deriving bin_io, compare, sexp]

    let to_model m = m
    let of_model m = m
  end

  module Model = V1
end

open! Core.Std
open! Import

include Stable.Model
