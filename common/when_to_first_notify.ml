module Stable = struct
  module V1 = struct
    type t =
      | Now
      | At_next_change
    [@@deriving bin_io, compare, enumerate, sexp]
  end
end

open! Core.Std
open! Import

module T = Stable.V1
include T
include Comparable.Make (T)
