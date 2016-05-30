module Stable = struct
  open Core.Stable
  module V1 = struct
    type t = bool
    [@@deriving bin_io, compare, sexp]
  end
end

open Core.Std
open! Import

include Bool
