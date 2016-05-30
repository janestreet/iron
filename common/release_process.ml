module Stable = struct
  module V1 = struct
    type t = Continuous | Direct
    [@@deriving bin_io, compare, enumerate, sexp]
  end
end

open! Core.Std
open! Import

include Stable.V1

let to_string_hum = function
  | Continuous -> "continuous"
  | Direct     -> "direct"
;;

let equal t t' = compare t t' = 0
