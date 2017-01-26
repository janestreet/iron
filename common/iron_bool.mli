open! Core
open! Import

include module type of Bool

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
