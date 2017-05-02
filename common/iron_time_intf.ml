open Core


module Stable = Time_ns.Stable

module type Iron_time = sig

  include module type of Time_ns with module Stable := Time_ns.Stable

  module Stable : sig
    module V1_round_trippable : Stable_without_comparator with type t = t
  end
end
