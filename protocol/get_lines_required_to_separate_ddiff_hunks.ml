module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = Unit
    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t = int Feature_name.V1.Map.t [@@deriving bin_io, sexp]
      let of_model m = m
    end
    module Model = V1
  end
end

open! Core.Std
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "get-lines-required-to-separate-ddiff-hunks" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action         = Stable.Action.         Model
module Reaction       = Stable.Reaction.       Model
