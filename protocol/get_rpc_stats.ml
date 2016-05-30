module Stable = struct

  open Import_stable

  module Action = struct
    module V1 = Unit

    module Model = V1
  end

  module Key = struct

    module V1 = struct
      type t =
        { by          : User_name.V1.t
        ; rpc_name    : string
        ; rpc_version : int
        }
      [@@deriving bin_io, compare, fields, sexp]
    end

    module Model = V1
  end

  module Data = struct

    module V1 = struct
      type t =
        { hits : int
        ; took : Span.V1.t
        }
      [@@deriving bin_io, fields, sexp]
    end

    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t = (Key.V1.t * Data.V1.t) list
      [@@deriving bin_io, sexp]

      let of_model m = m
    end

    module Model = V1
  end
end

open! Core.Std
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "get-rpc-stats" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action       = Stable.Action.  Model
module Data         = Stable.Data.    Model
module Key          = Stable.Key.     Model
module Reaction     = Stable.Reaction.Model
