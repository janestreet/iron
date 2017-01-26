open! Core
open! Import

module Row : sig
  type 'a t =
    { feature_path  : 'a
    ; other_columns : string array
    }
  [@@deriving fields, sexp_of]
end

module Action : sig
  type t =
    { headers                 : string Row.t
    ; data                    : Iron.Feature_path.t Row.t list
    ; preserve_input_ordering : bool
    ; display_ascii           : bool
    ; max_output_columns      : int
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t = string [@@deriving sexp_of]
end

include Iron_command_rpc.S
  with type action = Action.t
   and type reaction = Reaction.t
