open! Core
open! Import

(** Displays an arbitrary list of feature the same way [fe list] would. *)
module Table : sig
  module Action : sig
    type t =
      { features                    : Iron_protocol.List_features.Reaction.t
      ; preserve_input_ordering     : bool
      ; display_ascii               : bool
      ; max_output_columns          : int
      }
    [@@deriving sexp_of]
  end

  module Reaction : sig
    type t = string [@@deriving sexp_of]
  end

  include Iron_command_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end
