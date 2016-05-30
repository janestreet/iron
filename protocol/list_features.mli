open! Core.Std
open! Import

module Action : sig
  type t =
    { feature_path : Feature_path.t option
    ; depth        : int
    ; use_archived : bool
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type one =
    { feature_path      : Feature_path.t
    ; feature_id        : Feature_id.t
    ; owners            : User_name.t list
    ; review_is_enabled : bool
    ; num_lines         : int Or_error.t Or_pending.t
    ; next_steps        : Next_step.t list
    ; status            : [ `Existing | `Was_archived_at of Time.t ]
    }
  type t = one list
  [@@deriving sexp_of]

  module Stable : sig
    module V8 : sig
      type nonrec t = t [@@deriving bin_io, sexp]
    end
  end
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
