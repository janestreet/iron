open! Core
open! Import

module Action : sig
  type t =
    { descendants_of : Which_ancestor.t
    ; depth          : int
    ; use_archived   : bool
    }
  [@@deriving fields, sexp_of]
end

module Status : sig
  type t =
    | Existing
    | Archived of
        { archived_at          : Time.t
        ; reason_for_archiving : string
        }
  [@@deriving sexp_of]
end

module Reaction : sig
  type one =
    { feature_path      : Feature_path.t
    ; feature_id        : Feature_id.t
    ; owners            : User_name.t list
    ; review_is_enabled : bool
    ; num_lines         : int Or_error.t Or_pending.t
    ; next_steps        : Next_step.t list
    ; status            : Status.t
    }
  type t = one list
  [@@deriving sexp_of]

  module Stable : sig
    module V10 : sig
      type nonrec t = t [@@deriving bin_io, sexp]
    end
    module V9 : sig
      type t [@@deriving bin_io]
      val to_v10 : t -> V10.t
    end
    module V8 : sig
      type t [@@deriving bin_io]
      val to_v9 : t -> V9.t
    end
  end
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
