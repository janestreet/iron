
open! Core.Std
open! Import

module Action : sig
  type t =
    { what_feature : Maybe_archived_feature_spec.t
    ; what_diff    : What_diff.t
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t =
    { feature_path     : Feature_path.t
    ; feature_id       : Feature_id.t
    ; is_archived      : bool
    ; diffs            : Diff2s.t Or_error.t Or_pending.t
    (** The reaction [reviewer] is the resolution of [what_diff] in the context of the
        information known by the server about the feature and its attributes. *)
    ; reviewer         : Reviewer.t
    ; base             : Rev.t
    ; tip              : Rev.t
    ; remote_rev_zero  : Rev.t
    ; remote_repo_path : Remote_repo_path.t
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
