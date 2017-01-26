open! Core
open! Import

module Info : sig
  type t =
    { crs_at_tip              : Cr_comment.t list Or_error.t
    ; base_facts              : Rev_facts.t
    ; tip_facts               : Rev_facts.t
    ; base_allow_review_for   : Allow_review_for.t Or_error.t
    ; base_is_ancestor_of_tip : Rev_facts.Is_ancestor.t
    ; diff_from_base_to_tip   : Diff2s.t Or_error.t
    ; diff4s                  : Diff4.t list Or_error.t
    ; cr_soons                : Cr_soons.In_feature.t Or_error.t
    }
  [@@deriving sexp_of]
end

module Action : sig
  type t =
    { feature_path         : Feature_path.t
    ; feature_id           : Feature_id.t
    ; info                 : Info.t Or_error.t
    ; augment_worker_cache : Worker_cache.From_worker_back_to_server.t
    }
  [@@deriving fields, sexp_of]

  module Concise : sig type nonrec t = t [@@deriving sexp_of] end
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
