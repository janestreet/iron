open! Core
open! Import

module Action : sig
  type t =
    { feature_path      : Feature_path.t
    ; even_though_empty : bool
    ; even_though_owner : bool
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t =
    { whole_feature_review_remaining : (User_name.t * Line_count.t) list
    ; cr_summary                     : Cr_comment.Summary.t
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
