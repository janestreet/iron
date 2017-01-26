open! Core
open! Import

module Action : sig
  type t =
    | Feature_path of Feature_path.t
    | Metric_name  of Metric_name.t
  [@@deriving sexp_of]
end

module Reaction : sig
  type t =
    { feature_path : Feature_path.t
    ; metric_name  : Metric_name.t
    ; value        : float
    ; added_at     : Time.t
    }
  [@@deriving fields, sexp_of]
end

include Iron_versioned_rpc.S_pipe_rpc
  with type action   = Action.t
  with type reaction = Reaction.t
