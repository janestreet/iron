open! Core
open! Import

module Clear : sig
  module Action : sig
    type t =
      { metric_name    : Metric_name.t
      ; which_features : Which_features.t
      }
    [@@deriving fields, sexp_of]
  end

  module Reaction : Unit

  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end

module Get : sig
  module Action : sig
    type t = { descendants_of : Which_ancestor.t }
    [@@deriving sexp_of]
  end

  module Reaction : sig
    type t = Metric.Data_point.t list Metric_name.Map.t Feature_path.Map.t
    [@@deriving sexp_of]
  end

  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end

module Add_values : sig
  module Action : sig
    type t =
      { feature_path : Feature_path.t
      ; metric_name  : Metric_name.t
      ; values       : float list
      }
    [@@deriving fields, sexp_of]
  end

  module Reaction : Unit

  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end
