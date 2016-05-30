module Stable = struct

  open Import_stable

  module Feature = struct
    module V1 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; include_descendants : bool
        }
      [@@deriving bin_io, compare, sexp]
    end
  end

  module V1 = struct
    type t =
      | All_features
      | Features of Feature.V1.t list
    [@@deriving bin_io, compare, sexp]

    let to_model t = t
  end
end

module Feature = Stable.Feature.V1

include Stable.V1

open! Core.Std
open! Import

let these_features feature_paths =
  Features
    (List.map feature_paths ~f:(fun feature_path ->
       { Feature.
         feature_path
       ; include_descendants = false
       }))
;;

let mem t feature_path =
  match t with
  | All_features -> true
  | Features features ->
    List.exists features ~f:(fun which_feature ->
      Feature_path.equal which_feature.feature_path feature_path
      || (which_feature.include_descendants
          && Feature_path.is_ancestor
               ~ancestor:which_feature.feature_path
               ~descendant:feature_path))
;;
