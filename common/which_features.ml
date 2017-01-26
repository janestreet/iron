module Stable = struct
  open! Core.Core_stable

  module Feature_path = Feature_path.Stable

  module Feature = struct
    module V1 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; include_descendants : bool
        }
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 03fea758080b0eeeed1a8f5574ead42f |}]
      ;;
    end

    module Model = V1
  end

  module V1 = struct
    type t =
      | All_features
      | Features of Feature.V1.t list
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 708322a1129c574d2b37636504492ecb |}]
    ;;

    let to_model t = t
  end

  module Model = V1
end

module Feature = Stable.Feature.Model

include Stable.Model

open! Core
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
