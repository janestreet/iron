module Stable = struct
  module User_name = User_name.Stable
  module V1 = struct
    type t =
      [ `All
      | `Whole_feature_reviewers
      | `Only    of User_name.V1.Set.t
      | `All_but of User_name.V1.Set.t
      ]
    [@@deriving bin_io, compare, sexp]
  end
end

open! Core.Std
open! Import

include Stable.V1

let equal t1 t2 = compare t1 t2 = 0

let normalize t ~whole_feature_reviewers =
  match t with
  | `All | `Whole_feature_reviewers -> t
  | `All_but forbidden -> if Set.is_empty forbidden then `All else t
  | `Only permitted ->
    if Set.equal permitted whole_feature_reviewers
    then `Whole_feature_reviewers
    else t
;;

let add t users ~whole_feature_reviewers =
  let t =
    match t with
    | `All -> `All
    | `Whole_feature_reviewers -> `Only (Set.union users whole_feature_reviewers)
    | `Only permitted          -> `Only (Set.union users permitted)
    | `All_but forbidden       -> `All_but (Set.diff forbidden users)
  in
  normalize t ~whole_feature_reviewers
;;

let mem t user ~whole_feature_reviewers ~whole_feature_followers =
  match t with
  | `All                     -> true
  | `Whole_feature_reviewers ->
    User_name.Set.mem whole_feature_reviewers user
    || User_name.Set.mem whole_feature_followers user
  | `Only permitted          ->
    Set.mem permitted user
    || (User_name.Set.mem whole_feature_followers user
        && Set.subset whole_feature_reviewers permitted)
  | `All_but forbidden       -> not (Set.mem forbidden user)
;;

let to_sexp_hum = function
  | `All                     -> "all" |> [%sexp_of: string]
  | `Whole_feature_reviewers -> "whole-feature reviewers" |> [%sexp_of: string]
  | `Only permitted          -> Set.to_list permitted |> [%sexp_of: User_name.t list]
  | `All_but forbidden       -> ( "all but"
                                , Set.to_list forbidden |> [%sexp_of: User_name.t list]
                                ) |> [%sexp_of: string * Sexp.t]
;;
