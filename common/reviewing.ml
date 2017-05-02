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

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 777b2a5cb8865307bd89b9a62be7012b |}]
    ;;
  end
end

open! Core
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

let mem t user ~whole_feature_reviewers ~whole_feature_followers ~is_seconded =
  let t =
    match t with
    | ( `All | `All_but _ | `Only _ ) as t -> t
    | `Whole_feature_reviewers -> `Only whole_feature_reviewers
  in
  match t with
  | `All               -> true
  | `All_but forbidden -> not (Set.mem forbidden user)
  | `Only permitted    ->
    Set.mem permitted user
    || (Set.mem whole_feature_followers user
        && (is_seconded || Set.length whole_feature_reviewers > 1)
        && Set.is_subset whole_feature_reviewers ~of_:permitted)
;;

let to_sexp_hum = function
  | `All                     -> "all" |> [%sexp_of: string]
  | `Whole_feature_reviewers -> "whole-feature reviewers" |> [%sexp_of: string]
  | `Only permitted          -> Set.to_list permitted |> [%sexp_of: User_name.t list]
  | `All_but forbidden       -> ( "all but"
                                , Set.to_list forbidden |> [%sexp_of: User_name.t list]
                                ) |> [%sexp_of: string * Sexp.t]
;;
