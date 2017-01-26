module Stable = struct
  module V3 = struct
    type t =
      | Catch_up_reviewer
      | Cr_assignee
      | Has_bookmark_without_feature
      | Has_unclean_workspaces
      | Owner
      | Recommended_seconder
      | Reviewer
      | Seconder
      | Whole_feature_follower
      | Whole_feature_reviewer
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 443ef8e6ccc6d956b37c048b0a54ad6f |}]
    ;;

    let of_model m = m
    let to_model m = m
  end
  module Model = V3
  module V2 = struct
    type t =
      | Catch_up_reviewer
      | Cr_assignee
      | Has_bookmark_without_feature
      | Owner
      | Recommended_seconder
      | Reviewer
      | Seconder
      | Whole_feature_follower
      | Whole_feature_reviewer
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 010ade786a77fb1b0091783568e6ee51 |}]
    ;;

    open! Core
    open! Import

    let to_model (t : t) : Model.t =
      match t with
      | Catch_up_reviewer            -> Catch_up_reviewer
      | Cr_assignee                  -> Cr_assignee
      | Has_bookmark_without_feature -> Has_bookmark_without_feature
      | Owner                        -> Owner
      | Recommended_seconder         -> Recommended_seconder
      | Reviewer                     -> Reviewer
      | Seconder                     -> Seconder
      | Whole_feature_follower       -> Whole_feature_follower
      | Whole_feature_reviewer       -> Whole_feature_reviewer
    ;;

    let of_model (m : Model.t) : t Or_error.t =
      Or_error.try_with (fun () ->
        match m with
        | Catch_up_reviewer            -> Catch_up_reviewer
        | Cr_assignee                  -> Cr_assignee
        | Has_bookmark_without_feature -> Has_bookmark_without_feature
        | Owner                        -> Owner
        | Recommended_seconder         -> Recommended_seconder
        | Reviewer                     -> Reviewer
        | Seconder                     -> Seconder
        | Whole_feature_reviewer       -> Whole_feature_reviewer
        | Whole_feature_follower       -> Whole_feature_follower
        | Has_unclean_workspaces ->
          failwith "Has_unclean_workspaces is not supported in User_name_occurrence.V2"
      )
    ;;
  end
  module V1 = struct
    type t =
      | Catch_up_reviewer
      | Cr_assignee
      | Has_bookmark_without_feature
      | Owner
      | Recommended_seconder
      | Reviewer
      | Seconder
      | Whole_feature_reviewer
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| aa2f526a78388dbd62a8216df4567380 |}]
    ;;

    open! Core
    open! Import

    let to_model (t : t) : Model.t =
      match t with
      | Catch_up_reviewer            -> Catch_up_reviewer
      | Cr_assignee                  -> Cr_assignee
      | Has_bookmark_without_feature -> Has_bookmark_without_feature
      | Owner                        -> Owner
      | Recommended_seconder         -> Recommended_seconder
      | Reviewer                     -> Reviewer
      | Seconder                     -> Seconder
      | Whole_feature_reviewer       -> Whole_feature_reviewer
    ;;

    let of_model (m : Model.t) : t Or_error.t =
      Or_error.try_with (fun () ->
        match m with
        | Catch_up_reviewer            -> Catch_up_reviewer
        | Cr_assignee                  -> Cr_assignee
        | Has_bookmark_without_feature -> Has_bookmark_without_feature
        | Owner                        -> Owner
        | Recommended_seconder         -> Recommended_seconder
        | Reviewer                     -> Reviewer
        | Seconder                     -> Seconder
        | Whole_feature_reviewer       -> Whole_feature_reviewer
        | Whole_feature_follower ->
          failwith "Whole_feature_follower is not supported in User_name_occurrence.V1"
        | Has_unclean_workspaces ->
          failwith "Has_unclean_workspaces is not supported in User_name_occurrence.V1"
      )
    ;;
  end
end

open! Core
open! Import

include Stable.Model
include Comparable.Make(struct type nonrec t = t [@@deriving compare, sexp] end)

let to_string_hum = function
  | Catch_up_reviewer            -> "catch-up reviewer"
  | Cr_assignee                  -> "cr assignee"
  | Has_bookmark_without_feature -> "has bookmark without feature"
  | Has_unclean_workspaces       -> "has unclean workspaces"
  | Owner                        -> "owner"
  | Recommended_seconder         -> "recommended seconder"
  | Reviewer                     -> "reviewer"
  | Seconder                     -> "seconder"
  | Whole_feature_follower       -> "whole-feature follower"
  | Whole_feature_reviewer       -> "whole-feature reviewer"
;;
