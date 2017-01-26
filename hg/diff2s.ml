module Stable = struct
  open! Import_stable
  open Core.Core_stable
  module Diff2 = Diff2.Stable
  module V2 = struct
    type t = Diff2.V2.t list
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 6adaad1ab18aad7d705bef2041b3b5c5 |}]
    ;;
  end
end

open Core
open Import

include Stable.V2

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    List.iter t ~f:Diff2.invariant)
;;

let may_reviewers t ~include_file_followers =
  User_name.Set.union_list (List.map t ~f:(fun diff2 ->
    Diff2.may_reviewers diff2 ~include_file_followers))
;;

let num_lines_for_feature_size t =
  List.sum (module Int) t ~f:(fun diff2 ->
    Diff4.num_lines (Diff4.create_from_scratch_to_diff2 diff2)
      Reviewer.synthetic_whole_feature_reviewer)
;;

let restrict_to_may_review_or_follow t reviewer =
  List.filter t ~f:(fun diff ->
    Diff2.may_review diff ~include_may_follow:true reviewer)
;;
