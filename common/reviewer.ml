module Stable = struct
  open! Core.Core_stable
  module User_name = User_name.Stable
  module V2 = struct
    type t =
      { user_name : User_name.V1.t
      ; is_whole_feature_follower : bool
      ; is_whole_feature_reviewer : bool
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| a4e2e7c93927f169d3243a7fb3c6b5c8 |}]
    ;;
  end
  module Model = V2
  module V1 = struct
    type t =
      | Normal_reviewer of User_name.V1.t
      | Whole_feature_reviewer
      | Whole_feature_reviewer_plus_ignored
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| e26809aa282c539e4b168e5775a9f8fd |}]
    ;;
  end
end

open! Core
open! Import

include Stable.Model

let equal t1 t2 = compare t1 t2 = 0

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~user_name:(check User_name.invariant)
      ~is_whole_feature_follower:ignore
      ~is_whole_feature_reviewer:ignore)
;;

let create user_name = Fields.create ~user_name

let synthetic_whole_feature_reviewer =
  { user_name = User_name.synthetic_whole_feature_reviewer
  ; is_whole_feature_reviewer = true
  ; is_whole_feature_follower = false
  }
;;
