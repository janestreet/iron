module Stable = struct
  open! Core.Stable
  module User_name = User_name.Stable
  module V2 = struct
    type t =
      { user_name : User_name.V1.t
      ; is_whole_feature_follower : bool
      ; is_whole_feature_reviewer : bool
      }
    [@@deriving bin_io, compare, fields, sexp]
  end
  module Model = V2
  module V1 = struct
    type t =
      | Normal_reviewer of User_name.V1.t
      | Whole_feature_reviewer
      | Whole_feature_reviewer_plus_ignored
    [@@deriving bin_io, compare, sexp]
  end
end

open! Core.Std
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
