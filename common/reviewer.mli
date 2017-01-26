open! Core
open! Import

type t =
  { user_name                 : User_name.t
  ; is_whole_feature_follower : bool
  ; is_whole_feature_reviewer : bool
  }
[@@deriving compare, fields, sexp_of]

include Invariant.S with type t := t
include Equal.S     with type t := t

val create :
  User_name.t
  -> is_whole_feature_follower:bool
  -> is_whole_feature_reviewer:bool
  -> t

val synthetic_whole_feature_reviewer : t

module Stable : sig
  module Model : T with type t = t
  module V2 : Stable_without_comparator with type t = Model.t
  module V1 : sig
    type t =
      | Normal_reviewer of User_name.t
      | Whole_feature_reviewer
      | Whole_feature_reviewer_plus_ignored
    include Stable_without_comparator with type t := t
  end
end
