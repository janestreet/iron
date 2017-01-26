(** Circumstances in which a user name can appear in Iron server's state. *)

open! Core
open! Import

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
[@@deriving sexp_of]

include Comparable.S with type t := t

val to_string_hum : t -> string

module Stable : sig
  module Model : T with type t = t
  module V3 : sig
    include Stable_without_comparator with type t = Model.t
    val of_model : Model.t -> t
    val to_model : t -> Model.t
  end
  module V2 : sig
    include Stable_without_comparator
    val of_model : Model.t -> t Or_error.t
    val to_model : t -> Model.t
  end
  module V1 : sig
    include Stable_without_comparator
    val of_model : Model.t -> t Or_error.t
    val to_model : t -> Model.t
  end
end
