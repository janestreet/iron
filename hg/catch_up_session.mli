(** A catch-up session is a review session that keeps track of reviews done on behalf of
    a reviewer by other users.  It maintains a set of diff4s that the reviewer needs to
    catch up on. *)

open! Core
open! Import

module Id_and_kind : sig
  type t =
    { id   : Diff4_in_session.Id.t
    ; kind : Catch_up_kind.t
    }
  [@@deriving sexp_of]
end

module Diff4_to_catch_up : sig
  type t
  val id                 : t -> Diff4_in_session.Id.t
  val diff4_in_session   : t -> Review_session.Diff4_in_session.t
  val kind               : t -> Catch_up_kind.t
  val num_lines          : t -> Reviewer.t -> int
  val path_in_repo_at_f2 : t -> Path_in_repo.t
end

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val deserializer : (user_name: User_name.t -> t) Deserializer.t

val create
  :  review_session            : Review_session.t
  -> remote_rev_zero           : Rev.t
  -> remote_repo_path          : Remote_repo_path.t
  -> feature_path              : Feature_path.t
  -> feature_id                : Feature_id.t
  -> whole_feature_reviewers   : User_name.Set.t
  -> owners                    : User_name.t list
  -> base                      : Rev.t
  -> tip                       : Rev.t
  -> description               : string
  -> is_permanent              : bool
  -> seconder                  : User_name.t option
  -> Serializer.t
  -> t

(** [reviewed t query ids] expresses that [Query.by query] reviewed [ids], and adds the
    corresponding diff4s to the catch-up set. *)
val reviewed
  :  t
  -> _ Query.t
  -> Id_and_kind.t list
  -> unit Or_error.t

(** [catch_up t query ids] expresses that [Query.by query] reviewed [ids], and removes
    the corresponding diff4s from the catch-up set. *)
val catch_up
  :  t
  -> _ Query.t
  -> Diff4_in_session.Id.t list
  -> unit Or_error.t

(** [Creation.t] holds immutable attributes of the catch-up session at the time it was
    created.  The feature, may have been subsequently mutated; those changes are not
    reflected in the catch-up session. *)
module Creation : sig
  type t

  val session_id                : t -> Session_id.t
  val session_tip               : t -> Rev.t
  val diff4s_in_session         : t -> Review_session.Diff4_in_session.t array
  val remote_rev_zero           : t -> Rev.t
  val remote_repo_path          : t -> Remote_repo_path.t
  val feature_id                : t -> Feature_id.t
  val whole_feature_reviewers   : t -> User_name.Set.t
  val owners                    : t -> User_name.t list
  val base                      : t -> Rev.t
  val tip                       : t -> Rev.t
  val description               : t -> string
  val is_permanent              : t -> bool
  val seconder                  : t -> User_name.t option
end

(** Accessors, with direct access for the most common fields. *)
val id                  : t -> Session_id.t
val creation            : t -> Creation.t
val creation_time       : t -> Time.t
val feature_id          : t -> Feature_id.t
val reviewer            : t -> Reviewer.t

(** Accessors to mutable fields. *)
val all_diff4s_are_caught_up         : t -> bool
val diff4s_to_catch_up               : t -> Diff4_to_catch_up.t list
val feature_path                     : t -> Feature_path.t
val line_count_remaining_to_catch_up : t -> Line_count.Catch_up.t

val set_feature_path : t -> _ Query.t -> Feature_path.t -> unit

module Stable : sig
  module Diff4_to_catch_up : sig
    module Model : T with type t = Diff4_to_catch_up.t
    module V3 : sig
      type t = Model.t [@@deriving bin_io, compare, sexp]
    end
    module V2 : sig
      type t [@@deriving bin_io]
      val of_v3 : V3.t -> t
      val to_v3 : t -> V3.t
    end
  end
end
