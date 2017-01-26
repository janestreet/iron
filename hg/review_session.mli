(** Keeps track of a user's review of a review goal. *)

open! Core
open! Import

module Diff4_in_session : sig

  module Id : sig
    type t [@@deriving sexp_of]
    include Hashable.S   with type t := t
    val to_string : t -> string
    val arg_type : t Command.Spec.Arg_type.t
  end

  type t [@@deriving sexp_of]

  (** Accessors. *)
  val diff4       : t -> Diff4.t
  val id          : t -> Id.t
  val is_reviewed : t -> bool

  val num_lines          : t -> Reviewer.t -> int
  val path_in_repo_at_f2 : t -> Path_in_repo.t

  val compare_by_path_in_repo_at_f2_for_review : t -> t -> int

  val unreviewed_copy : t -> t

  module And_review_kind : sig
    type diff4_in_session

    (** This type is built by the review manager and sent along with the session to the
        client just as a hint to allow the reviewer to prioritize and handle their session
        in a more clever way.  (e.g. seeing diff4s that are not required to review, or
        just to be followed, etc.).

        We freeze the review_kind of diff4s as of the moment they are reviewed, this to
        avoid needless review_kind flickering of reviewed diff4s in sessions.  *)
    type t =
      { diff4_in_session : diff4_in_session
      ; review_kind      : Review_kind.t
      }
    [@@deriving fields, sexp_of]
  end with type diff4_in_session := t
end

type t [@@deriving sexp_of]

include Invariant.S  with type t := t

val deserializer
  : (user_name: User_name.t
     -> feature_cache_invalidator : Cached.Invalidator.t
     -> dynamic_upgrade_state : Dynamic_upgrade.State.t
     -> t
    ) Deserializer.t

(** Accessors. *)
val base            : t -> Rev.t
val diff4s          : t -> Diff4s.t
val id              : t -> Session_id.t
val reviewed_diff4s : t -> Diff4s.t
val reviewer        : t -> Reviewer.t
val tip             : t -> Rev.t

val diff4s_and_review_kind_in_session_not_implicitly_reviewed
  : t
  -> compute_review_kind:( Diff4.t -> Review_kind.t )
  -> Diff4_in_session.And_review_kind.t array
val diff4s_in_session_not_implicitly_reviewed : t -> Diff4_in_session.t array
val not_reviewed_diff4s : t -> Diff4s.t

val create
  :  serializer_dir_of_id          : (Session_id.t -> Relpath.t)
  -> reviewer                      : Reviewer.t
  -> diff4s                        : Diff4s.t
  -> tip                           : Rev.t
  -> base                          : Rev.t
  -> feature_cache_invalidator     : Cached.Invalidator.t
  -> dynamic_upgrade_state         : Dynamic_upgrade.State.t
  -> Serializer.t
  -> t

val have_done_some_review : t -> bool

val all_diff4s_are_reviewed : t -> bool

val set_is_locked
  : t
  -> _ Query.t
  -> bool
  -> unit

val is_locked : t -> bool

val reviewed
  : t
  -> _ Query.t
  -> Session_id.t
  -> Diff4_in_session.Id.t list
  -> compute_review_kind:( Diff4.t -> Review_kind.t )
  -> is_using_locked_sessions:bool
  -> even_if_some_files_are_already_reviewed:bool
  -> unit Or_error.t

val unreviewed
  : t
  -> _ Query.t
  -> Session_id.t
  -> Diff4_in_session.Id.t list
  -> unit Or_error.t

val num_lines_completed : t -> int

val set_to_nothing_reviewed : t -> _ Query.t -> Session_id.t -> unit Or_error.t

module Stable : sig
  module Diff4_in_session : sig
    module Id : sig
      module V1 : Stable_without_comparator with type t = Diff4_in_session.Id.t
    end
    module V2 : Stable_without_comparator with type t = Diff4_in_session.t
    module And_review_kind : sig
      module V2 : Stable_without_comparator with type t = Diff4_in_session.And_review_kind.t
    end
  end
end
