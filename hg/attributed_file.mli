open! Core
open! Async
open! Import

module Attributes : sig

  type t = private
    { hash                               : File_contents_hash.t
    ; owner                              : User_name.t
    ; review_obligation                  : Review_obligation.t
    ; followers                          : User_name.Set.t
    ; scrutiny                           : File_scrutiny.t
    ; is_read_by_whole_feature_reviewers : bool
    ; num_lines                          : int
    }
  [@@deriving fields, sexp_of]

  include Invariant.S with type t := t

  val may_review : t -> include_may_follow:bool -> Reviewer.t -> bool
  val may_follow : t -> Reviewer.t -> bool

  val may_reviewers  : t -> include_file_followers:bool -> User_name.Set.t

  val equal_file_contents : t -> t -> bool
end

type t = private
  { path_in_repo    : Path_in_repo.t
  ; rev             : Rev.t
  ; attributes      : [ `Absent | `Present of Attributes.t ]
  }
[@@deriving fields, sexp_of]

include Invariant.S with type t := t

val create
  :  path_in_repo  : Path_in_repo.t
  -> rev           : Rev.t
  -> file          : Abspath.t
  -> Review_attributes.t
  -> t Deferred.t

val with_path_in_repo : t -> Path_in_repo.t -> t

val absent
  :  path_in_repo  : Path_in_repo.t
  -> rev           : Rev.t
  -> t

val file_digest : t -> File_contents_hash.t option

val equal_file_contents : t -> t -> bool

val scrutiny       : t -> File_scrutiny.t
val num_lines      : t -> int

val is_present : t -> bool

val status : src:t -> dst:t -> [ `Added of t | `Removed of t | `Modified of t * t ]

val present_paths_in_repo : t list -> (Rev.t * Path_in_repo.t) list

(** [normalize_paths ~base ~tip] returns [base, tip] that are like the inputs, except with
    the additional invariant that if either is absent, then the absent [t] gets the path
    of the present one.  If both are absent, then [base] takes [tip]'s path. *)
val normalize_paths : base:t -> tip:t -> t * t

val de_alias : t -> User_name_by_alternate_name.t -> t

module Ignoring_rev : sig
  type nonrec t = t
  include Comparable with type t := t
  include Hashable   with type t := t
end

module Stable : sig
  module V2 : Stable_without_comparator with type t = t
end
