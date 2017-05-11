(** File name -- a full path. Paths may include .. elements. **)

open! Core
open! Async
open! Import

type abspath (** For forward reference to Abspath.t from Relpath module. *)

module Relpath : sig

  type t

  include Identifiable with type t := t
  include Invariant.S  with type t := t

  val empty : t (** AKA "dot" *)

  val is_empty : t -> bool

  val of_list : File_name.t list -> t

  val append : t -> t -> t

  val is_prefix : prefix:t -> t -> bool

  val extend : t -> File_name.t -> t

  val split_first : t -> (File_name.t * t) option

  val split_dir_file_exn : t -> t * File_name.t

  val parts : t -> File_name.t list

  (** Drop the last element of the path: /a/b/c -> /a/b. *)
  val parent     : t -> t option
  val parent_exn : t -> t

  val last_exn : t -> File_name.t

  val alphabetic_compare : t -> t -> int

  (** [default_review_compare] is like [alphabetic_compare], but has domain-specific rules
      based on file suffixes, so that e.g. [foo.mli] appears before [foo.ml]. *)
  val default_review_compare : t -> t -> int

  val chop_prefix : prefix:t -> t -> t Or_error.t

  module Stable : sig
    module V1 : sig
      type nonrec t = t
      include Stable_without_comparator with type t := t
      include Stringable.S           with type t := t
    end
  end
end

module Abspath : sig

  type t = abspath

  include Invariant.S  with type t := t
  include Identifiable with type t := t

  val alphabetic_compare : t -> t -> int

  val root : t (** The root path: / *)

  val dev_null : t

  val program_started_in : t Or_error.t

  val append : t -> Relpath.t -> t

  val extend : t -> File_name.t -> t

  val last_exn : t -> File_name.t

  val parent : t -> t option
  val parent_exn : t -> t

  val split_dir_file_exn : t -> t * File_name.t

  val of_list : File_name.t list -> t

  val to_list : t -> File_name.t list

  val chop_prefix : prefix:t -> t                -> Relpath.t Or_error.t
  val chop_suffix : t        -> suffix:Relpath.t -> t         Or_error.t

  (** Simplify the path by attempting to simplify all the ".." parts.  This function is
      purely syntactic -- it does not look at all at the state of the filesystem.
      Example:

      {v
        "/a/b/../c"  -> Ok "a/c"
        "/a/../../b" -> Error _
      v}
  *)
  val simplify_dotdots_syntax : t -> t Or_error.t

  val is_prefix : prefix:t -> t -> bool

  val file_exists_exn : t -> bool Deferred.t

  val rm_rf_exn : t -> unit Deferred.t

  val in_dir : t -> f:(unit -> 'a Deferred.t) -> 'a Deferred.t

  val rename_exn : src:t -> dst__delete_if_exists:t -> unit Deferred.t

  module Stable : sig
    module V1 : Stable_without_comparator with type t = t
  end
end

type t =
  | Abspath of Abspath.t
  | Relpath of Relpath.t
[@@deriving compare]

include Identifiable with type t := t

val alphabetic_compare : t -> t -> int

val root  : t  (** abspath *)

val empty : t  (** relative *)

(** Relocate p2, if it's relative, to p1. If p2 is abspath, just return p2.
    Examples: "a/b"  "c/d"  -> "a/b/c/d"         rel + rel -> rel
    "/a/b" "c/d"  -> "/a/b/c/d"        abs + rel -> abs
    "a/b"  "/c/d" -> "/c/d"            any + abs -> abs
    "/a/b" "/c/d" -> "/c/d"            any + abs -> abs
    "a"    "."    -> "a"               Boundary case
    "."    "a"    -> "a"               Boundary case
*)
val append : t -> t -> t

val is_prefix   : prefix:t -> t -> bool

(** If (1) the prefix & main path are either both Relpath, or both Abspath, and
    (2) the prefix is truly a prefix of the main path,
    then remove the prefix path and return Some of the relative-path remainder.
    Otherwise, None.
    To repeat: when we succeed, answer is always a relative path. *)
val chop_prefix : prefix:t -> t -> t Or_error.t

val with_temp_dir
  :  ?in_dir:Abspath.t (** default to [Filename.temp_dir_name] *)
  -> File_name.t
  -> f:(Abspath.t -> 'a Deferred.t)
  -> 'a Deferred.t

(** Resolve a Path.t to an Abspath.t:
    - If the path is abspath, that's the answer.
    - If the path is relative, append it to [relative_to]
    Does not eliminate .. elements in the path; see [Abspath.simplify_dotdots_syntax]. *)
val resolve : t -> relative_to:Abspath.t -> Abspath.t

val resolve_relative_to_program_started_in : t -> Abspath.t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
