open! Core
open! Async
open! Import

(** [num_lines_in_diff] may or may not be the numbers of lines that have to be read.  In
    the presence of scrutiny changes, different things will have to be read.  In the
    presence of renamings, you also have to review some text describing the renaming. *)
type t = private
  { diamond           : Attributed_file.t Diamond.t
  ; errors            : Error.t list
  ; num_lines_in_diff : int
  }
[@@deriving fields, sexp_of]

include Invariant.S with type t := t
include Hashable.S  with type t := t

(** A cache of [Diff4.t]s indexed by [Attributed_file.t Diamond.t]. *)
module Cache : sig
  type t [@@deriving sexp_of]
  val create : unit -> t
end

val create
  :  file_by_path_by_rev : Abspath.t Path_in_repo.Map.t Rev.Compare_by_hash.Map.t
  -> cache               : Cache.t
  -> errors              : Error.t list
  -> lines_required_to_separate_ddiff_hunks : int
  -> Attributed_file.t Diamond.t
  -> [ `Equal
     | `Unequal of t
     ] Deferred.t

(** Rev-update diff4s are used to update a reviewer's brain so that it has the same revs
    as the review goal.  This is important to minimize hydra work. *)
val create_rev_update : from_:Diff2.t -> to_:Diff2.t -> t Or_error.t
val is_rev_update     : t -> bool

val create_from_scratch_to_diff2 : Diff2.t -> t
val     as_from_scratch_to_diff2 : t -> Diff2.t option

val create_forget : Diff2.t -> t
val is_forget : t -> bool

val present_paths_in_repo : t -> (Rev.t * Path_in_repo.t) list

val path_in_repo_at_f2 : t -> Path_in_repo.t

val num_lines              : t -> Reviewer.t -> int
val is_implicitly_reviewed : t -> Reviewer.t -> bool

val should_review_ownership_change : t -> Reviewer.t -> bool

val may_review
  :  t
  -> Reviewer.t
  -> [ `Dropped_from_review
     | `Dropped_from_follow
     | `Follow_lines
     | `Nothing_to_review_or_follow
     | `Review_ownership_change
     | `Review_lines
     ]

val may_follow
  :  t
  -> Reviewer.t
  -> [ `Dropped_from_follow
     | `Follow_lines
     | `Nothing_to_follow
     ]

val input  : t -> num_lines_in_diff:int -> Diff2.t
val output : t -> num_lines_in_diff:int -> Diff2.t

val summary : t list -> sort:bool -> Ascii_table.t

module And_output_num_lines : sig
  type diff4
  type t =
    { diff4            : diff4
    ; output_num_lines : int
    }
  [@@deriving fields, sexp_of]

  include Invariant.S with type t := t

  val output : t -> Diff2.t
end with type diff4 := t

module Stable : sig
  module V2 : Stable_without_comparator with type t = t
  module And_output_num_lines : sig
    module V1 : Stable_without_comparator with type t = And_output_num_lines.t
  end
end
