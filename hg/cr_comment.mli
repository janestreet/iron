open! Core
open! Async
open! Import

module Raw : sig
  type t [@@deriving sexp_of]

  val content : t -> string

  module With_file_positions : sig
    type nonrec t =
      { content_start : int
      ; start_index   : int
      ; end_index     : int
      ; cr            : t
      }
    [@@deriving sexp_of]

    val extract
      :  ?extra_cr_comment_headers:string list
      -> Cr_comment_format.t
      -> path          : Relpath.t
      -> file_contents : string
      -> t list
  end
end

module Due : sig
  type t =
    | Now
    | Soon
    | Someday
  [@@deriving sexp_of]
end

module Assignee : sig
  (** An [Assignee] holds a [Unresolved_name.t] because we don't know whether this
      assignee is an alias or a user name yet.
      For [Missing_file_owner], see [User_name.missing_file_owner]. *)
  type t =
    | This of Unresolved_name.t
    | Feature_owner
    | Missing_file_owner

  val user_name
    :  t
    -> feature_owner   : User_name.t
    -> alternate_names : User_name_by_alternate_name.t
    -> User_name.t
end

type t [@@deriving sexp_of]

include Invariant.S  with type t := t

val hash : t -> int

val path       : t -> Relpath.t
val content    : t -> string
val start_line : t -> int
val start_col  : t -> int
val assignee   : t -> Assignee.t

val due    : t -> Due.t
val is_xcr : t -> bool

val work_on : t -> Due.t

val to_string
  :  ?attributes:(string * string) list
  -> t
  -> include_content:bool
  -> string

(** Sorts and prints a list of crs separated by whitespace (if needed). *)
val print_list
  :  crs_and_attributes:(t * (string * string) list) list
  -> include_content:bool
  -> unit

module Structurally_compared : sig
  type nonrec t = t [@@deriving compare, sexp_of]
end

module For_sorted_output : sig
  type nonrec t = t [@@deriving compare]
end

val sort : t list -> t list

(** A [Cr_soon] is a kind of processed CR with [work_on t = Soon] (which implies it's
    not an XCR). *)
module Cr_soon : sig

  type cr_comment

  type t [@@deriving sexp]

  include Invariant.S with type t := t

  (** Because CR-soons are never assigned implicitly to a feature's owner, we don't use
      [Assignee.t].  Instead, CR-soons are either explicitly assigned to a user name in
      the text of the CR-soon, or are assigned to the file owner via Iron obligations. *)
  val assignee     : t -> Unresolved_name.t
  val content      : t -> string
  val cr_comment   : t -> cr_comment
  val path         : t -> Relpath.t
  val start_line   : t -> int

  (** [Compare_ignoring_minor_text_changes] defines when we consider two CR-soons as
      equivalent, which in turn affects whether a CR-soon is active in a feature, defined
      as present at the base and not at the tip.
      Changes in positions in a file, or changes in whitespaces are ignored. *)
  module Compare_ignoring_minor_text_changes : sig
    type nonrec t = t [@@deriving sexp_of]
    include Comparable.S with type t := t
    include Hashable.  S with type t := t
  end

  module For_sorted_output : sig
    type nonrec t = t [@@deriving compare]
  end

  module Structurally_compared : sig
    type nonrec t = t [@@deriving compare, sexp_of]
  end
end with type cr_comment := t

module Crs : sig
  type nonrec t =
    { due_now     : t list
    ; due_soon    : Cr_soon.t list
    ; due_someday : t list
    }
end

module Crs_due_now_and_soon : sig
  type nonrec t =
    { due_now  : t list
    ; due_soon : Cr_soon.t list
    }
end

val grep
  :  Repo_root.t
  -> Cr_comment_format.t
  -> below      : Path_in_repo.t
  -> file_owner : (Path_in_repo.t -> User_name.t Or_error.t)
  -> Crs.t Deferred.t

module Cached_facts_for_incremental_computation : sig
  type t =
    { base_rev       : Rev.t
    ; base_crs       : Crs_due_now_and_soon.t
    ; base_cr_format : Cr_comment_format.t
    }
end

val maybe_incremental_grep
  :  Repo_root.t
  -> Cr_comment_format.t
  -> incremental_based_on : Cached_facts_for_incremental_computation.t option
  -> file_owner : (Path_in_repo.t -> User_name.t Or_error.t)
  -> Crs_due_now_and_soon.t Deferred.t

module Summary : sig

  type cr_comment

  type t [@@deriving sexp_of]

  val create
    :  cr_comment list
    -> feature_owner   : User_name.t
    -> alternate_names : User_name_by_alternate_name.t
    -> t

  val empty : t

  val to_ascii_table : t -> Ascii_table.t option

  module Row : sig
    type t =
      { assignee : User_name.t
      ; crs      : int
      ; xcrs     : int
      }
    [@@deriving fields, sexp_of]
  end

  val rows : t -> Row.t list
end with type cr_comment := t

module Stable : sig
  module Cr_soon : sig
    module V1 : Stable_without_comparator with type t = Cr_soon.t
  end
  module Due : sig
    module V1 : Stable_without_comparator with type t = Due.t
  end
  module Summary : sig
    module V1 : Stable_without_comparator with type t = Summary.t
  end
  module V1 : Stable_without_comparator with type t = t
end
