open! Core
open! Async
open! Import

module Is_ancestor : sig

  type t [@@deriving compare, sexp_of]

  val create : Repo_root.t -> ancestor:Rev.t -> descendant:Rev.t -> t Deferred.t

  val check : t -> ancestor:Rev.t -> descendant:Rev.t -> bool Or_error.t

  val reflexive : Rev.t -> t

  (** [transitive (a1, d1) (a2, d2) = (a1, d2)], when [d1 = a2]. *)
  val transitive : t -> t -> t Or_error.t

end

module Is_conflict_free : sig

  type t [@@deriving compare, sexp_of]

  val create
    :  ?repo_is_clean:Hg.Cleanliness_witness.t
    -> Repo_root.t
    -> Rev.t
    -> t Deferred.t

  val check : t -> Rev.t -> bool Or_error.t
end

(** A revision is CR clean if we can determine its CR format and there are no CRs
    according to that format. *)
module Is_cr_clean : sig

  type t [@@deriving compare, sexp_of]

  (** If [create] cannot determine the CR format (e.g. because it cannot locate
      obligations-global.sexp), then it returns a [t] that is not CR clean and an [Error]
      for the CR comments. *)
  val create
    :  ?repo_is_clean:Hg.Cleanliness_witness.t
    -> Repo_root.t
    -> Cr_comment_format.t Or_error.t
    -> incremental_based_on : Cr_comment.Cached_facts_for_incremental_computation.t option
    -> Rev.t
    -> file_owner : (Path_in_repo.t -> User_name.t Or_error.t)
    -> (t * Cr_comment.Crs_due_now_and_soon.t Or_error.t) Deferred.t

  val check : t -> Rev.t -> bool Or_error.t
end

module Obligations_are_valid : sig

  type t [@@deriving compare, sexp_of]

  (** [create_exn] raises unless the repo's parent is [rev] and the working directory is
      clean. *)
  val create_exn
    :  ?fake_for_testing : Review_attributes.t
    -> ?repo_is_clean:Hg.Cleanliness_witness.t
    -> Repo_root.t
    -> Rev.t
    -> aliases           : User_name_by_alternate_name.t
    -> (t * Obligations.t Or_error.t * Obligations_version.t Or_error.t) Deferred.t

  val check  : t -> Rev.t -> bool Or_error.t

end

type t = private
  { is_conflict_free      : Is_conflict_free.t
  ; is_cr_clean           : Is_cr_clean.t
  ; obligations_are_valid : Obligations_are_valid.t
  }
[@@deriving compare, sexp_of]

include Invariant.S with type t := t

val rev : t -> Rev.t

val create
  :  Is_conflict_free.t
  -> Is_cr_clean.t
  -> Obligations_are_valid.t
  -> t Or_error.t

val check
  :  ?allow_non_cr_clean:bool  (** default is [false] *)
  ->  t
  -> Rev.t
  -> bool Or_error.t

val check_true
  :  ?allow_non_cr_clean:bool   (** default is [false] *)
  -> ?error_msg_if_not_cr_clean:string
  -> t
  -> Rev.t
  -> unit Or_error.t

val with_rev_exn : t -> Rev.t -> t

module Stable : sig
  module Is_ancestor : sig
    module V1 : Stable_without_comparator with type t = Is_ancestor.t
  end
  module Is_conflict_free : sig
    module V1 : Stable_without_comparator with type t = Is_conflict_free.t
  end
  module Is_cr_clean : sig
    module V1 : Stable_without_comparator with type t = Is_cr_clean.t
  end
  module Obligations_are_valid : sig
    module V1 : Stable_without_comparator with type t = Obligations_are_valid.t
  end
  module V1 : Stable_without_comparator with type t = t
end
