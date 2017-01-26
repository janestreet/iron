(** The change in a single file from a base rev to a tip rev. *)

open! Core
open! Import

type t = private
  { base              : Attributed_file.t
  ; tip               : Attributed_file.t
  ; num_lines_in_diff : int
  }
[@@deriving fields, sexp_of]

include Invariant.S with type t := t

val create
  :  base              : Attributed_file.t
  -> tip               : Attributed_file.t
  -> num_lines_in_diff : int
  -> t

val involved_in_ownership_change : t -> User_name.Set.t

val may_review    : t -> include_may_follow:bool     -> Reviewer.t -> bool
val may_reviewers : t -> include_file_followers:bool -> User_name.Set.t

val review_edge : t -> Review_edge.t

val should_automatically_forget : t -> bool

val de_alias : t -> User_name_by_alternate_name.t -> t

val with_num_lines : t -> int -> t

val path_in_repo_at_tip : t -> Path_in_repo.t

module Ignoring_rev : sig
  type nonrec t = t [@@deriving sexp_of]
  include Comparable with type t := t
  include Hashable   with type t := t
end

module Stable : sig
  module V2 : Stable_without_comparator with type t = t
end
