open! Core
open! Import

(** A 4 way diff algo, also called view is a particular way to look at a the 4 files *)

module Id : (module type of Diff_algo_id with type t = Diff_algo_id.t)

type t
[@@deriving sexp_of] (** show the id only just for debugging purposes *)

module Block : sig
  type t =
    { hint   : string list
    ; lines  : string list
    }
  [@@deriving sexp_of]
end

module View : sig
  type t =
    { id           : Id.t
    ; blocks       : Block.t list
    }
  [@@deriving fields, sexp_of]
end

val id : t -> Id.t

val apply : t
  -> include_hunk_breaks:bool
  -> diff4_class:Diff4_class.t
  -> context:int
  -> Slice.t Diamond.t
  -> View.t

val display_forget :
  context:int
  -> Slice.t Diamond.t
  -> View.t

val display_errors :
  Error.t list
  -> View.t option

val should_split_files_in_hunks : Diff4_class.t -> bool

val select_algos_for_review : Diff4_class.Shown_class.t -> t list

val infinite_context : int

val all_standard_views : t list

val default_ddiff_view : t

val lines
  : t
  -> context:int
  -> Slice.t Diamond.t
  -> string list
