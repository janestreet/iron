(** Syntactic form in [.fe.sexp] files for specifying the review-obligations attribute. *)

open! Core
open! Import

type t [@@deriving sexp]

val eval
  :  t
  -> Error_context.t
  -> aliases       : User_name_by_alternate_name.t
  -> allowed_users : Unresolved_name.Set.t
  -> known_groups  : Groups.t
  -> Review_obligation.t

val synthesize : Review_obligation.t -> t
