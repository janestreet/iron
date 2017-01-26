(** Syntactic form in [.fe.sexp] files for specifying a set of users. *)

open! Core
open! Import

type t [@@deriving sexp]

type ('syntax, 'semantics) eval_user
  =  'syntax
  -> Error_context.t
  -> aliases       : User_name_by_alternate_name.t
  -> allowed_users : Unresolved_name.Set.t
  -> 'semantics

type ('syntax, 'semantics) eval
  = ('syntax,  known_groups:Groups.t -> 'semantics) eval_user

val eval       : (t                     , User_name.Set.t) eval
val eval_group : (Group_name.t          , User_name.Set.t) eval
val eval_user  : (Unresolved_name.t     , User_name.t    ) eval_user

val synthesize : User_name.Set.t -> t

(** Syntactic form of [Allow_review_for] stanzas in [obligations-repo.sexp]. *)
module Allow_review_for : sig
  type t [@@deriving sexp]

  val eval : (t, Allow_review_for.Users.t) eval
end
