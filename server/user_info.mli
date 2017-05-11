(** Tracks all the information about user names:

    - two maps from alternate names to user names
    - [typos], a map from typos to user_names
    - [aliases_seen], the historical aggregation of (alias, user_name) pairs
    - [valid_users], the users that we assume look at their todo
    - [existing_users], the users that can be used at the command line, which is all the
    users already in the system plus all the valid users
    - [invalid_users] -- existing users that aren't valid

    The following invariant about these sets hold: there is no overlap between aliases,
    typos and valid user names.

    Should I add an alias or a typo?
    ================================

    The difference between an alias and a typo is that we expect people to use aliases
    deliberately, whereas we expect typos are always accidental. E.g., (jode -> jdoe)
    is a typo but (sophia -> svonmecklenburg-strelitz) is an alias.

    Obligations can be defined in terms of aliases but not typos. In contrast, CRs can use
    either. *)


open! Core
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val dump : t -> Iron_protocol.Dump.Which_user_info.t -> Sexp.t

val complete
  :  t
  -> prefix:string
  -> Iron_protocol.Complete.Which_user_info.t
  -> string list

val ensure_user_exists : t -> User_name.t -> unit
val ensure_users_exist : t -> User_name.t list -> unit

val are_acting_for_themselves_or_for_invalid_user
  :  t
  -> for_:User_name.t
  -> by:User_name.t
  -> bool

(** This function recomputes the existing users, and it also computes what is returned by
    [invalid_users] to complain. *)
val refresh_existing_users
  :  t
  -> occurrences_by_user_name : User_name_occurrence.t list User_name.Table.t
  -> unit

val invalid_users : t -> Iron_protocol.Get_invalid_users.Reaction.t

val deserializer : t Deserializer.t

val define_typos_exn : t -> Iron_protocol.Define_typos.Definition.t list -> unit

val alternate_names
  : t
  -> which:Iron_protocol.Get_alternate_names.Action.t
  -> User_name_by_alternate_name.t

val remove_alternate_names_exn
  :  t
  -> Alternate_name.t list
  -> which : [< `Aliases | `Typos ]
  -> unit

(** Raises if the new values contain an alias resolution that clashes with an existing
    one.  Side effects are transactional. *)
val update_valid_users_and_aliases_exn
  :  t
  -> Iron_protocol.Update_valid_users_and_aliases.User_aliases.t list
  -> unit

module User_set : sig
  module type S = sig
    val add      : t -> User_name.Set.t -> idempotent:bool -> unit Or_error.t
    val remove   : t -> User_name.Set.t -> idempotent:bool -> unit Or_error.t
    val mem      : t -> User_name.t -> bool
    val get_set  : t -> User_name.Set.t
  end
end

module Admins                : User_set.S
module Feeding_metrics       : User_set.S
module Using_locked_sessions : User_set.S

val get_user_set : Iron_protocol.User_set.t -> (module User_set.S)
