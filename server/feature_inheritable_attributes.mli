open! Core
open! Import

type t [@@deriving sexp_of]

val create : unit -> t

include Invariant.S with type t := t

val to_protocol
  : t
  -> Inheritable_attributes.t

val set_crs_shown_in_todo_only_for_users_reviewing
  : t
  -> bool option
  -> unit

val set_xcrs_shown_in_todo_only_for_users_reviewing
  : t
  -> bool option
  -> unit

val set_owners
  : t
  -> User_name.t list
  -> unit

val set_properties
  : t
  -> Properties.t
  -> unit

val set_release_process
  : t
  -> Release_process.t option
  -> unit

val set_who_can_release_into_me
  : t
  -> Who_can_release_into_me.t option
  -> unit

val set_send_email_to
  : t
  -> Email_address.Set.t
  -> unit

val set_send_email_upon
  : t
  -> Send_email_upon.Set.t
  -> unit

val set_whole_feature_followers
  : t
  -> User_name.Set.t
  -> unit

val set_whole_feature_reviewers
  : t
  -> User_name.Set.t
  -> unit

val crs_shown_in_todo_only_for_users_reviewing
  : t
  -> bool option

val xcrs_shown_in_todo_only_for_users_reviewing
  : t
  -> bool option

val owners
  : t
  -> User_name.t list

val properties
  : t
  -> Properties.t

val remove_properties
  : t
  -> Property.Set.t
  -> unit

val release_process
  : t
  -> Release_process.t option

val who_can_release_into_me
  : t
  -> Who_can_release_into_me.t option

val send_email_to
  : t
  -> Email_address.Set.t

val send_email_upon
  : t
  -> Send_email_upon.Set.t

val whole_feature_followers
  : t
  -> User_name.Set.t

val whole_feature_reviewers
  : t
  -> User_name.Set.t




