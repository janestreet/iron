open! Core
open! Import

type t =
  { crs_shown_in_todo_only_for_users_reviewing  : bool option
  ; xcrs_shown_in_todo_only_for_users_reviewing : bool option
  ; owners                                      : User_name.t list
  ; properties                                  : Properties.t
  ; release_process                             : Release_process.t option
  ; who_can_release_into_me                     : Who_can_release_into_me.t option
  ; send_email_to                               : Email_address.Set.t
  ; send_email_upon                             : Send_email_upon.Set.t
  ; whole_feature_followers                     : User_name.Set.t
  ; whole_feature_reviewers                     : User_name.Set.t
  }
[@@deriving compare, sexp_of]

val empty : t

module Sexp_hum : sig
  (** Lighter syntax, omit the fields that are empty. *)
  type nonrec t = t [@@deriving sexp_of]
end

module Stable : sig
  module Model : T with type t = t
  module V1 : sig
    include Stable_without_comparator
      with type t = Model.t
  end
end
