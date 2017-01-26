open! Core
open! Import

type t =
  { mutable crs_shown_in_todo_only_for_users_reviewing  : bool option
  ; mutable xcrs_shown_in_todo_only_for_users_reviewing : bool option
  ; mutable owners                                      : User_name.t list
  ; properties                                          : Sexp.t Property.Table.t
  ; mutable release_process                             : Release_process.t option
  ; mutable who_can_release_into_me                     : Who_can_release_into_me.t option
  ; mutable send_email_to                               : Email_address.Set.t
  ; mutable send_email_upon                             : Send_email_upon.Set.t
  ; mutable whole_feature_followers                     : User_name.Set.t
  ; mutable whole_feature_reviewers                     : User_name.Set.t
  }
[@@deriving fields, sexp_of]

let create () =
  { crs_shown_in_todo_only_for_users_reviewing  = None
  ; xcrs_shown_in_todo_only_for_users_reviewing = None
  ; owners                                      = []
  ; properties                                  = Property.Table.create ()
  ; release_process                             = None
  ; who_can_release_into_me                     = None
  ; send_email_to                               = Email_address.Set.empty
  ; send_email_upon                             = Send_email_upon.Set.empty
  ; whole_feature_followers                     = User_name.Set.empty
  ; whole_feature_reviewers                     = User_name.Set.empty
  }
;;

let set_properties t properties =
  Map.iteri properties ~f:(fun ~key ~data -> Hashtbl.set t.properties ~key ~data)
;;

let properties t = Property.Map.of_hashtbl_exn t.properties
;;

let remove_properties t props =
  Set.iter props ~f:(fun prop -> Hashtbl.remove t.properties prop);
;;

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~crs_shown_in_todo_only_for_users_reviewing:ignore
      ~xcrs_shown_in_todo_only_for_users_reviewing:ignore
      ~owners:(check (List.iter ~f:User_name.invariant))
      ~properties:ignore
      ~release_process:ignore
      ~who_can_release_into_me:ignore
      ~send_email_to:ignore
      ~send_email_upon:ignore
      ~whole_feature_followers:(check (Set.iter ~f:User_name.invariant))
      ~whole_feature_reviewers:(check (Set.iter ~f:User_name.invariant))
  )
;;

let to_protocol
      { crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; owners
      ; properties
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; whole_feature_reviewers
      ; whole_feature_followers
      } =
  { Iron_common.Inheritable_attributes.
    crs_shown_in_todo_only_for_users_reviewing
  ; xcrs_shown_in_todo_only_for_users_reviewing
  ; owners
  ; properties = Property.Map.of_hashtbl_exn properties
  ; release_process
  ; who_can_release_into_me
  ; send_email_to
  ; send_email_upon
  ; whole_feature_followers
  ; whole_feature_reviewers
  }
;;
