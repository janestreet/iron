module Stable = struct
  open! Core.Core_stable

  module Email_address           = Email_address.           Stable
  module Properties              = Properties.              Stable
  module Release_process         = Release_process.         Stable
  module Send_email_upon         = Send_email_upon.         Stable
  module User_name               = User_name.               Stable
  module Who_can_release_into_me = Who_can_release_into_me. Stable

  module V1 = struct
    type t =
      { crs_shown_in_todo_only_for_users_reviewing  : bool option
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool option
      ; owners                                      : User_name.V1.t list
      ; properties                                  : Properties.V1.t
      ; release_process                             : Release_process.V1.t option
      ; who_can_release_into_me                     : Who_can_release_into_me.V1.t option
      ; send_email_to                               : Email_address.V1.Set.t
      ; send_email_upon                             : Send_email_upon.V1.Set.t
      ; whole_feature_followers                     : User_name.V1.Set.t
      ; whole_feature_reviewers                     : User_name.V1.Set.t
      }
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| c3a1f0a519ec04bc0275f1d373755c5e |}]
    ;;
  end

  module Model = V1
end

open! Core
open! Import

include Stable.V1

let empty =
  { crs_shown_in_todo_only_for_users_reviewing  = None
  ; xcrs_shown_in_todo_only_for_users_reviewing = None
  ; owners                                      = []
  ; properties                                  = Properties.empty
  ; release_process                             = None
  ; who_can_release_into_me                     = None
  ; send_email_to                               = Email_address.Set.empty
  ; send_email_upon                             = Send_email_upon.Set.empty
  ; whole_feature_followers                     = User_name.Set.empty
  ; whole_feature_reviewers                     = User_name.Set.empty
  }

module Sexp_hum = struct
  type nonrec t = t =
    { crs_shown_in_todo_only_for_users_reviewing
      : bool option                                      [@sexp_drop_if Option.is_none]
    ; xcrs_shown_in_todo_only_for_users_reviewing
      : bool option                                      [@sexp_drop_if Option.is_none]
    ; owners                  : User_name.t list         [@sexp_drop_if List.is_empty]
    ; properties              : Properties.t             [@sexp_drop_if Map.is_empty]
    ; release_process         : Release_process.t option [@sexp_drop_if Option.is_none]
    ; who_can_release_into_me :
        Who_can_release_into_me.t option                 [@sexp_drop_if Option.is_none]
    ; send_email_to           : Email_address.Set.t      [@sexp_drop_if Set.is_empty]
    ; send_email_upon         : Send_email_upon.Set.t    [@sexp_drop_if Set.is_empty]
    ; whole_feature_followers : User_name.Set.t          [@sexp_drop_if Set.is_empty]
    ; whole_feature_reviewers : User_name.Set.t          [@sexp_drop_if Set.is_empty]
    }
  [@@deriving sexp_of]
end
