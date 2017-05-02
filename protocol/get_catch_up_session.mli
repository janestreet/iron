(** Get the next catch-up session for a user for a feature. *)

open! Core
open! Import

module Action : sig
  type t =
    { feature_path : Feature_path.t
    ; for_         : User_name.t
    }
  [@@deriving fields, sexp_of]
end

module Catch_up_session : sig
  type t =
    { catch_up_session_id              : Session_id.t
    ; catch_up_session_tip             : Rev.t
    ; creation_time                    : Time.t
    ; reviewer_in_session              : Reviewer.t
    ; diff4s_to_catch_up               : Diff4_to_catch_up.t list
    ; line_count_remaining_to_catch_up : Line_count.Catch_up.t
    ; remote_rev_zero                  : Rev.t
    ; remote_repo_path                 : Remote_repo_path.t
    ; feature_path                     : Feature_path.t
    ; feature_id                       : Feature_id.t
    ; whole_feature_reviewers          : User_name.Set.t
    ; owners                           : User_name.t list
    ; base                             : Rev.t
    ; tip                              : Rev.t
    ; description                      : string
    ; is_permanent                     : bool
    ; is_archived                      : Is_archived.t
    ; seconder                         : User_name.t option
    ; lines_required_to_separate_ddiff_hunks : int
    }
  [@@deriving sexp_of]
end

module Reaction : sig
  type t =
    [ `Up_to_date
    | `Catch_up_session of Catch_up_session.t
    ]
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
