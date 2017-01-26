open! Core
open! Import

module Action : sig
  type active_user_set =
    | Some_active of User_name.Set.t
    | All_active
  [@@deriving sexp_of]

  type t =
    { feature_path : Feature_path.t
    ; users        : active_user_set
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t =
    { description                           : string
    ; line_count_by_user                    : (User_name.t * Line_count.t) list
    ; users_with_review_session_in_progress : User_name.Set.t Or_error.t
    ; users_with_unclean_workspaces         : Unclean_workspace_reason.t User_name.Map.t
    ; cr_summary                            : Cr_comment.Summary.t
    ; users                                 : User_name.Set.t
    ; next_bookmark_update                  : Next_bookmark_update.t
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
