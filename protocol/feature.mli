open! Core
open! Import

(** Default values for certain fields on creation, put here to be shared between the
    client and the server logic.  For example in the client, we do not [fe show] certain
    fields if they are equal to their default while we experiment *)
module Default_values : sig
  val crs_shown_in_todo_only_for_users_reviewing  : bool
  val xcrs_shown_in_todo_only_for_users_reviewing : bool
end

module Locked : sig
  type t =
    { by           : User_name.t
    ; reason       : string
    ; at           : Time.t
    ; is_permanent : bool
    }
  [@@deriving sexp_of]
end

(** [diff_from_base_to_tip] is not always populated, or sometimes is the diff for a
    particular user only.  Depends on what RPC is used, and what is the value requested
    [What_diff.t] in the query sent. *)
type t =
  { feature_id                     : Feature_id.t
  ; feature_path                   : Feature_path.t
  ; rev_zero                       : Rev.t
  ; whole_feature_followers        : User_name.Set.t
  ; whole_feature_reviewers        : User_name.Set.t
  ; owners                         : User_name.t list
  ; base                           : Rev.t
  ; base_facts                     : Rev_facts.t Or_pending.t
  ; next_base_update               : Next_base_update.t
  ; crs_are_enabled                : bool
  ; crs_shown_in_todo_only_for_users_reviewing  : bool
  ; xcrs_shown_in_todo_only_for_users_reviewing : bool
  ; next_bookmark_update           : Next_bookmark_update.t
  ; has_bookmark                   : bool
  ; tip                            : Rev.t
  ; tip_facts                      : Rev_facts.t Or_pending.t
  ; base_is_ancestor_of_tip        : Rev_facts.Is_ancestor.t Or_pending.t
  ; diff_from_base_to_tip          : Diff2s.t Or_error.t Or_pending.t
  ; description                    : string
  ; is_permanent                   : bool
  ; seconder                       : User_name.t option
  ; review_is_enabled              : bool
  ; reviewing                      : Reviewing.t
  ; allow_review_for               : Allow_review_for.t
  ; included_features              : Released_feature.t list
  ; properties                     : Properties.t
  ; remote_repo_path               : Remote_repo_path.t
  ; has_children                   : bool
  ; release_process                : Release_process.t
  ; who_can_release_into_me        : Who_can_release_into_me.t
  ; send_email_to                  : Email_address.Set.t
  ; send_email_upon                : Send_email_upon.Set.t
  ; locked                         : (Lock_name.t * Locked.t list) list
  ; line_count_by_user             : (User_name.t * Line_count.t) list Or_error.t
  ; cr_summary                     : Cr_comment.Summary.t Or_error.t
  ; next_steps                     : Next_step.t list
  ; users_with_review_session_in_progress : User_name.Set.t Or_error.t
  ; users_with_unclean_workspaces  : Unclean_workspace_reason.t User_name.Map.t
  ; is_archived                    : Is_archived.t
  ; is_rebased                     : bool
  ; latest_release                 : Latest_release.t option
  ; inheritable_attributes         : Inheritable_attributes.t
  ; compilation_status             : Compilation_status.t
  }
[@@deriving fields, sexp_of]

module Sorted_by : sig
  type t =
    [ `Name
    (** [Release_time] is local: siblings are ordered by release time, and nested features
        are grouped with their parents. *)
    | `Release_time
    | `Release_time_decreasing
    ]
  [@@deriving compare, sexp_of]
end

val released_features
  :  t
  -> sorted_by:Sorted_by.t
  -> Released_feature.t list

val user_is_currently_reviewing : t -> User_name.t -> bool

val reviewers_exn
  :  t
  -> sort : [ `Alphabetically | `Decreasing_review ]
  -> User_name.t list

val who_can_review_exn : t -> User_name.Set.t

val reviewer_in_feature : t -> User_name.t -> Reviewer.t

(** Allow [fe diff -archived] to work on released feature, even though as part of their
    release they have [base <- tip].  We keep the state of the latest released edge
    before archive. *)
val recover_diff_of_its_latest_release : t -> t option

module Stable : sig
  module Locked : sig
    module Model : sig
      type t = Locked.t [@@deriving sexp_of]
    end
    module V2 : sig
      type t = Model.t [@@deriving bin_io, sexp_of]
    end
    module V1 : sig
      type t [@@deriving bin_io]
      val of_v2 : V2.t -> t
    end
  end

  module Sorted_by : sig

    module Model : sig
      type t = Sorted_by.t [@@deriving sexp_of]
    end

    module V2 : sig
      type t = Model.t [@@deriving bin_io, sexp]
    end

    module V1 : sig
      type t [@@deriving bin_io]

      val to_v2 : t -> V2.t
    end
  end

  module Model : sig
    type nonrec t = t [@@deriving sexp_of]
  end

  module V23 : sig
    type t = Model.t [@@deriving bin_io, sexp_of]
    val of_model : Model.t -> t
  end

  module V22 : sig
    type t [@@deriving bin_io]
    val of_model : Model.t -> t
  end

  module V21 : sig
    type t [@@deriving bin_io]
    val of_model : Model.t -> t
  end

  module V20 : sig
    type t [@@deriving bin_io]
    val of_model : Model.t -> t
  end

  module V19 : sig
    type t [@@deriving bin_io]
    val of_model : Model.t -> t
  end

  module V18 : sig
    type t [@@deriving bin_io]
    val of_model : Model.t -> t
  end

  module V17 : sig
    type t [@@deriving bin_io]
    val of_model : Model.t -> t
  end

  module V16 : sig
    type t [@@deriving bin_io]
    val of_model : Model.t -> t
  end

  module V15 : sig
    type t [@@deriving bin_io]
    val of_model : Model.t -> t
  end

  module V14 : sig
    type t [@@deriving bin_io]
    val of_model : Model.t -> t
  end

  module V13 : sig
    type t [@@deriving bin_io]
    val of_model : Model.t -> t
  end

  module V12 : sig
    type t [@@deriving bin_io]
    val of_model : Model.t -> t
  end

  module V11 : sig
    type t [@@deriving bin_io]
    val of_model : Model.t -> t
  end

  module V10 : sig
    type t [@@deriving bin_io]
    val of_model : Model.t -> t
  end
end
