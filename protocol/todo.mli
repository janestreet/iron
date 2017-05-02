open! Core
open! Import

module Action : sig

  (** [include_all_owned_features] is whether or not to include in [Reaction.owned] empty
      features that are permanent or have children. *)

  type t =
    { for_                       : User_name.t
    ; include_active_cr_soons    : bool
    ; include_all_owned_features : bool
    }
  [@@deriving sexp_of]
end

module Num_crs : sig
  type t =
    [ `Enabled of int Or_error.t
    | `Disabled
    ]
  [@@deriving sexp_of]
end

module Assigned : sig
  (** [assigned_next_steps] may be different from [next_steps].  A next-step is assigned
      if there is a clear action that we expect the assignee will want to do immediately.
      Each assigned next step is shown only to its assignee, typically the feature's first
      owner. *)
  type t =
    { feature_path        : Feature_path.t
    ; feature_path_exists : bool
    ; review_is_enabled   : bool
    ; user_is_reviewing   : bool
    ; assigned_next_steps : Next_step.t list
    ; num_crs             : Num_crs.t
    ; num_xcrs            : Num_crs.t
    ; line_count          : Line_count.t
    ; next_steps          : Next_step.t list
    }
  [@@deriving fields, sexp_of]

  val has_review_lines   : t -> bool
  val has_follow_lines   : t -> bool
  val has_catch_up_lines : t -> bool
end

module Rev_facts : sig
  type t =
    { is_conflict_free      : (bool, unit) Result.t
    ; is_cr_clean           : (bool, unit) Result.t
    ; obligations_are_valid : (bool, unit) Result.t
    }
  [@@deriving fields, sexp_of]
end

module Feature_info : sig
  type t =
    { feature_path                        : Feature_path.t
    ; num_crs                             : int Or_error.t
    ; num_xcrs                            : int Or_error.t
    ; num_reviewers_with_review_remaining : int Or_error.t
    ; base                                : Rev_facts.t Or_pending.t
    ; tip                                 : Rev_facts.t Or_pending.t
    ; review_is_enabled                   : bool
    ; next_steps                          : Next_step.t list
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t =
    { assigned                  : Assigned.t list
    ; unclean_workspaces        : Unclean_workspace.t list Machine_name.Map.t
    ; owned                     : Feature_info.t list
    ; watched                   : Feature_info.t list
    ; cr_soons                  : Cr_soon_multiset.t
    ; bookmarks_without_feature : (Remote_repo_path.t
                                   * Bookmark_without_feature.t list) list
    }
  [@@deriving fields, sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
