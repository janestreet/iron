module Stable = struct

  open Import_stable

  module Bookmark_without_feature = Bookmark_without_feature.Stable

  module Action = struct
    module V3 = struct
      type t =
        { for_                       : User_name.V1.t
        ; include_active_cr_soons    : bool
        ; include_all_owned_features : bool
        }
      [@@deriving bin_io, sexp]

      let to_model t = t
    end

    module Model = V3

    module V2 = struct
      type t =
        { for_                    : User_name.V1.t
        ; include_active_cr_soons : bool
        }
      [@@deriving bin_io]

      let to_model { for_; include_active_cr_soons } =
        V3.to_model { V3.
                      for_
                    ; include_active_cr_soons
                    ; include_all_owned_features = true
                    }
      ;;
    end

  end

  module Num_crs = struct
    module V1 = struct
      type t =
        [ `Enabled of int Or_error.V1.t
        | `Disabled
        ]
      [@@deriving bin_io, sexp]
    end

    module Model = V1
  end

  module Review_lines = struct
    module V1 = struct
      type t =
        { review : Review_or_commit.V1.t
        ; follow : int
        }
      [@@deriving bin_io]
    end
  end

  module Assigned = struct

    module V8 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; feature_path_exists : bool
        ; review_is_enabled   : bool
        ; user_is_reviewing   : bool
        ; may_second          : bool
        ; num_crs             : Num_crs.V1.t
        ; num_xcrs            : Num_crs.V1.t
        ; line_count          : Line_count.V4.t
        ; next_steps          : Next_step.V5.t list
        }
      [@@deriving bin_io, fields, sexp]
    end

    module V7 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; feature_path_exists : bool
        ; review_is_enabled   : bool
        ; may_second          : bool
        ; num_crs             : Num_crs.V1.t
        ; num_xcrs            : Num_crs.V1.t
        ; review_lines        : [ `Lines of Review_lines.V1.t
                                | `Not_reviewing
                                ]
        ; catch_up_lines      : int Or_error.V1.t
        ; next_steps          : Next_step.V5.t list
        }
      [@@deriving bin_io]

      open! Core.Std
      open! Import

      let of_v8 { V8.
                  feature_path
                ; feature_path_exists
                ; review_is_enabled
                ; user_is_reviewing
                ; may_second
                ; num_crs
                ; num_xcrs
                ; line_count
                ; next_steps
                } =
        let review_lines =
          if not user_is_reviewing
          then `Not_reviewing
          else `Lines { Review_lines.V1.
                        review = Line_count.to_review_column_shown line_count
                      ; follow = line_count.review.follow
                      }
        in
        let catch_up_lines = Ok (Line_count.Catch_up.total line_count.catch_up) in
        { feature_path
        ; feature_path_exists
        ; review_is_enabled
        ; may_second
        ; num_crs
        ; num_xcrs
        ; review_lines
        ; catch_up_lines
        ; next_steps
        }
      ;;
    end

    module V6 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; feature_path_exists : bool
        ; review_is_enabled   : bool
        ; may_second          : bool
        ; num_crs             : Num_crs.V1.t
        ; num_xcrs            : Num_crs.V1.t
        ; review_lines        : [ `Lines of Review_lines.V1.t
                                | `Not_reviewing
                                ]
        ; catch_up_lines      : int Or_error.V1.t
        }
      [@@deriving bin_io]

      let of_v7 { V7.
                  feature_path
                ; feature_path_exists
                ; review_is_enabled
                ; may_second
                ; num_crs
                ; num_xcrs
                ; review_lines
                ; catch_up_lines
                ; next_steps = _
                } =
        { feature_path
        ; feature_path_exists
        ; review_is_enabled
        ; may_second
        ; num_crs
        ; num_xcrs
        ; review_lines
        ; catch_up_lines
        }
      ;;
    end

    module V5 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; feature_path_exists : bool
        ; review_is_enabled   : bool
        ; may_second          : bool
        ; num_crs             : Num_crs.V1.t
        ; num_xcrs            : Num_crs.V1.t
        ; review_lines        : [ `Lines of int
                                | `Not_reviewing
                                ]
        ; catch_up_lines      : int Or_error.V1.t
        }
      [@@deriving bin_io]

      open! Core.Std
      open! Import

      let of_v6 { V6.
                  feature_path
                ; feature_path_exists
                ; review_is_enabled
                ; may_second
                ; num_crs
                ; num_xcrs
                ; review_lines
                ; catch_up_lines
                } =
        let review_lines =
          match review_lines with
          | `Not_reviewing            -> `Not_reviewing
          | `Lines { review; follow } ->
            `Lines (Review_or_commit.count review + follow)
        in
        { feature_path
        ; feature_path_exists
        ; review_is_enabled
        ; may_second
        ; num_crs
        ; num_xcrs
        ; review_lines
        ; catch_up_lines
        }
      ;;
    end

    module V4 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; feature_path_exists : bool
        ; review_is_enabled   : bool
        ; may_second          : bool
        ; num_crs             : int Or_error.V1.t
        ; num_xcrs            : int Or_error.V1.t
        ; review_lines        : [ `Lines of int
                                | `Not_reviewing
                                ]
        ; catch_up_lines      : int Or_error.V1.t
        }
      [@@deriving bin_io]

      let num_crs_of_v5 = function
        | `Enabled crs -> crs
        | `Disabled -> Result.V1.Ok 0
      ;;

      let of_v5 { V5.
                     feature_path
                   ; feature_path_exists
                   ; review_is_enabled
                   ; may_second
                   ; num_crs
                   ; num_xcrs
                   ; review_lines
                   ; catch_up_lines
                   } =
        { feature_path
        ; feature_path_exists
        ; review_is_enabled
        ; may_second
        ; num_crs           = num_crs_of_v5 num_crs
        ; num_xcrs          = num_crs_of_v5 num_xcrs
        ; review_lines
        ; catch_up_lines
        }
      ;;
    end

    module Model = V8
  end

  module Rev_facts = struct
    module V1 = struct
      type t =
        { is_conflict_free      : (bool, unit) Result.V1.t
        ; is_cr_clean           : (bool, unit) Result.V1.t
        ; obligations_are_valid : (bool, unit) Result.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]
    end

    module Model = V1
  end

  module Feature_info = struct
    module V6 = struct
      type t =
        { feature_path                        : Feature_path.V1.t
        ; num_crs                             : int Or_error.V1.t
        ; num_xcrs                            : int Or_error.V1.t
        ; num_reviewers_with_review_remaining : int Or_error.V1.t
        ; base                                : Rev_facts.V1.t Or_pending.V1.t
        ; tip                                 : Rev_facts.V1.t Or_pending.V1.t
        ; review_is_enabled                   : bool
        ; next_steps                          : Next_step.V5.t list
        }
      [@@deriving bin_io, fields, sexp]
    end

    module Model = V6
  end

  module Reaction = struct

    module V13 = struct
      type t =
        { assigned                  : Assigned.V8.t list
        ; unclean_workspaces        : Unclean_workspace.V1.t list Machine.V1.Map.t
        ; owned                     : Feature_info.V6.t list
        ; watched                   : Feature_info.V6.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io, fields, sexp]

      let of_model m = m
    end

    module V12 = struct
      type t =
        { assigned                  : Assigned.V7.t list
        ; unclean_workspaces        : Unclean_workspace.V1.t list Machine.V1.Map.t
        ; owned                     : Feature_info.V6.t list
        ; watched                   : Feature_info.V6.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io]

      let of_model m =
        let { V13.
              assigned
            ; unclean_workspaces
            ; owned
            ; watched
            ; cr_soons
            ; bookmarks_without_feature
            } = V13.of_model m in
        { assigned = List.map assigned ~f:Assigned.V7.of_v8
        ; unclean_workspaces
        ; owned
        ; watched
        ; cr_soons
        ; bookmarks_without_feature
        }
      ;;
    end

    module V11 = struct
      type t =
        { assigned                  : Assigned.V7.t list
        ; owned                     : Feature_info.V6.t list
        ; watched                   : Feature_info.V6.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io]

      let of_model m =
        let { V12.
              assigned
            ; owned
            ; watched
            ; cr_soons
            ; bookmarks_without_feature
            ; _
            } = V12.of_model m in
        { assigned
        ; owned
        ; watched
        ; cr_soons
        ; bookmarks_without_feature
        }
      ;;
    end

    module V10 = struct
      type t =
        { assigned                  : Assigned.V6.t list
        ; owned                     : Feature_info.V6.t list
        ; watched                   : Feature_info.V6.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io]

      let of_model m =
        let { V11.
              assigned
            ; owned
            ; watched
            ; cr_soons
            ; bookmarks_without_feature
            } = V11.of_model m in
        { assigned                  = List.map assigned ~f:Assigned.V6.of_v7
        ; owned
        ; watched
        ; cr_soons
        ; bookmarks_without_feature
        }
      ;;
    end

    module V9 = struct
      type t =
        { assigned                  : Assigned.V5.t list
        ; owned                     : Feature_info.V6.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io]

      let of_model m =
        let { V10.
              assigned
            ; owned
            ; cr_soons
            ; bookmarks_without_feature
            ; _
            } = V10.of_model m in
        { assigned                  = List.map assigned ~f:Assigned.V5.of_v6
        ; owned
        ; cr_soons
        ; bookmarks_without_feature
        }
      ;;
    end

    module V8 = struct
      type t =
        { assigned                  : Assigned.V4.t list
        ; owned                     : Feature_info.V6.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io]

      let of_model m =
        let { V9.
              assigned
            ; owned
            ; cr_soons
            ; bookmarks_without_feature
            } = V9.of_model m in
        { assigned                  = List.map assigned ~f:Assigned.V4.of_v5
        ; owned
        ; cr_soons
        ; bookmarks_without_feature
        }
      ;;
    end

    module Model = V13
  end
end

open! Core.Std
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "todo" end)
    (struct let version = 14 end)
    (Stable.Action.V3)
    (Stable.Reaction.V13)

include Register_old_rpc
    (struct let version = 13 end)
    (Stable.Action.V3)
    (Stable.Reaction.V12)

include Register_old_rpc
    (struct let version = 12 end)
    (Stable.Action.V3)
    (Stable.Reaction.V11)

include Register_old_rpc
    (struct let version = 11 end)
    (Stable.Action.V3)
    (Stable.Reaction.V10)

include Register_old_rpc
    (struct let version = 10 end)
    (Stable.Action.V3)
    (Stable.Reaction.V9)

include Register_old_rpc
    (struct let version = 9 end)
    (Stable.Action.V3)
    (Stable.Reaction.V8)

include Register_old_rpc
    (struct let version = 8 end)
    (Stable.Action.V2)
    (Stable.Reaction.V8)

module Action = Stable.Action.Model

module Assigned = struct
  include Stable.Assigned.Model

  let has_review_lines t =
    t.user_is_reviewing
    && Review_or_commit.count (Line_count.to_review_column_shown t.line_count) > 0
  ;;

  let has_follow_lines t =
    t.user_is_reviewing
    && t.line_count.review.follow > 0
  ;;

  let has_catch_up_lines t =
    Line_count.Catch_up.total t.line_count.catch_up > 0
  ;;
end

module Feature_info = Stable.Feature_info. Model
module Num_crs      = Stable.Num_crs.      Model
module Reaction     = Stable.Reaction.     Model
module Rev_facts    = Stable.Rev_facts.    Model
