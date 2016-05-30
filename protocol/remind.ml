module Stable = struct

  open Import_stable

  module Feature = Feature.Stable

  module Action = struct
    module V1 = struct
      type active_user_set =
        | Some_active of User_name.V1.Set.t
        | All_active
      [@@deriving bin_io, sexp]

      type t =
        { feature_path : Feature_path.V1.t
        ; users        : active_user_set
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end

    module Model = V1
  end

  module Reaction = struct
    module V8 = struct
      type t =
        { description                    : string
        ; line_count_by_user             : (User_name.V1.t * Line_count.V4.t) list
        ; users_with_uncommitted_session : User_name.V1.Set.t Or_error.V1.t
        ; users_with_unclean_workspaces
          : Unclean_workspace_reason.V1.t User_name.V1.Map.t
        ; cr_summary                     : Cr_comment.Summary.V1.t
        ; users                          : User_name.V1.Set.t
        ; next_bookmark_update           : Next_bookmark_update.V1.t
        }
      [@@deriving bin_io, sexp]

      let of_model (m : t) = m
    end

    module V7 = struct
      type t =
        { description                    : string
        ; line_count_by_user             : (User_name.V1.t * Line_count.V3.t) list
        ; users_with_uncommitted_session : User_name.V1.Set.t Or_error.V1.t
        ; users_with_unclean_workspaces
          : Unclean_workspace_reason.V1.t User_name.V1.Map.t
        ; cr_summary                     : Cr_comment.Summary.V1.t
        ; users                          : User_name.V1.Set.t
        ; next_bookmark_update           : Next_bookmark_update.V1.t
        }
      [@@deriving bin_io]

      let of_model m =
        let { V8.
              description
            ; line_count_by_user
            ; users_with_uncommitted_session
            ; users_with_unclean_workspaces
            ; cr_summary
            ; users
            ; next_bookmark_update
            } = V8.of_model m in
        let line_count_by_user =
          List.map line_count_by_user ~f:(fun (user, line_count) ->
            user, Line_count.V3.of_v4 line_count)
        in
        { description
        ; line_count_by_user
        ; users_with_uncommitted_session
        ; users_with_unclean_workspaces
        ; cr_summary
        ; users
        ; next_bookmark_update
        }
      ;;

    end

    module V6 = struct
      type t =
        { description        : string
        ; line_count_by_user : (User_name.V1.t * int) list
        ; cr_summary         : Cr_comment.Summary.V1.t
        ; users              : User_name.V1.Set.t
        }
      [@@deriving bin_io]

      open! Core.Std
      open! Import

      let of_model m =
        let { V7.
              description
            ; line_count_by_user
            ; cr_summary
            ; users
            ; _
            } = V7.of_model m in
        let line_count_by_user =
          List.map line_count_by_user ~f:(fun (user, line_count) ->
            user, Review_or_commit.count line_count.review
          )
        in
        { description
        ; line_count_by_user
        ; cr_summary
        ; users
        }
      ;;
    end

    module Model = V8
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "remind" end)
    (struct let version = 8 end)
    (Stable.Action.V1)
    (Stable.Reaction.V8)

include Register_old_rpc
    (struct let version = 7 end)
    (Stable.Action.V1)
    (Stable.Reaction.V7)

include Register_old_rpc
    (struct let version = 6 end)
    (Stable.Action.V1)
    (Stable.Reaction.V6)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
