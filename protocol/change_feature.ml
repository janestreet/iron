module Stable = struct

  open Import_stable

  (* The [of_model] functions are called only to build a Reaction containing an echo of
     the updates sent in the first place by an old client.  As a result, failing on
     unknown constructor is fine because the old clients cannot have sent these updates in
     the first place, so this is an unreachable execution path. *)
  let assert_false__invariant_in_reaction here =
    let open Core.Std in
    raise_s [%sexp "assert false", (here : Source_code_position.t)]
  ;;

  module Update = struct

    module V8 = struct
      type t =
        [ `Add_inheritable_owners        of User_name.V1.t list
        | `Add_inheritable_send_email_to of Email_address.V1.Set.t
        | `Add_inheritable_send_email_upon of Send_email_upon.V1.Set.t
        | `Add_inheritable_whole_feature_followers of User_name.V1.Set.t
        | `Add_inheritable_whole_feature_reviewers of User_name.V1.Set.t
        | `Add_owners                     of User_name.V1.t list
        | `Add_reviewing                  of User_name.V1.Set.t
        | `Add_send_email_to              of Email_address.V1.Set.t
        | `Add_send_email_upon            of Send_email_upon.V1.Set.t
        | `Add_whole_feature_followers    of User_name.V1.Set.t
        | `Add_whole_feature_reviewers    of User_name.V1.Set.t
        | `Remove_inheritable_owners      of User_name.V1.Set.t
        | `Remove_inheritable_properties    of Property.V1.Set.t
        | `Remove_inheritable_send_email_to of Email_address.V1.Set.t
        | `Remove_inheritable_send_email_upon of Send_email_upon.V1.Set.t
        | `Remove_inheritable_whole_feature_followers of User_name.V1.Set.t
        | `Remove_inheritable_whole_feature_reviewers of User_name.V1.Set.t
        | `Remove_owners                  of User_name.V1.Set.t
        | `Remove_properties              of Property.V1.Set.t
        | `Remove_reviewing               of User_name.V1.Set.t
        | `Remove_send_email_to           of Email_address.V1.Set.t
        | `Remove_send_email_upon         of Send_email_upon.V1.Set.t
        | `Remove_whole_feature_followers of User_name.V1.Set.t
        | `Remove_whole_feature_reviewers of User_name.V1.Set.t
        | `Set_base                       of Rev.V1.t
        | `Set_crs_are_enabled            of bool
        | `Set_crs_shown_in_todo_only_for_users_reviewing of bool
        | `Set_description                of string
        | `Set_inheritable_crs_shown_in_todo_only_for_users_reviewing of bool option
        | `Set_inheritable_xcrs_shown_in_todo_only_for_users_reviewing of bool option
        | `Set_inheritable_owners         of User_name.V1.t list
        | `Set_inheritable_properties     of Properties.V1.t
        | `Set_inheritable_release_process of Release_process.V1.t option
        | `Set_inheritable_who_can_release_into_me of Who_can_release_into_me.V1.t option
        | `Set_inheritable_send_email_to of Email_address.V1.Set.t
        | `Set_inheritable_send_email_upon of Send_email_upon.V1.Set.t
        | `Set_inheritable_whole_feature_followers of User_name.V1.Set.t
        | `Set_inheritable_whole_feature_reviewers of User_name.V1.Set.t
        | `Set_is_permanent               of bool
        | `Set_owners                     of User_name.V1.t list
        | `Set_properties                 of Properties.V1.t
        | `Set_release_process            of Release_process.V1.t
        | `Set_review_is_enabled          of bool
        | `Set_reviewing                  of Reviewing.V1.t
        | `Set_send_email_to              of Email_address.V1.Set.t
        | `Set_send_email_upon            of Send_email_upon.V1.Set.t
        | `Set_who_can_release_into_me    of Who_can_release_into_me.V1.t
        | `Set_whole_feature_followers    of User_name.V1.Set.t
        | `Set_whole_feature_reviewers    of User_name.V1.Set.t
        | `Set_xcrs_shown_in_todo_only_for_users_reviewing of bool
        ]
      [@@deriving bin_io, sexp]

      let of_model m = m
      let to_model t = t
    end

    module Model = V8

    module V7 = struct
      type same_as_v8 =
        [ `Add_owners                     of User_name.V1.t list
        | `Add_reviewing                  of User_name.V1.Set.t
        | `Add_send_email_to              of Email_address.V1.Set.t
        | `Add_send_email_upon            of Send_email_upon.V1.Set.t
        | `Add_whole_feature_followers    of User_name.V1.Set.t
        | `Add_whole_feature_reviewers    of User_name.V1.Set.t
        | `Remove_owners                  of User_name.V1.Set.t
        | `Remove_reviewing               of User_name.V1.Set.t
        | `Remove_send_email_to           of Email_address.V1.Set.t
        | `Remove_send_email_upon         of Send_email_upon.V1.Set.t
        | `Remove_whole_feature_followers of User_name.V1.Set.t
        | `Remove_whole_feature_reviewers of User_name.V1.Set.t
        | `Set_base                       of Rev.V1.t
        | `Set_crs_are_enabled            of bool
        | `Set_crs_shown_in_todo_only_for_users_reviewing of bool
        | `Set_description                of string
        | `Set_is_permanent               of bool
        | `Set_owners                     of User_name.V1.t list
        | `Set_properties                 of Properties.V1.t
        | `Set_release_process            of Release_process.V1.t
        | `Set_review_is_enabled          of bool
        | `Set_reviewing                  of Reviewing.V1.t
        | `Set_send_email_to              of Email_address.V1.Set.t
        | `Set_send_email_upon            of Send_email_upon.V1.Set.t
        | `Set_who_can_release_into_me    of Who_can_release_into_me.V1.t
        | `Set_whole_feature_followers    of User_name.V1.Set.t
        | `Set_whole_feature_reviewers    of User_name.V1.Set.t
        | `Set_xcrs_shown_in_todo_only_for_users_reviewing of bool
        ]
      [@@deriving bin_io]

      type t =
        [ same_as_v8
        | `Remove_properties of string list
        ]
      [@@deriving bin_io]

      open! Core.Std
      open! Import

      let to_v8 (t: t) : V8.t =
        match t with
        | `Remove_properties properties ->
          `Remove_properties (Property.Set.of_list properties)
        | #same_as_v8 as x -> x
      ;;

      let to_model t = V8.to_model (to_v8 t)

      let of_v8 (v8: V8.t) : t =
        match v8 with
        | `Remove_properties properties -> `Remove_properties (Set.to_list properties)
        | #same_as_v8 as x -> x
        | _ -> assert_false__invariant_in_reaction [%here]
      ;;

      let of_model m = of_v8 (V8.of_model m)

    end

    module V6 = struct
      type t =
        [ `Add_owners                     of User_name.V1.t list
        | `Add_reviewing                  of User_name.V1.Set.t
        | `Add_send_email_to              of Email_address.V1.Set.t
        | `Add_send_email_upon            of Send_email_upon.V1.Set.t
        | `Add_whole_feature_reviewers    of User_name.V1.Set.t
        | `Remove_owners                  of User_name.V1.Set.t
        | `Remove_properties              of string list
        | `Remove_reviewing               of User_name.V1.Set.t
        | `Remove_send_email_to           of Email_address.V1.Set.t
        | `Remove_send_email_upon         of Send_email_upon.V1.Set.t
        | `Remove_whole_feature_reviewers of User_name.V1.Set.t
        | `Set_base                       of Rev.V1.t
        | `Set_crs_are_enabled            of bool
        | `Set_crs_shown_in_todo_only_for_users_reviewing of bool
        | `Set_description                of string
        | `Set_is_permanent               of bool
        | `Set_owners                     of User_name.V1.t list
        | `Set_properties                 of Properties.V1.t
        | `Set_release_process            of Release_process.V1.t
        | `Set_review_is_enabled          of bool
        | `Set_reviewing                  of Reviewing.V1.t
        | `Set_send_email_to              of Email_address.V1.Set.t
        | `Set_send_email_upon            of Send_email_upon.V1.Set.t
        | `Set_who_can_release_into_me    of Who_can_release_into_me.V1.t
        | `Set_whole_feature_reviewers    of User_name.V1.Set.t
        | `Set_xcrs_shown_in_todo_only_for_users_reviewing of bool
        ]
      [@@deriving bin_io]

      open! Core.Std
      open! Import

      let to_v7 (t : t) : V7.t =
        match t with
        | #t as x -> x
      ;;

      let to_model t = V7.to_model (to_v7 t)

      let of_v7 (v7 : V7.t) : t =
        match v7 with
        | #t as x -> x
        | _ -> assert_false__invariant_in_reaction [%here]
      ;;

      let of_model m = of_v7 (V7.of_model m)
    end

    module V5 = struct
      type same_as_v6 =
        [ `Add_owners                     of User_name.V1.t list
        | `Add_reviewing                  of User_name.V1.Set.t
        | `Add_whole_feature_reviewers    of User_name.V1.Set.t
        | `Remove_owners                  of User_name.V1.Set.t
        | `Remove_properties              of string list
        | `Remove_reviewing               of User_name.V1.Set.t
        | `Remove_whole_feature_reviewers of User_name.V1.Set.t
        | `Set_base                       of Rev.V1.t
        | `Set_crs_are_enabled            of bool
        | `Set_crs_shown_in_todo_only_for_users_reviewing of bool
        | `Set_description                of string
        | `Set_is_permanent               of bool
        | `Set_owners                     of User_name.V1.t list
        | `Set_properties                 of Properties.V1.t
        | `Set_release_process            of Release_process.V1.t
        | `Set_review_is_enabled          of bool
        | `Set_reviewing                  of Reviewing.V1.t
        | `Set_who_can_release_into_me    of Who_can_release_into_me.V1.t
        | `Set_whole_feature_reviewers    of User_name.V1.Set.t
        | `Set_xcrs_shown_in_todo_only_for_users_reviewing of bool
        ]
      [@@deriving bin_io]

      type t =
        [ same_as_v6
        | `Add_send_release_email_to      of Email_address.V1.Set.t
        | `Remove_send_release_email_to   of Email_address.V1.Set.t
        | `Set_send_release_email_to      of Email_address.V1.Set.t
        | `Set_should_send_release_email  of bool
        ]
      [@@deriving bin_io]

      open! Core.Std
      open! Import

      let to_v6 (t : t) : V6.t =
        match t with
        | `Add_send_release_email_to    emails -> `Add_send_email_to    emails
        | `Remove_send_release_email_to emails -> `Remove_send_email_to emails
        | `Set_send_release_email_to    emails -> `Set_send_email_to    emails
        | `Set_should_send_release_email bool  ->
          if bool
          then `Add_send_email_upon    Send_email_upon.(Set.singleton release)
          else `Remove_send_email_upon Send_email_upon.(Set.singleton release)
        | #same_as_v6 as x -> x
      ;;

      let to_model t = V6.to_model (to_v6 t)

      let of_v6 (v6 : V6.t) : t =
        match v6 with
        | #same_as_v6 as x -> x
        | _ -> assert_false__invariant_in_reaction [%here]
      ;;

      let of_model m = of_v6 (V6.of_model m)
    end

    module V4 = struct
      type t =
        [ `Add_owners                     of User_name.V1.t list
        | `Add_reviewing                  of User_name.V1.Set.t
        | `Add_send_release_email_to      of Email_address.V1.Set.t
        | `Add_whole_feature_reviewers    of User_name.V1.Set.t
        | `Remove_owners                  of User_name.V1.Set.t
        | `Remove_properties              of string list
        | `Remove_reviewing               of User_name.V1.Set.t
        | `Remove_send_release_email_to   of Email_address.V1.Set.t
        | `Remove_whole_feature_reviewers of User_name.V1.Set.t
        | `Set_base                       of Rev.V1.t
        | `Set_description                of string
        | `Set_is_permanent               of bool
        | `Set_owners                     of User_name.V1.t list
        | `Set_properties                 of Properties.V1.t
        | `Set_release_process            of Release_process.V1.t
        | `Set_review_is_enabled          of bool
        | `Set_reviewing                  of Reviewing.V1.t
        | `Set_send_release_email_to      of Email_address.V1.Set.t
        | `Set_should_send_release_email  of bool
        | `Set_who_can_release_into_me    of Who_can_release_into_me.V1.t
        | `Set_whole_feature_reviewers    of User_name.V1.Set.t
        ]
      [@@deriving bin_io]

      let to_v5 (t : t) = (t :> V5.t)

      let to_model t = V5.to_model (to_v5 t)

      let of_v5 : V5.t -> t = function
        | ( `Add_owners                     _
          | `Add_reviewing                  _
          | `Add_send_release_email_to      _
          | `Add_whole_feature_reviewers    _
          | `Remove_owners                  _
          | `Remove_properties              _
          | `Remove_reviewing               _
          | `Remove_send_release_email_to   _
          | `Remove_whole_feature_reviewers _
          | `Set_base                       _
          | `Set_description                _
          | `Set_is_permanent               _
          | `Set_owners                     _
          | `Set_properties                 _
          | `Set_release_process            _
          | `Set_review_is_enabled          _
          | `Set_reviewing                  _
          | `Set_send_release_email_to      _
          | `Set_should_send_release_email  _
          | `Set_who_can_release_into_me    _
          | `Set_whole_feature_reviewers    _
          ) as compatible -> compatible
        | `Set_crs_are_enabled _
        | `Set_crs_shown_in_todo_only_for_users_reviewing _
        | `Set_xcrs_shown_in_todo_only_for_users_reviewing _
          -> assert_false__invariant_in_reaction [%here]
      ;;

      let of_model m = of_v5 (V5.of_model m)
    end
  end

  module Action = struct
    module V8 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; updates      : Update.V8.t list
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end

    module Model = V8

    module V7 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; updates      : Update.V7.t list
        }
      [@@deriving bin_io]

      let to_model { feature_path; updates } =
        { Model.
          feature_path
        ; updates = List.map updates ~f:Update.V7.to_model
        }
      ;;
    end

    module V6 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; updates      : Update.V6.t list
        }
      [@@deriving bin_io]

      let to_model { feature_path; updates } =
        { Model.
          feature_path
        ; updates = List.map updates ~f:Update.V6.to_model
        }
      ;;
    end

    module V5 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; updates      : Update.V5.t list
        }
      [@@deriving bin_io]

      let to_model { feature_path; updates } =
        { Model.
          feature_path
        ; updates = List.map updates ~f:Update.V5.to_model
        }
      ;;
    end

    module V4 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; updates      : Update.V4.t list
        }
      [@@deriving bin_io]

      let to_model { feature_path; updates } =
        { Model.
          feature_path
        ; updates = List.map updates ~f:Update.V4.to_model
        }
      ;;
    end
  end

  module Reaction = struct
    module V8 = struct
      type t = (Update.V8.t * unit Or_error.V1.t) list
      [@@deriving bin_io, sexp]

      let of_model t = t
    end

    module Model = V8

    module V7 = struct
      type t = (Update.V7.t * unit Or_error.V1.t) list
      [@@deriving bin_io]

      let of_model m =
        List.map m ~f:(fun (update, result) -> Update.V7.of_model update, result)
      ;;
    end

    module V6 = struct
      type t = (Update.V6.t * unit Or_error.V1.t) list
      [@@deriving bin_io]

      let of_model m =
        List.map m ~f:(fun (update, result) -> Update.V6.of_model update, result)
      ;;
    end

    module V5 = struct
      type t = (Update.V5.t * unit Or_error.V1.t) list
      [@@deriving bin_io]

      let of_model m =
        List.map m ~f:(fun (update, result) -> Update.V5.of_model update, result)
      ;;
    end

    module V4 = struct
      type t = (Update.V4.t * unit Or_error.V1.t) list
      [@@deriving bin_io]

      let of_model t =
        List.map t ~f:(fun (update, result) -> Update.V4.of_model update, result)
      ;;
    end
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "change-feature" end)
    (struct let version = 8 end)
    (Stable.Action.V8)
    (Stable.Reaction.V8)

include Register_old_rpc
    (struct let version = 7 end)
    (Stable.Action.V7)
    (Stable.Reaction.V7)

include Register_old_rpc
    (struct let version = 6 end)
    (Stable.Action.V6)
    (Stable.Reaction.V6)

include Register_old_rpc
    (struct let version = 5 end)
    (Stable.Action.V5)
    (Stable.Reaction.V5)

include Register_old_rpc
    (struct let version = 4 end)
    (Stable.Action.V4)
    (Stable.Reaction.V4)

module Action   = Stable.Action.  Model
module Reaction = Stable.Reaction.Model
module Update   = Stable.Update.  Model
