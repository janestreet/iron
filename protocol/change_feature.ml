module Stable = struct

  open! Import_stable

  (* The [of_model] functions are called only to build a Reaction containing an echo of
     the updates sent in the first place by an old client.  As a result, failing on
     unknown constructor is fine because the old clients cannot have sent these updates in
     the first place, so this is an unreachable execution path. *)
  let assert_false__invariant_in_reaction here =
    let open Core in
    raise_s [%sexp "assert false", (here : Source_code_position.t)]
  ;;

  module Update = struct

    module V9 = struct
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
        | `Set_lines_required_to_separate_ddiff_hunks of int
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

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| dfd087aeda4ee8fb19f8d8018c525aea |}]
      ;;

      let of_model m = m
      let to_model t = t
    end

    module Model = V9

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
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 4b92dfa291675eae0afdb7e0c543bc85 |}]
      ;;

      open! Core
      open! Import

      let to_v9 (t: t) : V9.t = (t :> V9.t)
      let to_model t = V9.to_model (to_v9 t)

      let of_v9 (v9: V9.t) : t =
        match v9 with
        | `Set_lines_required_to_separate_ddiff_hunks _ ->
          assert_false__invariant_in_reaction [%here]
        | #t as x -> x
      ;;

      let of_model m = of_v9 (V9.of_model m)
    end

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

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| a26cf217411c9e99163d0c090a225dcf |}]
      ;;

      open! Core
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

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 116c17197d022d295bcf781cfb312c6f |}]
      ;;

      open! Core
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

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 19aea7c181b2b614f3b27aacffc4fff8 |}]
      ;;

      open! Core
      open! Import

      let to_v6 (t : t) : V6.t =
        match t with
        | `Add_send_release_email_to    emails -> `Add_send_email_to    emails
        | `Remove_send_release_email_to emails -> `Remove_send_email_to emails
        | `Set_send_release_email_to    emails -> `Set_send_email_to    emails
        | `Set_should_send_release_email bool  ->
          if bool
          then `Add_send_email_upon    Send_email_upon.(Set.singleton Release)
          else `Remove_send_email_upon Send_email_upon.(Set.singleton Release)
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

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ca2e9fa9a1d1bc5c657c03d7ba0d08e0 |}]
      ;;

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
    module V9 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; updates      : Update.V9.t list
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 5aae484f655442fa1fceee3f1118a5d4 |}]
      ;;

      let to_model t = t
    end

    module Model = V9

    module V8 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; updates      : Update.V8.t list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| a3b71c6e7695036b2a2dbb4548f9cd6e |}]
      ;;

      let to_model { feature_path; updates } =
        { Model.
          feature_path
        ; updates = List.map updates ~f:Update.V8.to_model
        }
      ;;
    end

    module V7 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; updates      : Update.V7.t list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 6a80a6f0e53447b748fa69c1bddd5d8f |}]
      ;;

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

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 9e5c6c83177b8c5e66d1f7049520f750 |}]
      ;;

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

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 53c9c17e2dff39843ac680648e013cd4 |}]
      ;;

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

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d6c824cc46e69c5434f75ccffb6b6675 |}]
      ;;

      let to_model { feature_path; updates } =
        { Model.
          feature_path
        ; updates = List.map updates ~f:Update.V4.to_model
        }
      ;;
    end
  end

  module Reaction = struct
    module V9 = struct
      type t = (Update.V9.t * unit Or_error.V2.t) list
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| bcbe701298c3feab2e636e4c35ec5688 |}]
      ;;

      let of_model t = t
    end

    module Model = V9

    module V8 = struct
      type t = (Update.V8.t * unit Or_error.V1.t) list
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 314feff5980b9d9d545057496442d283 |}]
      ;;

      let of_model m =
        List.map m ~f:(fun (update, result) -> Update.V8.of_model update, result)
      ;;
    end

    module V7 = struct
      type t = (Update.V7.t * unit Or_error.V1.t) list
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 03006f59b742e4eb78a176b0a8b3d038 |}]
      ;;

      let of_model m =
        List.map m ~f:(fun (update, result) -> Update.V7.of_model update, result)
      ;;
    end

    module V6 = struct
      type t = (Update.V6.t * unit Or_error.V1.t) list
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ee55e94531368fa9ad6216e33bce9304 |}]
      ;;

      let of_model m =
        List.map m ~f:(fun (update, result) -> Update.V6.of_model update, result)
      ;;
    end

    module V5 = struct
      type t = (Update.V5.t * unit Or_error.V1.t) list
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 540ec31aefd9e16a8f2693c9fa7f37d4 |}]
      ;;

      let of_model m =
        List.map m ~f:(fun (update, result) -> Update.V5.of_model update, result)
      ;;
    end

    module V4 = struct
      type t = (Update.V4.t * unit Or_error.V1.t) list
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 32ed3ec908aa6e3cd87a074c3a21c5d2 |}]
      ;;

      let of_model t =
        List.map t ~f:(fun (update, result) -> Update.V4.of_model update, result)
      ;;
    end
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "change-feature" end)
    (struct let version = 9 end)
    (Stable.Action.V9)
    (Stable.Reaction.V9)

include Register_old_rpc
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
