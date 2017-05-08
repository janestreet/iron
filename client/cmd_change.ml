open! Core
open! Async
open! Import

let apply_updates_exn (type update) ~sexp_of_update ~rpc_to_server_exn
      ?(verbose = false) ~feature_path ~(updates : update list) () =
  if List.is_empty updates
  then raise_s [%sexp (sprintf "must request at least one change" : string)
                    , (feature_path : Feature_path.t)];
  let%map result = rpc_to_server_exn ~feature_path ~updates in
  let print result =
    result
    |> [%sexp_of: update * unit Or_error.t]
    |> Sexp.to_string_hum
    |> print_endline
  in
  let ok, errors = List.partition_tf result ~f:(fun (_, res) -> Result.is_ok res) in
  if verbose then List.iter ok ~f:print;
  if not (List.is_empty errors)
  then
    raise_s
      [%sexp "there were problems", (errors : (update * unit Or_error.t) list)];
;;

let change_feature ?verbose ~feature_path ~updates () =
  apply_updates_exn () ~feature_path ~updates ?verbose
    ~sexp_of_update:[%sexp_of: Change_feature.Update.t]
    ~rpc_to_server_exn:(fun ~feature_path ~updates ->
      Change_feature.rpc_to_server_exn { feature_path; updates })
;;

let command =

  Command.async'
    ~summary:"change a feature's attributes"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path =
       feature_path_or_current_bookmark
     and add_owners =
       user_list_option ~switch:"-add-owners"
     and add_reviewing =
       users_option ~switch:"-add-reviewing"
     and add_whole_feature_followers =
       users_option ~switch:"-add-whole-feature-followers"
     and add_whole_feature_reviewers =
       users_option ~switch:Switch.add_whole_feature_reviewers
     and lock =
       no_arg_flag "-lock" ~doc:"error -- use [fe lock] or [fe unlock]"
     and remove_owners =
       users_option ~switch:"-remove-owners"
     and remove_properties =
       properties_set_option ~switch:"-remove-property" ~verb:"remove"
     and remove_reviewing =
       users_option ~switch:"-remove-reviewing"
     and remove_whole_feature_followers =
       users_option ~switch:"-remove-whole-feature-followers"
     and remove_whole_feature_reviewers =
       users_option ~switch:"-remove-whole-feature-reviewers"
     and set_crs_are_enabled =
       flag "-set-crs-are-enabled" (optional bool) ~doc:"BOOL "
     and set_crs_shown_in_todo_only_for_users_reviewing =
       flag "-set-crs-shown-in-todo-only-for-users-reviewing"  (optional bool) ~doc:"BOOL "
     and set_xcrs_shown_in_todo_only_for_users_reviewing =
       flag "-set-xcrs-shown-in-todo-only-for-users-reviewing" (optional bool) ~doc:"BOOL "
     and set_owners =
       user_list_option ~switch:"-set-owners"
     and set_properties =
       property_values_flag
         ~switch:"-set-property" ~doc:"set user-defined properties"
     and set_reviewing =
       users_option ~switch:"-set-reviewing"
     and set_reviewing_all =
       no_arg_flag "-set-reviewing-all" ~doc:""
     and set_reviewing_none =
       no_arg_flag "-set-reviewing-none" ~doc:""
     and set_reviewing_whole_feature_only =
       no_arg_flag Switch.set_reviewing_whole_feature_only ~doc:""
     and set_whole_feature_followers =
       users_option ~switch:"-set-whole-feature-followers"
     and set_whole_feature_reviewers =
       users_option ~switch:"-set-whole-feature-reviewers"
     and set_base = set_base
     and set_description = set_description
     and set_is_permanent = set_is_permanent
     and set_release_process =
       enum_optional "-set-release-process"
         ~doc:"PROCESS how features are released into this one" (module Release_process)
     and set_who_can_release_into_me =
       enum_optional "-set-who-can-release-into-me"
         ~doc:"WHO owner who can release" (module Who_can_release_into_me)
     and set_send_email_to = email_addresses_option ~switch:"-set-send-email-to"
     and add_send_email_to = email_addresses_option ~switch:"-add-send-email-to"
     and remove_send_email_to = email_addresses_option ~switch:"-remove-send-email-to"
     and set_send_email_upon =
       send_email_upon ~switch:"-set-send-email-upon"
         ~doc:"cause email to be sent only upon ACTION(S)"
     and add_send_email_upon =
       send_email_upon ~switch:"-add-send-email-upon"
         ~doc:"cause email to be sent also upon ACTION(S)"
     and remove_send_email_upon =
       send_email_upon ~switch:"-remove-send-email-upon"
         ~doc:"cause email to not be sent upon ACTION(S)"
     and set_inheritable_crs_shown_in_todo_only_for_users_reviewing =
       flag "-set-inheritable-crs-shown-in-todo-only-for-users-reviewing"
         (optional bool) ~doc:"BOOL "
     and remove_inheritable_crs_shown_in_todo_only_for_users_reviewing =
       no_arg_flag "-remove-inheritable-crs-shown-in-todo-only-for-users-reviewing"
         ~doc:""
     and set_inheritable_xcrs_shown_in_todo_only_for_users_reviewing =
       flag "-set-inheritable-xcrs-shown-in-todo-only-for-users-reviewing"
         (optional bool) ~doc:"BOOL "
     and remove_inheritable_xcrs_shown_in_todo_only_for_users_reviewing =
       no_arg_flag
         "-remove-inheritable-xcrs-shown-in-todo-only-for-users-reviewing" ~doc:""
     and set_inheritable_who_can_release_into_me =
       enum_optional "-set-inheritable-who-can-release-into-me"
         ~doc:"" (module Who_can_release_into_me)
     and remove_inheritable_who_can_release_into_me =
       no_arg_flag "-remove-inheritable-who-can-release-into-me" ~doc:""
     and set_inheritable_owners =
       user_list_option ~switch:"set-inheritable-owners"
     and add_inheritable_owners =
       user_list_option ~switch:"add-inheritable-owners"
     and remove_inheritable_owners =
       users_option ~switch:"remove-inheritable-owners"
     and set_inheritable_properties =
       property_values_flag
         ~switch:"set-inheritable-property"
         ~doc:""
     and remove_inheritable_properties =
       inheritable_properties_set_option
         ~switch:"remove-inheritable-property"
     and set_inheritable_release_process =
       enum_optional
         "-set-inheritable-release-process" (module Release_process) ~doc:"PROCESS"
     and remove_inheritable_release_process =
       no_arg_flag "-remove-inheritable-release-process" ~doc:""
     and add_inheritable_send_email_to =
       email_addresses_option ~switch:"-add-inheritable-send-email-to"
     and remove_inheritable_send_email_to =
       email_addresses_option ~switch:"-remove-inheritable-send-email-to"
     and set_inheritable_send_email_to =
       email_addresses_option ~switch:"-set-inheritable-send-email-to"
     and add_inheritable_send_email_upon =
       send_email_upon ~switch:"-add-inheritable-send-email-upon" ~doc:""
     and remove_inheritable_send_email_upon =
       send_email_upon ~switch:"-remove-inheritable-send-email-upon" ~doc:""
     and set_inheritable_send_email_upon =
       send_email_upon ~switch:"-set-inheritable-send-email-upon" ~doc:""
     and set_inheritable_whole_feature_followers =
       users_option ~switch:"-set-inheritable-whole-feature-followers"
     and add_inheritable_whole_feature_followers =
       users_option ~switch:"-add-inheritable-whole-feature-followers"
     and remove_inheritable_whole_feature_followers =
       users_option ~switch:"-remove-inheritable-whole-feature-followers"
     and set_inheritable_whole_feature_reviewers =
       users_option ~switch:"-set-inheritable-whole-feature-reviewers"
     and add_inheritable_whole_feature_reviewers =
       users_option ~switch:"-add-inheritable-whole-feature-reviewers"
     and remove_inheritable_whole_feature_reviewers =
       users_option ~switch:"-remove-inheritable-whole-feature-reviewers"
     and set_lines_required_to_separate_ddiff_hunks =
       flag "-set-lines-required-to-separate-ddiff-hunks" (optional int)
         ~doc:""
     and verbose = verbose
     in
     fun () ->
       let open! Deferred.Let_syntax in
       if lock then failwith "use [fe lock] or [fe unlock]";
       let feature_path = ok_exn feature_path in
       let%bind set_base =
         match set_base with
         | None -> return None
         | Some rev ->
           let%bind repo_root =
             Cmd_workspace.repo_for_hg_operations_exn feature_path ~use:`Clone
           in
           Raw_rev.resolve_opt_exn (Some rev) ~in_:(Ok repo_root)
       in
       let updates =
         List.filter_opt
           [ Option.map add_owners
               ~f:(fun users -> `Add_owners users)
           ; Option.map add_reviewing
               ~f:(fun users -> `Add_reviewing users)
           ; Option.map add_whole_feature_followers
               ~f:(fun users -> `Add_whole_feature_followers users)
           ; Option.map add_whole_feature_reviewers
               ~f:(fun users -> `Add_whole_feature_reviewers users)
           ; Option.map remove_owners
               ~f:(fun users -> `Remove_owners users)
           ; Option.map remove_properties
               ~f:(fun attrs -> `Remove_properties attrs)
           ; Option.map remove_reviewing
               ~f:(fun users -> `Remove_reviewing users)
           ; Option.map remove_whole_feature_followers
               ~f:(fun users -> `Remove_whole_feature_followers users)
           ; Option.map remove_whole_feature_reviewers
               ~f:(fun users -> `Remove_whole_feature_reviewers users)
           ; Option.map set_crs_are_enabled
               ~f:(fun bool -> `Set_crs_are_enabled bool)
           ; Option.map set_crs_shown_in_todo_only_for_users_reviewing
               ~f:(fun bool -> `Set_crs_shown_in_todo_only_for_users_reviewing bool)
           ; Option.map set_xcrs_shown_in_todo_only_for_users_reviewing
               ~f:(fun bool -> `Set_xcrs_shown_in_todo_only_for_users_reviewing bool)
           ; Option.map set_owners
               ~f:(fun users -> `Set_owners users)
           ; Option.bind set_properties ~f:(fun properties ->
               if Map.is_empty properties
               then None
               else Some (`Set_properties properties))
           ; Option.map set_reviewing
               ~f:(fun users ->
                 let reviewing =
                   match List.map (Set.to_list users) ~f:User_name.to_string with
                   | [ "all" ] -> `All
                   | [ "whole-feature-reviewers" ] -> `Whole_feature_reviewers
                   | _ -> `Only users
                 in
                 `Set_reviewing reviewing)
           ; if set_reviewing_all
             then Some (`Set_reviewing `All)
             else None
           ; if set_reviewing_none
             then Some (`Set_reviewing (`Only User_name.Set.empty))
             else None
           ; if set_reviewing_whole_feature_only
             then Some (`Set_reviewing `Whole_feature_reviewers)
             else None
           ; Option.map set_whole_feature_followers
               ~f:(fun users -> `Set_whole_feature_followers users)
           ; Option.map set_whole_feature_reviewers
               ~f:(fun users -> `Set_whole_feature_reviewers users)
           ; Option.map set_base         ~f:(fun rev  -> `Set_base rev)
           ; Option.map set_is_permanent ~f:(fun bool -> `Set_is_permanent bool)
           ; Option.map set_description  ~f:(fun str  -> `Set_description str)
           ; Option.map set_release_process
               ~f:(fun how -> `Set_release_process how)
           ; Option.map set_who_can_release_into_me
               ~f:(fun who -> `Set_who_can_release_into_me who)
           ; Option.map set_send_email_to
               ~f:(fun whom -> `Set_send_email_to whom)
           ; Option.map add_send_email_to
               ~f:(fun whom -> `Add_send_email_to whom)
           ; Option.map remove_send_email_to
               ~f:(fun whom -> `Remove_send_email_to whom)
           ; Option.map set_send_email_upon
               ~f:(fun send_email_upon -> `Set_send_email_upon send_email_upon)
           ; Option.map add_send_email_upon
               ~f:(fun send_email_upon -> `Add_send_email_upon send_email_upon)
           ; Option.map remove_send_email_upon
               ~f:(fun send_email_upon -> `Remove_send_email_upon send_email_upon)
           ; Option.map set_inheritable_crs_shown_in_todo_only_for_users_reviewing
               ~f:(fun option ->
                 `Set_inheritable_crs_shown_in_todo_only_for_users_reviewing
                   (Option.return option))
           ; if remove_inheritable_crs_shown_in_todo_only_for_users_reviewing
             then Some (`Set_inheritable_crs_shown_in_todo_only_for_users_reviewing None)
             else None
           ; Option.map set_inheritable_xcrs_shown_in_todo_only_for_users_reviewing
               ~f:(fun option ->
                 `Set_inheritable_xcrs_shown_in_todo_only_for_users_reviewing
                   (Option.return option))
           ; if remove_inheritable_xcrs_shown_in_todo_only_for_users_reviewing
             then Some (`Set_inheritable_xcrs_shown_in_todo_only_for_users_reviewing None)
             else None
           ; Option.map set_inheritable_owners
               ~f:(fun owners -> `Set_inheritable_owners owners)
           ; Option.map add_inheritable_owners
               ~f:(fun owners -> `Add_inheritable_owners owners)
           ; Option.map remove_inheritable_owners
               ~f:(fun owners -> `Remove_inheritable_owners owners)
           ; Option.map set_inheritable_properties
               ~f:(fun attrs -> `Set_inheritable_properties attrs)
           ; Option.map remove_inheritable_properties
               ~f:(fun attrs -> `Remove_inheritable_properties attrs)
           ; Option.map set_inheritable_release_process
               ~f:(fun how -> `Set_inheritable_release_process (Option.return how))
           ; if remove_inheritable_release_process
             then Some (`Set_inheritable_release_process None)
             else None
           ; Option.map set_inheritable_who_can_release_into_me
               ~f:(fun whom ->
                 `Set_inheritable_who_can_release_into_me (Option.return whom))
           ; if remove_inheritable_who_can_release_into_me
             then Some (`Set_inheritable_who_can_release_into_me None)
             else None
           ; Option.map set_inheritable_send_email_to
               ~f:(fun send_email_to ->
                 `Set_inheritable_send_email_to send_email_to)
           ; Option.map add_inheritable_send_email_to
               ~f:(fun send_email_to ->
                 `Add_inheritable_send_email_to send_email_to)
           ; Option.map remove_inheritable_send_email_to
               ~f:(fun send_email_to ->
                 `Remove_inheritable_send_email_to send_email_to)
           ; Option.map set_inheritable_send_email_upon
               ~f:(fun send_email_upon ->
                 `Set_inheritable_send_email_upon send_email_upon)
           ; Option.map add_inheritable_send_email_upon
               ~f:(fun send_email_upon ->
                 `Add_inheritable_send_email_upon send_email_upon)
           ; Option.map remove_inheritable_send_email_upon
               ~f:(fun send_email_upon ->
                 `Remove_inheritable_send_email_upon send_email_upon)
           ; Option.map set_inheritable_whole_feature_followers
               ~f:(fun whole_feature_followers ->
                 `Set_inheritable_whole_feature_followers whole_feature_followers)
           ; Option.map add_inheritable_whole_feature_followers
               ~f:(fun whole_feature_followers ->
                 `Add_inheritable_whole_feature_followers whole_feature_followers)
           ; Option.map remove_inheritable_whole_feature_followers
               ~f:(fun whole_feature_followers ->
                 `Remove_inheritable_whole_feature_followers whole_feature_followers)
           ; Option.map set_inheritable_whole_feature_reviewers
               ~f:(fun whole_feature_reviewers ->
                 `Set_inheritable_whole_feature_reviewers whole_feature_reviewers)
           ; Option.map add_inheritable_whole_feature_reviewers
               ~f:(fun whole_feature_reviewers ->
                 `Add_inheritable_whole_feature_reviewers whole_feature_reviewers)
           ; Option.map remove_inheritable_whole_feature_reviewers
               ~f:(fun whole_feature_reviewers ->
                 `Remove_inheritable_whole_feature_reviewers whole_feature_reviewers)
           ; Option.map set_lines_required_to_separate_ddiff_hunks
               ~f:(fun lines -> `Set_lines_required_to_separate_ddiff_hunks lines)
           ]
       in
       change_feature ~verbose ~feature_path ~updates ()
    )
;;
