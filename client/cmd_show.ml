open! Core.Std
open! Async.Std
open! Import

let pending_for ~since =
  concat [ "pending for "; Time.Span.to_short_string (how_long ~since) ]
;;

let underline char line =
  sprintf "%s\n%s\n" line (String.make (String.length line) char)
;;

let header feature_path =
  sprintf "%s" (underline '=' (Feature_path.to_string feature_path));
;;

let render_description description =
  let lines =
    String.split_lines description
    |> List.filter ~f:(fun line ->
      let line = String.lstrip line in
      String.is_empty line || not (Char.equal line.[0] '#'))
  in
  let description = String.concat lines ~sep:"\n" in
  if String.is_empty description
  then description
  else concat [ description; "\n" ]
;;

let render_included_features feature ~display_ascii ~max_output_columns
      ~show_attribute_table ~show_description ~show_included_feature_details
      ~included_features_order =
  let included_features =
    Feature.released_features feature ~sorted_by:included_features_order
  in
  let maybe should_show f = if should_show then f () else [] in
  if List.is_empty included_features
  then ""
  else
    concat
      ([ "\nIncluded features:\n" ]
       @ (List.map included_features ~f:(fun (r : Released_feature.t) ->
         sprintf "  %s\n" (Feature_path.to_string r.feature_path)))
       @ maybe show_included_feature_details (fun () ->
         List.concat_map included_features ~f:(fun (r : Released_feature.t) ->
           [ sprintf "\n%s" (header r.feature_path) ]
           @ maybe show_description (fun () -> [ render_description r.description ])
           @ maybe show_attribute_table (fun () ->
             [ sprintf "\n%s"
                 (Ascii_table.to_string (Released_feature.attribute_table r) ~display_ascii
                    ~max_output_columns)]))))
;;

module Attribute = struct
  module User_defined = struct
    type t = string [@@deriving sexp]
    let all = [] (* can't enumerate user defined attributes, so don't try *)
  end
  module T = struct
    type t =
      | Allow_review_for
      | Base
      | Crs_are_enabled
      | Crs_shown_in_todo_only_for_users_reviewing
      | Feature_path
      | First_owner
      | Has_bookmark
      | Id
      | Included_features
      | Is_permanent
      | Is_archived
      | Is_seconded
      | Next_base_update
      | Next_bookmark_update
      | Next_steps
      | Owners
      | Release_process
      | Remote_repo_path
      | Rev_zero
      | Review_is_enabled
      | Reviewers
      | Reviewing
      | Seconder
      | Send_email_to
      | Send_email_upon
      | Tip
      | User_defined of User_defined.t
      | Users_with_review_session_in_progress
      | Users_with_unclean_workspaces
      | What_is_locked
      | Who_can_release_into_me
      | Who_can_review
      | Whole_feature_followers
      | Whole_feature_reviewers
      | Xcrs_shown_in_todo_only_for_users_reviewing
    [@@deriving enumerate, sexp]
  end

  include T

  let args =
    let open Command.Let_syntax in
    let%map_open () = return ()
    and ts = enum_no_args (module T) ~doc:(fun ~name _ -> sprintf "show %s" name)
    and user_defined = properties_option ~switch:"property" ~verb:"show"
    in
    let user_defined =
      match user_defined with
      | None -> []
      | Some names -> List.map names ~f:(fun name -> User_defined name)
    in
    ts @ user_defined
  ;;

  let%test _ =
    String.(=)
      ("-" ^ Enum.to_string_hum (module T) What_is_locked)
      Switch.what_is_locked
  ;;

  let users users = users |> [%sexp_of: User_name.t list]
  let user_set set = set |> Set.to_list |> users

  let get t (feature : Feature.t) ~included_features_order =
    match t with
    | Allow_review_for        ->
      feature.allow_review_for
      |> [%sexp_of: Allow_review_for.t]
    | Base                    -> Rev.node_hash feature.base |> [%sexp_of: Node_hash.t]
    | Crs_are_enabled         ->
      feature.crs_are_enabled
      |> [%sexp_of: bool]
    | Crs_shown_in_todo_only_for_users_reviewing ->
      feature.crs_shown_in_todo_only_for_users_reviewing
      |> [%sexp_of: bool]
    | Feature_path            -> feature.feature_path |> [%sexp_of: Feature_path.t]
    | First_owner             -> List.hd_exn feature.owners |> [%sexp_of: User_name.t]
    | Has_bookmark            -> feature.has_bookmark |> [%sexp_of: bool]
    | Id                      -> feature.feature_id |> [%sexp_of: Feature_id.t]
    | Included_features       ->
      List.map (Feature.released_features feature ~sorted_by:included_features_order)
        ~f:Released_feature.feature_path
      |> [%sexp_of: Feature_path.t list]
    | Is_permanent            -> feature.is_permanent |> [%sexp_of: bool]
    | Is_archived             -> feature.is_archived |> [%sexp_of: bool]
    | Is_seconded             -> is_some feature.seconder |> [%sexp_of: bool]
    | Next_base_update ->
      feature.next_base_update |> [%sexp_of: Next_base_update.t]
    | Next_bookmark_update  ->
      feature.next_bookmark_update |> [%sexp_of: Next_bookmark_update.t]
    | Owners                  -> users feature.owners
    | Release_process         ->
      feature.release_process |> [%sexp_of: Release_process.t]
    | Remote_repo_path        ->
      feature.remote_repo_path |> [%sexp_of: Remote_repo_path.t]
    | Rev_zero -> feature.rev_zero |> [%sexp_of: Rev.t]
    | Review_is_enabled       ->
      feature.review_is_enabled
      |> [%sexp_of: bool]
    | Reviewers -> Feature.reviewers_exn feature ~sort:`Decreasing_review |> users
    | Reviewing               -> Reviewing.to_sexp_hum feature.reviewing
    | Seconder                ->
      begin match feature.seconder with
      | None -> "not seconded" |> [%sexp_of: string]
      | Some user -> user |> [%sexp_of: User_name.t]
      end
    | Send_email_to           ->
      feature.send_email_to |> [%sexp_of: Email_address.Set.t]
    | Send_email_upon         ->
      feature.send_email_upon |> [%sexp_of: Send_email_upon.Set.t]
    | Tip                     -> Rev.node_hash feature.tip |> [%sexp_of: Node_hash.t]
    | What_is_locked          ->
      feature.locked |> [%sexp_of: (Lock_name.t * Feature.Locked.t list) list]
    | Who_can_release_into_me ->
      feature.who_can_release_into_me |> [%sexp_of: Who_can_release_into_me.t]
    | Who_can_review          -> Feature.who_can_review_exn feature |> user_set
    | Whole_feature_followers -> feature.whole_feature_followers    |> user_set
    | Whole_feature_reviewers -> feature.whole_feature_reviewers    |> user_set
    | Next_steps              -> feature.next_steps |> [%sexp_of: Next_step.t list]
    | User_defined key        ->
      begin match Hashtbl.find feature.properties key with
      | None -> raise_s [%sexp "undefined property", (key : string)]
      | Some sexp -> sexp
      end
    | Users_with_review_session_in_progress ->
      begin match feature.users_with_uncommitted_session with
      | Ok users  -> users |> user_set
      | Error err -> Error.raise err
      end
    | Users_with_unclean_workspaces ->
      feature.users_with_unclean_workspaces
      |> Map.keys
      |> users
    | Xcrs_shown_in_todo_only_for_users_reviewing ->
      feature.xcrs_shown_in_todo_only_for_users_reviewing
      |> [%sexp_of: bool]
  ;;
end

let print_table ascii_table ~display_ascii ~max_output_columns =
  if not (Ascii_table.is_empty ascii_table) then begin
    print_newline ();
    print_string (Ascii_table.to_string ascii_table ~display_ascii ~max_output_columns)
  end
;;

let user_names_to_string user_names =
  concat ~sep:", " (List.map user_names ~f:User_name.to_string)
;;

let user_list name users =
  [ match users with
    | [ user ] -> (name, ([], User_name.to_string user))
    | users -> (concat [ name; "s"], ([], user_names_to_string users))
  ]
;;

let user_set name users = user_list name (Set.to_list users)

let bool_or_error attribute = function
  | Error _ -> [ attribute, ([`Red], "error") ]
  | Ok bool ->
    if bool
    then []
    else [ attribute, ([`Red], Bool.to_string bool) ]
;;

let or_pending ~is_archived name or_pending rows_a =
  match or_pending with
  | Known a -> rows_a a
  | Pending_since since ->
    if is_archived
    then [] (* It will not be processed by hydra *)
    else [ name, ([ `Yellow ], pending_for ~since) ]
;;

let rev_facts ~is_archived =
  let maybe opt f = Option.value_map opt ~default:[] ~f in
  fun ?update_expected_since name rev rev_facts_or_pending ->
    [ name,
      begin match update_expected_since with
      | None -> [], Rev.to_string_hum rev
      | Some since ->
        [`Yellow], sprintf "%s (%s)" (Rev.to_string_hum rev) (pending_for ~since)
      end
    ]
    @ maybe rev_facts_or_pending (fun rev_facts_or_pending ->
      or_pending ~is_archived (concat [ name; " facts" ]) rev_facts_or_pending
        (fun rev_facts ->
           match Rev_facts.check rev_facts rev with
           | Ok true -> []
           | Ok false | Error _ ->
             let { Rev_facts.
                   is_conflict_free
                 ; is_cr_clean
                 ; obligations_are_valid
                 } = rev_facts
             in
             let fact attr fact check =
               bool_or_error (concat [ "  "; name; " "; attr ]) (check fact rev)
             in
             List.concat
               [ fact "is conflict free" is_conflict_free
                   Rev_facts.Is_conflict_free.check
               ; fact "obligations are valid" obligations_are_valid
                   Rev_facts.Obligations_are_valid.check
               ; fact "is cr clean" is_cr_clean
                   Rev_facts.Is_cr_clean.check
               ]))
;;

let reviewing_to_string = function
  | `All -> "all"
  | `Whole_feature_reviewers -> "whole-feature reviewers"
  | `Only user_names -> user_names_to_string (Set.to_list user_names)
  | `All_but user_names ->
    concat [ "all except: "; user_names_to_string (Set.to_list user_names) ]
;;

let block name = function
  | [] -> []
  | _ :: _ as inside ->
    (name, ([], "")) :: List.map inside ~f:(fun (key, value) -> ("  " ^ key, value))
;;

let attribute_table_with_fields ~display_ascii ~max_output_columns ~next_steps
      ?feature_id
      ?whole_feature_followers
      ?whole_feature_reviewers
      ?owners
      ?base
      ?base_facts
      ?next_base_update
      ?crs_are_enabled
      ?crs_shown_in_todo_only_for_users_reviewing
      ?xcrs_shown_in_todo_only_for_users_reviewing
      ?next_bookmark_update
      ?has_bookmark
      ?tip
      ?tip_facts
      ?base_is_ancestor_of_tip
      ?is_permanent
      ?is_archived
      ?seconder
      ?review_is_enabled
      ?reviewing
      ?properties
      ?has_children
      ?release_process
      ?who_can_release_into_me
      ?send_email_to
      ?send_email_upon
      ?locked
      ?(show_lock_reasons=false)
      ?(show_is_archived_if_not_archived=false)
      ()
  =
  let rows =
    let maybe opt f = Option.value_map opt ~default:[] ~f in
    let maybe3 a b c f =
      match a, b, c with
      | Some a, Some b, Some c -> f a b c
      | _, _, _ -> []
    in
    let in_todo_for crs user_reviewing_only =
      [ crs ^ " shown in todo for",
        ([],
         if user_reviewing_only
         then "users reviewing only"
         else "all"
        ) ]
    in
    let is_known_to_be_archived = Option.value is_archived ~default:false in
    let rev_facts  = rev_facts  ~is_archived:is_known_to_be_archived in
    let or_pending = or_pending ~is_archived:is_known_to_be_archived in
    List.concat
      [ maybe feature_id (fun feature_id ->
          [ "id", ([], Feature_id.to_string feature_id )])
      ; maybe is_archived (fun is_archived ->
          if is_archived || show_is_archived_if_not_archived
          then [ "is archived", ([ `Yellow ], Bool.to_string is_archived) ]
          else [])
      ; maybe next_steps (if is_known_to_be_archived then const [] else (fun next_steps ->
          [ "next step",
            let next_steps =
              if List.is_empty next_steps
              then [ Next_step.Report_iron_bug ]
              else next_steps
            in
            Next_step.to_attrs_and_string next_steps
              ~review_is_enabled:(Option.value ~default:false review_is_enabled)
          ]))
      ; maybe owners (user_list "owner")
      ; maybe whole_feature_reviewers (user_set "whole-feature reviewer")
      ; maybe seconder (fun seconder ->
        [ "seconder",
          ([],
           match seconder with
           | None -> "not seconded"
           | Some seconder ->
             sprintf "%s%s" (User_name.to_string seconder)
               (match owners with
                | None -> ""
                | Some owners ->
                  if List.mem owners seconder ~equal:User_name.equal
                  then " (even though owner)"
                  else ""))
        ])
      ; maybe review_is_enabled (fun review_is_enabled ->
          [ "review is enabled", ([], Bool.to_string review_is_enabled) ])
      ; maybe crs_are_enabled (fun crs_are_enabled ->
          if not (Option.value review_is_enabled ~default:false)
             || not crs_are_enabled
          then [ "CRs are enabled", ([], Bool.to_string crs_are_enabled) ]
          else [])
      ; maybe crs_shown_in_todo_only_for_users_reviewing
          (fun crs_shown_in_todo_only_for_users_reviewing ->
             if Bool.equal
                  crs_shown_in_todo_only_for_users_reviewing
                  Feature.Default_values.crs_shown_in_todo_only_for_users_reviewing
             then []
             else in_todo_for "CRs" crs_shown_in_todo_only_for_users_reviewing)
      ; maybe xcrs_shown_in_todo_only_for_users_reviewing
          (fun xcrs_shown_in_todo_only_for_users_reviewing ->
             if Bool.equal
                  xcrs_shown_in_todo_only_for_users_reviewing
                  Feature.Default_values.xcrs_shown_in_todo_only_for_users_reviewing
             then []
             else in_todo_for "XCRs" xcrs_shown_in_todo_only_for_users_reviewing)
      ; maybe reviewing (fun reviewing ->
          [ "reviewing", ([], reviewing_to_string reviewing) ])
      ; maybe whole_feature_followers (fun whole_feature_followers ->
          if Set.is_empty whole_feature_followers
          then []
          else user_set "whole-feature follower" whole_feature_followers)
      ; maybe is_permanent (fun is_permanent ->
          [ "is permanent", ([], Bool.to_string is_permanent) ])
      ; maybe next_bookmark_update (fun next_bookmark_update ->
          let show color string = [ "bookmark update", ([ color ], string) ] in
          match (next_bookmark_update : Next_bookmark_update.t) with
          | No_update_expected -> []
          | No_update_expected_due_to_iron_bug _ -> show `Red "Iron bug"
          | Update_expected_since since ->
            if is_known_to_be_archived
            then []
            else show `Yellow (pending_for ~since))
      (* We show [has bookmark] only if the bookmark is missing.  Although showing [has
         bookmark = false] is redundant with showing [next step = restore bookmark], we
         show it here in case there is a bug or change in [next step] that causes it to be
         something other than [restore bookmark]. *)
      ; maybe has_bookmark
          (let show color string = [ "has bookmark", ([ color ], string) ] in
           function
           | true -> []
           | false -> show `Red "false")
      ; maybe tip  (fun tip  -> rev_facts "tip"  tip  tip_facts)
      ; maybe base (fun base ->
          let update_expected_since =
            match (next_base_update : Next_base_update.t option) with
            | Some Update_expected { expected_since; _} -> Some expected_since
            | None | Some No_update_expected -> None
          in
          rev_facts "base" base base_facts ?update_expected_since)
      ; maybe3 base tip base_is_ancestor_of_tip (fun base tip base_is_ancestor_of_tip ->
          or_pending "base is ancestor of tip" base_is_ancestor_of_tip
            (fun is_ancestor ->
               bool_or_error "base is ancestor of tip"
                 (Rev_facts.Is_ancestor.check is_ancestor
                    ~ancestor:base ~descendant:tip)))
      ; block "release into me"
          (List.concat
             [ begin match has_children with
                 | None | Some false -> []
                 | Some true ->
                   List.concat
                     [ maybe release_process (fun release_process ->
                         [ "release process"
                         , ([], Release_process.to_string_hum release_process)
                         ])
                     ; maybe who_can_release_into_me (fun who_can_release_into_me ->
                         [ "who can release"
                         , ([], Who_can_release_into_me.to_string_hum_as_parent
                                  who_can_release_into_me)
                         ])
                     ]
               end
               ;
             ])
      ; maybe send_email_upon (fun send_email_upon ->
          if Set.equal send_email_upon Send_email_upon.default
          then []
          else [ "send email upon"
               , ([], (send_email_upon
                       |> Set.to_list
                       |> List.map ~f:Send_email_upon.to_string
                       |> String.concat ~sep:", "))
               ])
      ; maybe send_email_to (fun send_email_to ->
          if Set.is_empty send_email_to
          then []
          else
            let attrs =
              let enabled =
                match send_email_upon with
                | None -> false
                | Some set -> not (Set.is_empty set)
              in
              if enabled then [] else [ `Dim ]
            in
            [ "send email to"
            , (attrs, (send_email_to
                       |> Set.to_list
                       |> List.map ~f:Email_address.to_string
                       |> String.concat ~sep:", "))
            ])
      ; maybe properties (fun properties ->
          List.sort (Hashtbl.to_alist properties)
            ~cmp:(fun (key1, _) (key2, _) -> String.compare key1 key2)
          |> List.map ~f:(fun (key, value) -> (key, ([], Sexp.to_string value))))
      ; block "locks"
          (maybe locked (fun locked ->
             List.concat_map locked ~f:(fun (lock, locks) ->
               List.map locks ~f:(fun (locked : Feature.Locked.t) ->
                 let key = sprintf "%s locked by" (Lock_name.to_string_hum lock) in
                 let value =
                   let by = User_name.to_string locked.by in
                   let reason = String.strip locked.reason in
                   if show_lock_reasons && not (String.is_empty reason)
                   then sprintf "%s: %s" by locked.reason
                   else by
                 in
                 key, ([], value)))
             |> List.sort ~cmp:(
               Comparable.lexicographic
                 [ (fun (lock_name1, _) (lock_name2, _) -> String.compare lock_name1 lock_name2)
                 ; (fun (_, (_, user1)) (_, (_, user2)) -> String.compare user1 user2)
                 ]
             )))
      ]
  in
  let columns =
    Ascii_table.Column.(
      [ string ~header:"attribute" (cell fst)
      ; string ~header:"value"     (attr_cell snd)
      ])
  in
  Ascii_table.to_string (Ascii_table.create ~columns ~rows)
    ~display_ascii
    ~max_output_columns;
;;

let attribute_table ~display_ascii ~max_output_columns
      ~show_feature_id ~show_lock_reasons ~show_next_steps
      { Feature.
        feature_id
      ; feature_path              = _
      ; rev_zero                  = _
      ; whole_feature_followers
      ; whole_feature_reviewers
      ; owners
      ; base
      ; base_facts
      ; next_base_update
      ; crs_are_enabled
      ; crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; next_bookmark_update
      ; has_bookmark
      ; tip
      ; tip_facts
      ; allow_review_for          = _
      ; base_is_ancestor_of_tip
      ; diff_from_base_to_tip     = _
      ; description               = _
      ; is_permanent
      ; is_archived
      ; seconder
      ; review_is_enabled
      ; reviewing
      ; has_children
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; included_features         = _
      ; properties
      ; remote_repo_path          = _
      ; locked
      ; line_count_by_user        = _
      ; cr_summary                = _
      ; next_steps
      ; users_with_uncommitted_session = _
      ; users_with_unclean_workspaces = _
      ; latest_release = _
      } =
  attribute_table_with_fields ~display_ascii ~max_output_columns
    ?feature_id:(if (show_feature_id || is_archived) then Some feature_id else None)
    ~next_steps:(Option.some_if show_next_steps next_steps)
    ~whole_feature_followers
    ~whole_feature_reviewers
    ~owners
    ~base
    ~base_facts
    ~next_base_update
    ~crs_are_enabled
    ~crs_shown_in_todo_only_for_users_reviewing
    ~xcrs_shown_in_todo_only_for_users_reviewing
    ~next_bookmark_update
    ~has_bookmark
    ~tip
    ~tip_facts
    ~base_is_ancestor_of_tip
    ~is_permanent
    ~seconder
    ~review_is_enabled
    ~reviewing
    ~properties
    ~has_children
    ~release_process
    ~who_can_release_into_me
    ~send_email_to
    ~send_email_upon
    ~locked
    ~show_lock_reasons
    ~is_archived
    ~show_is_archived_if_not_archived:false
    ()
;;

let print_cr_table cr_summary ~display_ascii ~max_output_columns =
  match Cr_comment.Summary.to_ascii_table cr_summary with
  | None -> ()
  | Some table -> print_table ~display_ascii ~max_output_columns table
;;

let print_line_count_table line_count_by_user
      ~display_ascii ~max_output_columns ~show_completed_review =
  print_table (Line_count_table.create line_count_by_user ~show_completed_review)
    ~display_ascii ~max_output_columns
;;

let uncommitted_sessions_table user_set =
  let rows = Set.to_list user_set in
  let columns =
    Ascii_table.Column.(
      [ of_to_string ~header:"review sessions in progress" User_name.to_string
          (cell (fun name -> name)) ])
  in
  Ascii_table.create ~columns ~rows
;;

let print_uncommitted_sessions_table ~display_ascii ~max_output_columns user_set =
  print_table (uncommitted_sessions_table user_set)
    ~display_ascii ~max_output_columns
;;

let unclean_workspaces_table users =
  let rows = Map.to_alist users in
  let columns =
    Ascii_table.Column.(
      [ of_to_string ~header:"unclean workspaces" User_name.to_string (cell fst)
      ; string       ~header:"reason" (attr_cell (fun (_, reason) ->
          Cmd_workspace_unclean.unclean_workspace_attr_text reason))
      ])
  in
  Ascii_table.create ~columns ~rows
;;

let print_unclean_workspaces_table ~display_ascii ~max_output_columns users =
  print_table (unclean_workspaces_table users)
    ~display_ascii ~max_output_columns
;;

let show_whole_feature
      (feature : Feature.t)
      ~display_ascii ~max_output_columns ~show_attribute_table ~show_description
      ~show_included_feature_details ~included_features_order
      ~show_completed_review ~show_lock_reasons
      ~show_uncommitted_sessions_table
      ~show_unclean_workspaces_table
  =
  print_string (header feature.feature_path);
  if show_description then print_string (render_description feature.description);
  if show_attribute_table then begin
    printf "\n%s"
      (attribute_table feature ~display_ascii ~max_output_columns
         ~show_feature_id:false
         ~show_lock_reasons
         ~show_next_steps:true);
  end;
  begin match feature.cr_summary with
  | Ok cr_summary -> print_cr_table cr_summary ~display_ascii ~max_output_columns
  | Error e ->
    printf "\n%s\n"
      (Error.create "not showing crs" e [%sexp_of: Error.t]
       |> [%sexp_of: Error.t]
       |> Sexp.to_string_hum)
  end;
  begin match feature.line_count_by_user with
  | Ok line_count_by_user ->
    print_line_count_table line_count_by_user ~display_ascii ~max_output_columns
      ~show_completed_review;
  | Error e ->
    printf "\n%s\n"
      (Error.create "not showing line counts" e [%sexp_of: Error.t]
       |> [%sexp_of: Error.t]
       |> Sexp.to_string_hum)
  end;
  if show_uncommitted_sessions_table then begin
    match feature.users_with_uncommitted_session with
    | Ok users ->
      if not (Set.is_empty users)
      then print_uncommitted_sessions_table users ~display_ascii ~max_output_columns
    | Error _ -> ()
  end;
  if show_unclean_workspaces_table then begin
    print_unclean_workspaces_table feature.users_with_unclean_workspaces
      ~display_ascii ~max_output_columns
  end;
  printf "%s"
    (render_included_features feature ~display_ascii ~max_output_columns
       ~show_attribute_table ~show_description ~show_included_feature_details
       ~included_features_order);
;;

let org_header ~depth string = sprintf "%s %s\n" (String.make depth '*') string

let display_header_org_mode feature_path ~depth =
  print_string (org_header ~depth (Feature_path.to_string feature_path));
;;

let display_description_org_mode ~description =
  print_string
    (concat (List.map (String.split_lines description) ~f:(fun line ->
       concat [ ": "; line; "\n" ])));
;;

let display_included_features_org_mode (feature : Feature.t) ~depth
      ~show_description ~show_diff_stat ~show_included_feature_details
      ~included_features_order =
  let display_ascii = true in
  let included_features =
    Feature.released_features feature ~sorted_by:included_features_order
  in
  if List.is_empty included_features
  then return ()
  else begin
    print_string (org_header ~depth "Included feature names");
    List.map included_features ~f:Released_feature.feature_path
    |> List.dedup ~compare:Feature_path.compare
    |> List.iter ~f:(fun feature_path ->
      printf "- %s\n" (Feature_path.to_string feature_path));
    if not show_included_feature_details
    then return ()
    else begin
      print_string (org_header ~depth "Included features");
      Deferred.List.iter included_features ~f:(fun (r : Released_feature.t) ->
        let depth =
          depth + (Feature_path.num_parts r.feature_path
                   - Feature_path.num_parts feature.feature_path)
        in
        display_header_org_mode r.feature_path ~depth;
        if show_description then display_description_org_mode ~description:r.description;
        let depth = depth + 1 in
        printf "%s%s" (org_header ~depth "Attributes")
          (Ascii_table.to_string (Released_feature.attribute_table r) ~display_ascii
             ~max_output_columns:Int.max_value);
        begin match show_diff_stat with
        | None -> return ()
        | Some repo_root ->
          print_string (org_header ~depth "Affected files");
          let%bind output =
            Hg.diff repo_root ~from:r.base ~to_:(`Rev r.tip)
              (* Don't show changes in files in [.projections] except for [spec.txt]. *)
              ~args:["--stat"; "set:(** - (.projections/** - .projections/spec.txt))" ]
          in
          let output = ok_exn output in
          List.iter (String.split_lines output) ~f:(fun line -> printf ":%s\n" line);
          return ()
        end);
    end;
  end;
;;

let show_org_mode (feature : Feature.t) ~show_attribute_table
      ~show_description ~show_diff_stat ~show_included_feature_details
      ~included_features_order ~show_lock_reasons ~show_next_steps =
  let display_ascii = true in
  let max_output_columns = Int.max_value in
  let feature_path = feature.feature_path in
  let depth = 1 in
  display_header_org_mode feature_path ~depth;
  if show_description then display_description_org_mode ~description:feature.description;
  if show_attribute_table then begin
    print_string (org_header ~depth:(depth + 1) "Attributes");
    print_string
      (attribute_table feature ~display_ascii ~max_output_columns
         ~show_feature_id:false ~show_lock_reasons ~show_next_steps);
  end;
  display_included_features_org_mode feature ~depth ~show_description
    ~show_diff_stat ~show_included_feature_details ~included_features_order;
;;

let print_user_list users =
  if not (List.is_empty users) then begin
    users
    |> List.map ~f:User_name.to_string
    |> concat ~sep:"\n"
    |> print_endline;
  end
;;

let command =
  Command.async'
    ~summary:"show a feature or some of its attributes"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and maybe_archived_feature = maybe_archived_feature
     and attributes             = Attribute.args
     and display_ascii          = display_ascii
     and max_output_columns     = max_output_columns
     and omit_description =
       no_arg_flag "-omit-description" ~doc:"don't include feature descriptions"
     and omit_attribute_table =
       no_arg_flag "-omit-attribute-table"
         ~doc:"don't include feature attributes"
     and omit_uncommitted_sessions_table =
       no_arg_flag "-omit-review-sessions-in-progress-table"
         ~doc:"don't include review sessions in progress"
     and omit_unclean_workspaces_table =
       no_arg_flag "-omit-unclean-workspaces-table"
         ~doc:"don't include the unclean workspaces table"
     and org_mode =
       no_arg_flag "-org-mode" ~doc:"produce output in org-mode format"
     and show_diff_stat =
       no_arg_flag Switch.show_diff_stat
         ~doc:"compute and show [hg diff --stat] in org-mode output"
     and show_included_feature_details =
       no_arg_flag "-show-included-feature-details"
         ~doc:"show included-feature descriptions and attributes"
     and print_attribute =
       no_arg_flag "-print-attribute"
         ~doc:"print all required attributes and exit"
     and included_features_order = included_features_order
     and __DEPRECATED_show_completed_review =
       no_arg_flag "-show-completed-review" ~doc:"DEPRECATED -- became the default"
     and omit_completed_review =
       no_arg_flag "-omit-completed-review" ~doc:"omit completed review lines"
     and sexp =
       no_arg_flag "-sexp" ~doc:"print [Feature.t] as a sexp and exit"
     and show_lock_reasons =
       no_arg_flag "-show-lock-reasons" ~doc:"show lock reasons as well"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let client_config = Client_config.get () in
       let show_completed_review =
         not (omit_completed_review
              || Client_config.Cmd.Show.omit_completed_review client_config)
       in
       let show_lock_reasons =
         show_lock_reasons || Client_config.Cmd.Show.show_lock_reasons client_config
       in
       if show_diff_stat && not org_mode
       then failwith (sprintf "%s can only be used with -org-mode" Switch.show_diff_stat);
       let show_description = not omit_description in
       let show_attribute_table = not omit_attribute_table in
       let show_uncommitted_sessions_table = not omit_uncommitted_sessions_table in
       let show_unclean_workspaces_table =
         not (omit_unclean_workspaces_table
              || Client_config.Cmd.Show.omit_unclean_workspaces_table client_config)
       in
       if print_attribute then begin
         List.iter Attribute.all ~f:(fun attribute ->
           print_endline (Sexp.to_string (Attribute.sexp_of_t attribute)));
         return (shutdown 0);
       end else
         let%bind what_feature =
           Command.Param.resolve_maybe_archived_feature_spec_exn
             (ok_exn maybe_archived_feature)
         in
         let%bind feature =
           Get_feature.Maybe_archived.rpc_to_server_exn
             { what_feature; what_diff = None }
         in
         if sexp
         then begin
           feature
           |> [%sexp_of: Feature.t]
           |> Sexp.to_string_hum
           |> print_endline;
           return (shutdown 0)
         end
         else begin
           let show_diff_stat =
             if not show_diff_stat
             then return None
             else
               let not_in_clone () =
                 failwith (sprintf "%s requires being in a clone of %s"
                             Switch.show_diff_stat
                             (Remote_repo_path.to_string feature.remote_repo_path));
               in
               match%bind
                 Monitor.try_with (fun () ->
                   Cmd_workspace.repo_for_hg_operations_exn
                     (Feature.feature_path feature) ~use:`Clone)
               with
               | Error _ -> not_in_clone ()
               | Ok repo_root ->
                 let%bind rev_zero = Hg.create_rev_zero repo_root in
                 if not (Rev.equal_node_hash rev_zero feature.rev_zero)
                 then not_in_clone ();
                 return (Some repo_root)
           in
           let%bind show_diff_stat = show_diff_stat in
           match attributes with
           | [] ->
             if org_mode
             then show_org_mode feature ~show_attribute_table
                    ~show_description ~show_diff_stat ~show_included_feature_details
                    ~included_features_order ~show_lock_reasons ~show_next_steps:true
             else begin
               show_whole_feature feature ~display_ascii ~max_output_columns
                 ~show_attribute_table ~show_description ~show_included_feature_details
                 ~included_features_order ~show_completed_review ~show_lock_reasons
                 ~show_uncommitted_sessions_table
                 ~show_unclean_workspaces_table;
               return ()
             end
           | [ Reviewers ] ->
             Feature.reviewers_exn feature ~sort:`Decreasing_review |> print_user_list;
             return ()
           | [ Who_can_review ] ->
             Feature.who_can_review_exn feature |> Set.to_list |> print_user_list;
             return ()
           | [ attribute ] ->
             print_endline
               (Attribute.get attribute feature ~included_features_order
                |> Sexp.to_string_hum);
             return ();
           | attributes ->
             List.map attributes ~f:(fun attribute ->
               attribute, Attribute.get attribute feature ~included_features_order)
             |> [%sexp_of: (Attribute.t * Sexp.t) list]
             |> Sexp.to_string_hum
             |> print_endline;
             return ()
         end
    )
;;

let header_and_description feature_path ~description =
  sprintf "%s%s"
    (header feature_path)
    (render_description description);
;;

let render_email_body (feature : Feature.t) ~included_features_order =
  let display_ascii = true in
  let max_output_columns = 70 in
  sprintf "%s\n%s%s"
    (header_and_description feature.feature_path ~description:feature.description)
    (attribute_table feature ~display_ascii ~max_output_columns
       ~show_feature_id:true ~show_lock_reasons:false ~show_next_steps:false)
    (render_included_features feature ~display_ascii
       ~max_output_columns
       ~show_attribute_table:true
       ~show_description:true
       ~show_included_feature_details:true
       ~included_features_order)
;;

let render_release_email_command =
  Command.async'
    ~summary:"output the body of the release email for a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and included_features_order = included_features_order
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind feature =
         Get_feature.rpc_to_server_exn { feature_path; rev_zero = None }
       in
       printf "%s" (render_email_body feature ~included_features_order);
       return ()
    )
;;
