open! Core
open! Async
open! Import

let pending_for ~since =
  concat [ "pending for "; Time.Span.to_short_string (how_long ~since) ]
;;

let underline char line =
  sprintf "%s\n%s\n" line (String.make (String.length line) char)
;;

let header feature_path =
  let render_tree_if_feature_path_longer_than = 90 in
  let feature_path_string = Feature_path.to_string feature_path in
  if String.length feature_path_string <= render_tree_if_feature_path_longer_than
  then sprintf "%s" (underline '=' feature_path_string)
  else (
    let path_as_tree =
      List.foldi (Feature_path.parts feature_path)
        ~init:[]
        ~f:(fun i ac feature_name ->
          "\n"
          :: Feature_name.to_string feature_name
          :: String.make (2 * i) ' '
          :: ac)
      |> List.rev
      |> String.concat
    in
    sprintf "%s\n\n%s%s\n"
      feature_path_string
      path_as_tree
      (String.make render_tree_if_feature_path_longer_than '='))
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
           @ maybe show_description (fun () ->
             [ sprintf "%s\n" r.description ])
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
      | Compilation_status
      | Crs_are_enabled
      | Crs_shown_in_todo_only_for_users_reviewing
      | Feature_id
      | Feature_path
      | First_owner
      | Has_bookmark
      | Included_features
      | Inheritable_attributes
      | Is_archived
      | Is_permanent
      | Is_rebased
      | Is_seconded
      | Next_base_update
      | Next_bookmark_update
      | Next_steps
      | Owners
      | Properties
      | Release_process
      | Remote_repo_path
      | Reviewers
      | Reviewing
      | Review_is_enabled
      | Rev_zero
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
    and ts =
      enum_no_args (module T)
        ~aliases:(function Feature_id -> [ "id" ] | _ -> [])
        ~doc:(fun ~name _ -> sprintf "show %s" name)
    and user_defined = properties_list_option ~switch:"property" ~verb:"show"
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
    | Compilation_status      -> feature.compilation_status
                                 |> [%sexp_of: Compilation_status.t]
    | Crs_are_enabled         ->
      feature.crs_are_enabled
      |> [%sexp_of: bool]
    | Crs_shown_in_todo_only_for_users_reviewing ->
      feature.crs_shown_in_todo_only_for_users_reviewing
      |> [%sexp_of: bool]
    | Feature_id              -> feature.feature_id |> [%sexp_of: Feature_id.t]
    | Feature_path            -> feature.feature_path |> [%sexp_of: Feature_path.t]
    | First_owner             -> List.hd_exn feature.owners |> [%sexp_of: User_name.t]
    | Has_bookmark            -> feature.has_bookmark |> [%sexp_of: bool]
    | Included_features       ->
      List.map (Feature.released_features feature ~sorted_by:included_features_order)
        ~f:Released_feature.feature_path
      |> [%sexp_of: Feature_path.t list]
    | Is_archived -> Is_archived.to_bool feature.is_archived |> [%sexp_of: bool]
    | Is_permanent            -> feature.is_permanent |> [%sexp_of: bool]
    | Is_rebased              -> feature.is_rebased |> [%sexp_of: bool]
    | Is_seconded             -> is_some feature.seconder |> [%sexp_of: bool]
    | Inheritable_attributes  ->
      feature.inheritable_attributes |> [%sexp_of: Inheritable_attributes.Sexp_hum.t]
    | Next_base_update ->
      feature.next_base_update |> [%sexp_of: Next_base_update.t]
    | Next_bookmark_update  ->
      feature.next_bookmark_update |> [%sexp_of: Next_bookmark_update.t]
    | Owners                  -> users feature.owners
    | Properties -> feature.properties |> [%sexp_of: Properties.t]
    | Release_process         ->
      feature.release_process |> [%sexp_of: Release_process.t]
    | Remote_repo_path        ->
      feature.remote_repo_path |> [%sexp_of: Remote_repo_path.t]
    | Rev_zero -> Rev.to_string_40 feature.rev_zero |> [%sexp_of: string]
    | Review_is_enabled       ->
      feature.review_is_enabled
      |> [%sexp_of: bool]
    | Reviewers -> Feature.reviewers_exn feature ~sort:`Decreasing_review |> users
    | Reviewing               -> Reviewing.to_sexp_hum feature.reviewing
    | Seconder                ->
      (match feature.seconder with
       | None -> "not seconded" |> [%sexp_of: string]
       | Some user -> user |> [%sexp_of: User_name.t])
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
      (match Map.find feature.properties key with
       | None -> raise_s [%sexp "undefined property", (key : string)]
       | Some sexp -> sexp)
    | Users_with_review_session_in_progress ->
      (match feature.users_with_review_session_in_progress with
       | Ok users  -> users |> user_set
       | Error err -> Error.raise err)
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
  if not (Ascii_table.is_empty ascii_table)
  then (
    print_newline ();
    print_string (Ascii_table.to_string ascii_table ~display_ascii ~max_output_columns))
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
  fun ?last_known_at name rev rev_facts_or_pending ->
    [ name
    , (match last_known_at with
       | None -> [], Rev.to_string_hum rev
       | Some since ->
         [`Yellow],
         sprintf "none (last known as %s %s ago)"
           (Rev.to_string_hum rev)
           (Time.Span.to_short_string (how_long ~since)))
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

let maybe_empty_set set ~f =
  if Set.is_empty set
  then []
  else f set
;;

let maybe_empty_list list ~f =
  if List.is_empty list
  then []
  else f list
;;

let set_to_row name ~f:to_string ?(attrs=[]) set =
  [ name
  , (attrs, (set
             |> Set.to_list
             |> List.map ~f:to_string
             |> String.concat ~sep:", "))
  ]
;;

let send_email_upon_row = set_to_row "send email upon" ~f:Send_email_upon.to_string
let send_email_to_row   = set_to_row "send email to"   ~f:Email_address.to_string

let property_rows properties =
  Map.to_alist properties
  |> List.map ~f:(fun (key, value) -> (key, ([], Sexp.to_string_hum value)))
;;

let release_process_row release_process =
  [ "release process"
  , ([], Release_process.to_string_hum release_process)
  ]
;;

let who_can_release_into_me_row who_can_release_into_me =
  [ "who can release"
  , ([], Who_can_release_into_me.to_string_hum_as_parent
           who_can_release_into_me)
  ]
;;

let block name = function
  | [] -> []
  | _ :: _ as inside ->
    (name, ([], "")) :: List.map inside ~f:(fun (key, value) -> ("  " ^ key, value))
;;

let attribute_table_with_fields ~display_ascii ~max_output_columns ~next_steps
      ?compilation_status_to_display
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
      ?inheritable_attributes
      ?(show_lock_reasons=false)
      ?(show_inheritable_attributes=false)
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
    let is_known_to_be_archived =
      Option.value_map ~default:false ~f:Is_archived.to_bool is_archived
    in
    let rev_facts  = rev_facts  ~is_archived:is_known_to_be_archived in
    let or_pending = or_pending ~is_archived:is_known_to_be_archived in
    List.concat
      [ maybe feature_id (fun feature_id ->
          [ "id", ([], Feature_id.to_string feature_id )])
      ; maybe is_archived (fun is_archived ->
          let is_archived = Is_archived.to_bool is_archived in
          if is_archived || show_is_archived_if_not_archived
          then [ "is archived", ([ `Yellow ], Bool.to_string is_archived) ]
          else [])
      ; maybe is_archived (fun is_archived ->
          match is_archived with
          | No | Yes { reason_for_archiving = "" } -> []
          | Yes { reason_for_archiving } ->
            [ "reason for archiving", ([], reason_for_archiving) ])
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
      ; maybe compilation_status_to_display
          Compilation_status_to_display.to_ascii_table_rows
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
      ; maybe tip  (fun tip  -> rev_facts "tip" tip tip_facts)
      ; maybe base (fun base ->
          let last_known_at =
            match (next_base_update : Next_base_update.t option) with
            | Some Update_expected { expected_since; _} -> Some expected_since
            | None | Some No_update_expected -> None
          in
          rev_facts "base" base base_facts ?last_known_at)
      ; maybe3 base tip base_is_ancestor_of_tip (fun base tip base_is_ancestor_of_tip ->
          or_pending "base is ancestor of tip" base_is_ancestor_of_tip
            (fun is_ancestor ->
               bool_or_error "base is ancestor of tip"
                 (Rev_facts.Is_ancestor.check is_ancestor
                    ~ancestor:base ~descendant:tip)))
      ; block "release into me"
          (match has_children with
           | None | Some false -> []
           | Some true ->
             List.concat
               [ maybe release_process release_process_row
               ; maybe who_can_release_into_me who_can_release_into_me_row
               ])
      ; maybe send_email_upon (fun send_email_upon ->
          if Set.equal send_email_upon Send_email_upon.default
          then []
          else send_email_upon_row send_email_upon)
      ; maybe send_email_to (fun send_email_to ->
          if Set.is_empty send_email_to
          then []
          else send_email_to_row ~attrs:[] send_email_to)
      ; maybe properties property_rows
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
      ; block "inheritable attributes"
          ( maybe inheritable_attributes (
              fun { Inheritable_attributes.
                    crs_shown_in_todo_only_for_users_reviewing
                  ; xcrs_shown_in_todo_only_for_users_reviewing
                  ; owners
                  ; properties
                  ; release_process
                  ; who_can_release_into_me
                  ; send_email_to
                  ; send_email_upon
                  ; whole_feature_followers
                  ; whole_feature_reviewers
                  } ->
                if not show_inheritable_attributes
                then []
                else
                  List.concat
                    [ maybe crs_shown_in_todo_only_for_users_reviewing
                        (fun crs_shown_in_todo_only_for_users_reviewing ->
                           in_todo_for
                             "CRs" crs_shown_in_todo_only_for_users_reviewing)
                    ; maybe xcrs_shown_in_todo_only_for_users_reviewing
                        (fun xcrs_shown_in_todo_only_for_users_reviewing ->
                           in_todo_for
                             "XCRs" xcrs_shown_in_todo_only_for_users_reviewing)
                    ; maybe_empty_list ~f:(user_list "owner") owners
                    ; property_rows properties
                    ; maybe release_process release_process_row
                    ; maybe who_can_release_into_me who_can_release_into_me_row
                    ; maybe_empty_set send_email_upon ~f:send_email_upon_row
                    ; maybe_empty_set send_email_to ~f:(send_email_to_row ~attrs:[])
                    ; maybe_empty_set
                        ~f:(user_set "whole feature follower") whole_feature_followers
                    ; maybe_empty_set
                        ~f:(user_set "whole feature reviewer") whole_feature_reviewers
                    ]
            ))
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
      ~show_feature_id ~show_lock_reasons ~show_inheritable_attributes ~show_next_steps
      ~show_full_compilation_status
      { Feature.
        feature_id
      ; feature_path
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
      ; is_rebased                = _
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
      ; users_with_review_session_in_progress = _
      ; users_with_unclean_workspaces = _
      ; latest_release = _
      ; inheritable_attributes
      ; compilation_status
      } =
  attribute_table_with_fields ~display_ascii ~max_output_columns
    ?feature_id:(Option.some_if (show_feature_id || Is_archived.to_bool is_archived)
                   feature_id)
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
    ~inheritable_attributes
    ~show_lock_reasons
    ~show_inheritable_attributes
    ~is_archived
    ~show_is_archived_if_not_archived:false
    ?compilation_status_to_display:(
      if am_functional_testing
      || Client_config.(get () |> Cmd.Show.show_compilation_status)
      then
        Some (Compilation_status_to_display.of_compilation_status compilation_status
                feature_path ~feature_tip:tip ~show_full_compilation_status)
      else
        None)
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

let review_sessions_in_progress_table user_set =
  let rows = Set.to_list user_set in
  let columns =
    Ascii_table.Column.(
      [ of_to_string ~header:"review sessions in progress" User_name.to_string
          (cell (fun name -> name)) ])
  in
  Ascii_table.create ~columns ~rows
;;

let print_review_sessions_in_progress_table ~display_ascii ~max_output_columns user_set =
  print_table (review_sessions_in_progress_table user_set)
    ~display_ascii ~max_output_columns
;;

let unclean_workspaces_table users =
  let rows = Map.to_alist users in
  let columns =
    Ascii_table.Column.(
      [ of_to_string ~header:"unclean workspaces" User_name.to_string (cell fst)
      ; string       ~header:"reason" (cell (fun (_, reason) ->
          Unclean_workspace_reason.to_ascii_table_column_text reason))
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
      ~show_included_feature_details ~included_features_order ~show_inheritable_attributes
      ~show_completed_review ~show_feature_id ~show_lock_reasons
      ~show_review_sessions_in_progress_table
      ~show_unclean_workspaces_table
      ~show_full_compilation_status
  =
  print_string (header feature.feature_path);
  (if show_description then printf "%s\n" feature.description);
  (if show_attribute_table
   then (
     printf "\n%s"
       (attribute_table feature ~display_ascii ~max_output_columns
          ~show_feature_id
          ~show_lock_reasons
          ~show_inheritable_attributes
          ~show_full_compilation_status
          ~show_next_steps:true)));
  (match feature.cr_summary with
   | Ok cr_summary -> print_cr_table cr_summary ~display_ascii ~max_output_columns
   | Error e ->
     printf "\n%s\n"
       (Error.create "not showing crs" e [%sexp_of: Error.t]
        |> [%sexp_of: Error.t]
        |> Sexp.to_string_hum));
  (match feature.line_count_by_user with
   | Ok line_count_by_user ->
     print_line_count_table line_count_by_user ~display_ascii ~max_output_columns
       ~show_completed_review;
   | Error e ->
     printf "\n%s\n"
       (Error.create "not showing line counts" e [%sexp_of: Error.t]
        |> [%sexp_of: Error.t]
        |> Sexp.to_string_hum));
  (if show_review_sessions_in_progress_table
   then (
     match feature.users_with_review_session_in_progress with
     | Ok users ->
       if not (Set.is_empty users)
       then print_review_sessions_in_progress_table users
              ~display_ascii ~max_output_columns
     | Error _ -> ()));
  (if show_unclean_workspaces_table
   then (
     print_unclean_workspaces_table feature.users_with_unclean_workspaces
       ~display_ascii ~max_output_columns));
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
  else (
    print_string (org_header ~depth "Included feature names");
    List.map included_features ~f:Released_feature.feature_path
    |> List.dedup_and_sort ~compare:Feature_path.compare
    |> List.iter ~f:(fun feature_path ->
      printf "- %s\n" (Feature_path.to_string feature_path));
    if not show_included_feature_details
    then return ()
    else (
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
        (match show_diff_stat with
         | None -> return ()
         | Some repo_root ->
           print_string (org_header ~depth "Affected files");
           let%bind output =
             Hg.diff repo_root ~from:r.base ~to_:(`Rev r.tip) ~args:["--stat"]
           in
           let output = ok_exn output in
           List.iter (String.split_lines output) ~f:(fun line -> printf ":%s\n" line);
           return ()))))
;;

let show_org_mode (feature : Feature.t) ~show_attribute_table
      ~show_description ~show_diff_stat ~show_included_feature_details
      ~included_features_order ~show_feature_id ~show_lock_reasons
      ~show_inheritable_attributes ~show_full_compilation_status ~show_next_steps =
  let display_ascii = true in
  let max_output_columns = Int.max_value in
  let feature_path = feature.feature_path in
  let depth = 1 in
  display_header_org_mode feature_path ~depth;
  (if show_description then display_description_org_mode ~description:feature.description);
  (if show_attribute_table
   then (
     print_string (org_header ~depth:(depth + 1) "Attributes");
     print_string
       (attribute_table feature ~display_ascii ~max_output_columns
          ~show_feature_id ~show_lock_reasons ~show_inheritable_attributes
          ~show_next_steps ~show_full_compilation_status)));
  display_included_features_org_mode feature ~depth ~show_description
    ~show_diff_stat ~show_included_feature_details ~included_features_order;
;;

let print_user_list users =
  if not (List.is_empty users)
  then (
    users
    |> List.map ~f:User_name.to_string
    |> concat ~sep:"\n"
    |> print_endline)
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
     and omit_review_sessions_in_progress_table =
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
     and show_inheritable_attributes =
       no_arg_flag "-show-inheritable-attributes"
         ~doc:"show inheritable attributes in attributes table"
     and sexp =
       no_arg_flag "-sexp" ~doc:"print [Feature.t] as a sexp and exit"
     and show_feature_id =
       no_arg_flag "-show-feature-id" ~doc:"show feature id as well"
     and show_lock_reasons =
       no_arg_flag "-show-lock-reasons" ~doc:"show lock reasons as well"
     and show_full_compilation_status =
       no_arg_flag "-show-full-compilation-status"
         ~doc:"show compilation status for all repo controllers"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let client_config = Client_config.get () in
       let show_completed_review =
         not (omit_completed_review
              || Client_config.Cmd.Show.omit_completed_review client_config)
       in
       let show_feature_id =
         show_feature_id || Client_config.Cmd.Show.show_feature_id client_config
       in
       let show_lock_reasons =
         show_lock_reasons || Client_config.Cmd.Show.show_lock_reasons client_config
       in
       let show_full_compilation_status =
         show_full_compilation_status
         || Client_config.Cmd.Show.show_full_compilation_status client_config
       in
       let show_inheritable_attributes =
         show_inheritable_attributes
         || Client_config.Cmd.Show.show_inheritable_attributes client_config
       in
       if show_diff_stat && not org_mode
       then failwith (sprintf "%s can only be used with -org-mode" Switch.show_diff_stat);
       let show_description = not omit_description in
       let show_attribute_table = not omit_attribute_table in
       let show_review_sessions_in_progress_table =
         not omit_review_sessions_in_progress_table
       in
       let show_unclean_workspaces_table =
         not (omit_unclean_workspaces_table
              || Client_config.Cmd.Show.omit_unclean_workspaces_table client_config)
       in
       let included_features_order = ok_exn included_features_order in
       if print_attribute
       then (
         List.iter Attribute.all ~f:(fun attribute ->
           print_endline (Sexp.to_string (Attribute.sexp_of_t attribute)));
         return ())
       else (
         let%bind what_feature =
           Command.Param.resolve_maybe_archived_feature_spec_exn
             (ok_exn maybe_archived_feature)
         in
         let%bind feature =
           Get_feature.Maybe_archived.rpc_to_server_exn
             { what_feature; what_diff = None }
         in
         if sexp
         then (
           feature
           |> [%sexp_of: Feature.t]
           |> Sexp.to_string_hum
           |> print_endline;
           return ())
         else (
           let show_diff_stat =
             if not show_diff_stat
             then return None
             else (
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
                 return (Some repo_root))
           in
           let%bind show_diff_stat = show_diff_stat in
           match attributes with
           | [] ->
             if org_mode
             then show_org_mode feature ~show_attribute_table
                    ~show_description ~show_diff_stat ~show_included_feature_details
                    ~included_features_order ~show_feature_id ~show_lock_reasons
                    ~show_inheritable_attributes ~show_full_compilation_status
                    ~show_next_steps:true
             else (
               show_whole_feature feature ~display_ascii ~max_output_columns
                 ~show_attribute_table ~show_description ~show_included_feature_details
                 ~included_features_order ~show_inheritable_attributes
                 ~show_completed_review
                 ~show_feature_id ~show_lock_reasons
                 ~show_review_sessions_in_progress_table
                 ~show_unclean_workspaces_table
                 ~show_full_compilation_status;
               return ())
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
             return ())))
;;

let header_and_description feature_path ~description =
  sprintf "%s%s\n" (header feature_path) description;
;;

module Event = struct
  type t =
    | Released
    | Archived of { reason_for_archiving : string }
end

let render_email_body (feature : Feature.t) ~included_features_order ~event =
  let display_ascii = true in
  let max_output_columns = 70 in
  let reason =
    match (event : Event.t) with
    | Released -> ""
    | Archived { reason_for_archiving } ->
      if String.is_empty reason_for_archiving
      then ""
      else sprintf "Reason for archiving: %s\n\n" reason_for_archiving
  in
  sprintf "%s%s\n%s%s" reason
    (header_and_description feature.feature_path ~description:feature.description)
    (attribute_table feature ~display_ascii ~max_output_columns
       ~show_feature_id:true ~show_lock_reasons:false ~show_inheritable_attributes:false
       ~show_next_steps:false ~show_full_compilation_status:false)
    (render_included_features feature ~display_ascii
       ~max_output_columns
       ~show_attribute_table:true
       ~show_description:true
       ~show_included_feature_details:true
       ~included_features_order)
;;

let make_render_email_command name event_param =
  Command.async'
    ~summary:(sprintf "output the body of the %s email for a feature" name)
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and included_features_order = included_features_order
     and event = event_param
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let included_features_order = ok_exn included_features_order in
       let%bind feature =
         Get_feature.rpc_to_server_exn { feature_path; rev_zero = None }
       in
       printf "%s" (render_email_body feature ~included_features_order ~event);
       return ())
;;

let render_release_email_command =
  make_render_email_command "release" (Command.Param.return Event.Released)
;;

let render_archive_email_command =
  make_render_email_command "archive"
    (let open Command.Let_syntax in
     let%map_open reason_for_archiving = reason_for_archiving in
     Event.Archived { reason_for_archiving })
;;

let show_lines_required_to_separate_ddiff_hunks =
  Command.async'
    ~summary:"show settings of lines required to separate ddiff hunks"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map rows =
         Get_lines_required_to_separate_ddiff_hunks.rpc_to_server_exn ()
         >>| Map.to_alist
       in
       let columns =
         Ascii_table.Column.(
           [ string ~header:"feature"
               (cell (fun (feature, _) -> Feature_name.to_string feature))
           ; int ~header:"value" ~show_zero:true (cell snd)
           ])
       in
       print_table (Ascii_table.create ~columns ~rows) ~display_ascii ~max_output_columns)
;;
