open! Core
open! Async
open! Import

module Assigned     = Todo.Assigned
module Feature_info = Todo.Feature_info

let preceding_text = ref false

let print_table ~table_name ~columns ~rows ~display_ascii ~max_output_columns =
  if not (List.is_empty rows)
  then (
    if !preceding_text then printf "\n";
    preceding_text := true;
    printf "%s%s"
      (if String.is_empty table_name
       then ""
       else table_name ^ ":\n")
      (Ascii_table.to_string (Ascii_table.create ~columns ~rows)
         ~display_ascii
         ~max_output_columns))
;;

let feature_display_attributes ~next_steps ~review_is_enabled ~feature_path_exists =
  if not feature_path_exists
  then [ `Dim ]
  else Next_step.to_attrs next_steps ~review_is_enabled
;;

let show_assigned assigned ~display_ascii ~max_output_columns =
  if not (List.is_empty assigned)
  then (
    let rows =
      Feature_table.create assigned
        Assigned.feature_path
        (fun ~feature assigned_option -> (feature, assigned_option))
    in
    let columns =
      Ascii_table.Column.(
        let num_crs ~header f =
          int_or_error ~header (cell (fun (_, a) ->
            match a with
            | None -> Ok 0
            | Some a ->
              match f a with
              | `Disabled -> Ok 0
              | `Enabled result -> result))
        in
        let if_assigned f =
          cell (fun (_, a) ->
            match a with
            | None -> ""
            | Some (a : Assigned.t) -> f a)
        in
        let review_lines ~header f =
          string ~header ~align:Right ~show:`If_not_empty ~show_zero:false
            (if_assigned (fun a ->
               if not a.user_is_reviewing
               then ""
               else f a.line_count))
        in
        let catch_up_lines ~header f =
          string ~header ~align:Right ~show:`If_not_empty ~show_zero:false
            (if_assigned (fun a -> f a.line_count.catch_up))
        in
        [ string ~header:"feature"
            (attr_cell (fun (feature, assigned_option) ->
               let attrs =
                 match (assigned_option : Assigned.t option) with
                 | None -> []
                 | Some { next_steps; review_is_enabled; feature_path_exists; _ } ->
                   feature_display_attributes
                     ~next_steps
                     ~review_is_enabled
                     ~feature_path_exists
               in
               (attrs, feature)))
        ; num_crs ~header:"CRs"  Assigned.num_crs
        ; num_crs ~header:"XCRs" Assigned.num_xcrs
        ; review_lines ~header:"review" (fun t ->
            Line_count.to_review_column_shown t
            |> Review_or_commit.to_string_hum)
        ; review_lines ~header:"follow"
            (fun t -> t.review.follow |> Int.to_string_hum)
        ; catch_up_lines ~header:"catch-up"
            (fun t -> Line_count.Catch_up.total t |> Int.to_string_hum)
        ; string ~header:"next step"
            (attr_cell (fun (_, assigned_option) ->
               match (assigned_option : Assigned.t option) with
               | None -> ([], "")
               | Some { review_is_enabled; assigned_next_steps; _ } ->
                 Next_step.Assigned.to_attrs_and_string
                   ~review_is_enabled assigned_next_steps))
        ])
    in
    print_table ~table_name:"" ~columns ~rows ~display_ascii ~max_output_columns)
;;

let show_unclean_workspaces
      (unclean_workspaces_on_machine : Unclean_workspace.t list Machine_name.Map.t)
      ~display_ascii ~max_output_columns=
  Map.iteri unclean_workspaces_on_machine ~f:(fun ~key:machine ~data:unclean_workspaces ->
    let columns, rows =
      Cmd_workspace_unclean.unclean_workspaces_columns_and_rows
        unclean_workspaces
    in
    print_table
      ~table_name:(sprintf "Unclean workspaces on %s" (Machine_name.to_string machine))
      ~columns ~rows ~display_ascii ~max_output_columns)
;;

let show_feature_info feature_info ~table_name ~display_ascii ~max_output_columns =
  let rows =
    Feature_table.create feature_info
      Feature_info.feature_path
      (fun ~feature feature_info_option -> (feature, feature_info_option))
  in
  let columns =
    Ascii_table.Column.(
      [ string ~header:"feature"
          (attr_cell (fun (feature, feature_info_option) ->
             let attrs =
               match feature_info_option with
               | None -> []
               | Some (feature_info : Feature_info.t) ->
                 feature_display_attributes
                   ~next_steps:feature_info.next_steps
                   ~review_is_enabled:feature_info.review_is_enabled
                   ~feature_path_exists:true
             in
             (attrs, feature)))
      ]
      @ (
        let empty = ([], "") in
        let cell field =
          cell (fun (_, a) -> Option.value_map a ~default:(Ok 0) ~f:field)
        in
        let attr_cell field f =
          attr_cell (fun (_, a) ->
            Option.value_map a ~default:empty ~f:(fun a -> f (field a)))
        in
        let rev_facts header field ~mention_crs =
          string ~header
            (attr_cell field
               (function
                 | Pending_since _ -> ([ `Yellow ], "pending")
                 | Known { Todo.Rev_facts.
                           is_conflict_free
                         ; is_cr_clean
                         ; obligations_are_valid
                         } ->
                   let fact b problem_name =
                     match b with
                     | Ok b -> if b then [] else [ problem_name ]
                     | Error () -> [ problem_name ]
                   in
                   let problems =
                     List.concat
                       [ if mention_crs then fact is_cr_clean "CRs" else []
                       ; fact is_conflict_free      "<<<"
                       ; fact obligations_are_valid "oblig"
                       ]
                   in
                   if List.is_empty problems
                   then ([], "")
                   else ([ `Red ], String.concat ~sep:"," problems)))
        in
        [ rev_facts "base" Feature_info.base ~mention_crs:true
        ; rev_facts "tip"  Feature_info.tip  ~mention_crs:false
        ; int_or_error ~header:"CRs"   (cell Feature_info.num_crs)
        ; int_or_error ~header:"XCRs"  (cell Feature_info.num_xcrs)
        ; int_or_error ~header:"#left" (cell Feature_info.num_reviewers_with_review_remaining)
        ])
      @ [ string ~header:"next step"
            (attr_cell (fun (_, feature_info_option) ->
               match feature_info_option with
               | None -> ([], "")
               | Some (feature_info : Feature_info.t) ->
                 Next_step.to_attrs_and_string feature_info.next_steps
                   ~review_is_enabled:feature_info.review_is_enabled))
        ])
  in
  print_table ~table_name ~columns ~rows ~display_ascii ~max_output_columns
;;

let cr_soons_table_name = "CR-soons assigned to you"

let show_cr_soons cr_soons ~display_ascii ~max_output_columns =
  let columns =
    Ascii_table.Column.(
      [ string ~header:"family"
          (cell (fun cr_soon ->
             Feature_name.to_string
               (Feature_path.root (Cr_soon_in_feature.feature_path cr_soon))))
      ; string ~header:"file:line"
          (cell (fun cr_soon ->
             concat [ Relpath.to_string (Cr_soon_in_feature.path cr_soon)
                    ; ":"; Int.to_string (Cr_soon_in_feature.start_line cr_soon)
                    ]))
      ; string ~header:"active in"
          (cell (fun cr_soon ->
             Option.value_map (Cr_soon_in_feature.active_in cr_soon) ~default:""
               ~f:Feature_path.to_string))
      ])
  in
  let rows = List.sort cr_soons ~cmp:Cr_soon_in_feature.For_sorted_output.compare in
  print_table ~table_name:cr_soons_table_name
    ~columns ~rows ~display_ascii ~max_output_columns
;;

let bookmarks_without_feature_name = "Bookmarks without a feature"

let show_bookmarks_without_feature bookmarks_without_feature
      ~display_ascii ~max_output_columns =
  let rows =
    List.concat_map
      (List.sort bookmarks_without_feature
         ~cmp:(fun (r1, _) (r2, _) -> Remote_repo_path.compare r1 r2))
      ~f:(fun (r, bookmarks) ->
        match bookmarks with
        | [] -> []
        | b :: bookmarks ->
          (Remote_repo_path.to_string r, b)
          :: List.map bookmarks ~f:(fun b -> ("", b)))
  in
  let columns =
    Ascii_table.Column.(
      [ string ~header:"repo"     (cell fst)
      ; string ~header:"bookmark" (cell (fun (_, b) -> Bookmark_without_feature.bookmark b))
      ])
  in
  print_table ~table_name:bookmarks_without_feature_name
    ~columns ~rows ~display_ascii ~max_output_columns;
;;


let owned_table_name   = "Features you own"
let watched_table_name = "Features you watch"

let command =
  let switch_my_cr_soons = "-cr-soons" in
  let switch_my_unclean_workspaces = "-unclean-workspaces" in
  Command.async'
    ~summary:"show what a user should work on"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and for_ = for_
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     and include_active_cr_soons = include_active_cr_soons
     and bookmarks_without_feature_names =
       no_arg_flag "-bookmarks-without-feature"
         ~doc:(sprintf "list bookmarks from the '%s' table"
                 bookmarks_without_feature_name)
     and crs_and_review_names =
       no_arg_flag "-crs-and-review-names"
         ~doc:"list feature names, including features from the assigned table"
     and my_unclean_workspaces_names =
       no_arg_flag "-unclean-workspaces-names"
         ~doc:"list feature names, including features with unclean workspaces"
     and review_names =
       no_arg_flag "-review-names"
         ~doc:"list feature names, including features with review lines"
     and follow_names =
       no_arg_flag "-follow-names"
         ~doc:"list feature names, including features with follow lines"
     and catch_up_names =
       no_arg_flag "-catch-up-names"
         ~doc:"list feature names, including features with catch-up lines"
     and owned_by_me_names =
       no_arg_flag "-owned-by-me-names"
         ~doc:(sprintf "list features names, including features from the '%s' table"
                 owned_table_name)
     and watched_by_me_names =
       no_arg_flag "-watched-by-me-names"
         ~doc:(sprintf "list features names, including features from the '%s' table"
                 watched_table_name)
     and releasable_names =
       no_arg_flag "-releasable-names"
         ~doc:"list feature names, including releasable features"
     and do_not_show_cr_soons =
       do_not_show_cr_soons
     and do_not_show_unclean_workspaces =
       do_not_show_unclean_workspaces
     and include_all_owned_features =
       no_arg_flag "-include-all-owned-features"
         ~doc:"include owned features that are normally omitted"
     and feature_path_option =
       feature_path_option
     and assigned_to_me =
       no_arg_flag "-assigned"
         ~aliases:[ "-crs-and-review" ]
         ~doc:"show only the assigned table"
     and my_cr_soons =
       no_arg_flag switch_my_cr_soons
         ~doc:(sprintf "show only the '%s' table" cr_soons_table_name)
     and my_unclean_workspaces =
       no_arg_flag switch_my_unclean_workspaces
         ~doc:"show only the unclean workspaces table(s)"
     and owned_by_me =
       no_arg_flag "-owned-by-me"
         ~doc:(sprintf "show only the '%s' table" owned_table_name)
     and watched_by_me =
       no_arg_flag "-watched-by-me"
         ~doc:(sprintf "show only the '%s' table" watched_table_name)
     and releasable =
       no_arg_flag "-releasable"
         ~doc:"show only releasable features"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path_option = ok_exn feature_path_option in
       (if do_not_show_cr_soons && my_cr_soons
        then failwithf "The flags [%s] and [%s] are mutually exclusive"
               Switch.do_not_show_cr_soons switch_my_cr_soons ());
       (if do_not_show_unclean_workspaces && my_unclean_workspaces
        then failwithf "The flags [%s] and [%s] are mutually exclusive"
               Switch.do_not_show_unclean_workspaces switch_my_unclean_workspaces ());
       let client_config = Client_config.get () in
       let do_not_show_cr_soons =
         do_not_show_cr_soons
         || Client_config.Cmd.Todo.do_not_show_cr_soons client_config
       in
       let do_not_show_unclean_workspaces =
         do_not_show_unclean_workspaces
         || Client_config.Cmd.Todo.do_not_show_unclean_workspaces client_config
       in
       let%map reaction =
         Todo.rpc_to_server_exn
           { for_; include_active_cr_soons; include_all_owned_features }
       in
       let reaction =
         match feature_path_option with
         | None -> reaction
         | Some ancestor ->
           let should_include descendant =
             Feature_path.is_ancestor ~ancestor ~descendant
           in
           { assigned =
               List.filter reaction.assigned ~f:(fun t -> should_include t.feature_path)
           ; unclean_workspaces =
               Map.filter_map reaction.unclean_workspaces ~f:(fun unclean_workspaces ->
                 match
                   List.filter unclean_workspaces
                     ~f:(fun t -> should_include t.feature_path)
                 with
                 | [] -> None
                 | (_::_) as unclean_workspaces -> Some unclean_workspaces)
           ; owned =
               List.filter reaction.owned ~f:(fun t -> should_include t.feature_path)
           ; watched =
               List.filter reaction.watched ~f:(fun t -> should_include t.feature_path)
           ; cr_soons =
               Cr_soon_multiset.filter reaction.cr_soons ~f:(fun t ->
                 should_include (Cr_soon_multiset.Cr_soon_in_feature.feature_path t))
           (* Bookmarks without a feature aren't associated with a feature, so we don't
              show them if todo is being restricted to a feature subtree. *)
           ; bookmarks_without_feature = []
           }
       in
       let { Todo.Reaction.
             assigned
           ; unclean_workspaces
           ; owned
           ; watched
           ; cr_soons
           ; bookmarks_without_feature
           } =
         if not (releasable || releasable_names)
         then reaction
         else (
           let should_include next_steps =
             List.exists (next_steps : Next_step.t list) ~f:(function
               | Release
               | Wait_for_continuous_release -> true
               | _ -> false)
           in
           { assigned =
               List.filter reaction.assigned ~f:(fun t -> should_include t.next_steps)
           ; unclean_workspaces = Machine_name.Map.empty
           ; owned =
               List.filter reaction.owned    ~f:(fun t -> should_include t.next_steps)
           ; watched =
               List.filter reaction.watched  ~f:(fun t -> should_include t.next_steps)
           ; cr_soons = Cr_soon_multiset.empty
           ; bookmarks_without_feature = []
           })
       in
       let feature_paths = ref Feature_path.Set.empty in
       let list ?(if_ = const true) l get_feature_path =
         feature_paths :=
           Set.union !feature_paths
             (Feature_path.Set.of_list
                (List.map (List.filter l ~f:if_) ~f:get_feature_path))
       in
       if bookmarks_without_feature_names
       then (
         bookmarks_without_feature
         |> List.concat_map ~f:(fun (_remote_repo_path, bookmarks) ->
           List.map bookmarks ~f:Bookmark_without_feature.bookmark)
         |> String.Set.of_list
         |> Set.iter ~f:print_endline)
       else (
         (if crs_and_review_names
          then list assigned Assigned.feature_path);
         (if my_unclean_workspaces_names
          then
            Map.iteri unclean_workspaces ~f:(fun ~key:_machine ~data:unclean_workspaces ->
              list unclean_workspaces Unclean_workspace.feature_path));
         (if review_names
          then list assigned Assigned.feature_path ~if_:Assigned.has_review_lines);
         (if follow_names
          then list assigned Assigned.feature_path ~if_:Assigned.has_follow_lines);
         (if catch_up_names
          then list assigned Assigned.feature_path ~if_:Assigned.has_catch_up_lines);
         (if owned_by_me_names   then list owned   Feature_info.feature_path);
         (if watched_by_me_names then list watched Feature_info.feature_path);
         (if releasable_names then (
            list assigned Assigned.feature_path;
            list owned    Feature_info.feature_path;
            list watched  Feature_info.feature_path));
         if crs_and_review_names
         || my_unclean_workspaces_names
         || review_names
         || follow_names
         || catch_up_names
         || owned_by_me_names
         || watched_by_me_names
         || releasable_names
         then
           Set.iter !feature_paths ~f:(fun feature_path ->
             print_endline (Feature_path.to_string feature_path))
         else (
           (* We force to show everything if nothing is set to be shown, because it
              doesn't make sense to show nothing.  Otherwise we only show what is
              requested. *)
           let force_show =
             not assigned_to_me
             && not owned_by_me
             && not watched_by_me
             && not my_unclean_workspaces
             && not my_cr_soons
           in
           if assigned_to_me || force_show
           then show_assigned assigned ~display_ascii ~max_output_columns;
           if my_unclean_workspaces || (force_show && not do_not_show_unclean_workspaces)
           then show_unclean_workspaces unclean_workspaces
                  ~display_ascii ~max_output_columns;
           if owned_by_me || force_show
           then show_feature_info owned ~table_name:owned_table_name
                  ~display_ascii ~max_output_columns;
           if watched_by_me || force_show
           then show_feature_info watched ~table_name:watched_table_name
                  ~display_ascii ~max_output_columns;
           if my_cr_soons || (force_show && not do_not_show_cr_soons)
           then show_cr_soons (Cr_soon_multiset.to_list cr_soons) ~display_ascii
                  ~max_output_columns;
           if force_show
           then show_bookmarks_without_feature bookmarks_without_feature ~display_ascii
                  ~max_output_columns)))
;;
