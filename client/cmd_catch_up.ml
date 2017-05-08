open! Core
open! Async
open! Import

let clear =
  let ok_if_nothing_cleared_switch = "-ok-if-nothing-cleared" in
  let only_those_reviewed_by_switch = "-only-those-reviewed-by" in
  Command.async'
    ~summary:"clear all catch-up review for a user in a feature"
    ~readme:(fun () ->
      concat [ "\
This command fails if any of the following are true:

* no catch-up exists for that user in that feature, unless
  [";ok_if_nothing_cleared_switch;"] is supplied.

* the feature provided does not exist and is not a valid archived feature.

* [-for] is supplied, unless requested by an Iron admin.
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path =
       catch_up_feature_path_or_current_bookmark
     and for_ = for_
     and ok_if_nothing_cleared =
       no_arg_flag ok_if_nothing_cleared_switch
         ~doc:"do not fail if there is no catch-up to clear (idempotent)"
     and only_those_reviewed_by =
       flag only_those_reviewed_by_switch (optional string)
         ~doc:"USER_BLANG only clear those catch-up reviewed by a user satisfying \
               the specified boolean expression.  default is [true]."
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let only_those_reviewed_by =
         match only_those_reviewed_by with
         | None -> Blang.true_
         | Some str ->
           try str |> Sexp.of_string |> [%of_sexp: Unresolved_name.t Blang.t]
           with
           | exn ->
             raise_s
               [%sexp
                 (concat [ "invalid boolean expression provided with "
                         ; only_those_reviewed_by_switch
                         ] : string)
               , (str : string)
               , (exn : Exn.t)
               ]
       in
       Clear_catch_up_sessions.rpc_to_server_exn
         { feature_path
         ; for_
         ; ok_if_nothing_cleared
         ; only_those_reviewed_by
         })
;;

let get_catch_up_session_exn feature_path for_ =
  match%map Get_catch_up_session.rpc_to_server_exn { feature_path; for_ } with
  | `Catch_up_session catch_up_session -> catch_up_session
  | `Up_to_date ->
    failwithf !"no review to catch up on for %{User_name} in %{Feature_path}"
      for_ feature_path ()
;;

let diff =
  Command.async'
    ~summary:"show diffs from a catch-up session"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path          = catch_up_feature_path_or_current_bookmark
     and context               = context ()
     and lines_required_to_separate_ddiff_hunks_override =
       lines_required_to_separate_ddiff_hunks_override
     and for_                  = for_
     and may_modify_local_repo = may_modify_local_repo
     and session_id            = which_session
     and which_files           = which_files
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind repo_root =
         Cmd_workspace.repo_for_hg_operations_exn feature_path
           ~use:`Share_or_clone_if_share_does_not_exist
       in
       let%bind { Get_catch_up_session.Catch_up_session.
                  diff4s_to_catch_up
                ; catch_up_session_id
                ; catch_up_session_tip
                ; remote_repo_path
                ; reviewer_in_session
                ; lines_required_to_separate_ddiff_hunks
                ; _
                } =
         get_catch_up_session_exn feature_path for_
       in
       (match session_id with
        | Current_session -> ()
        | This_session session_id ->
          ok_exn (Session_id.check ~actual:catch_up_session_id ~supplied:session_id));
       let%bind () =
         if not may_modify_local_repo
         then return ()
         else
           Hg.pull repo_root ~from:remote_repo_path
             ~even_if_unclean:true (`Rev catch_up_session_tip)
       in
       let diff4s =
         diff4s_to_catch_up
         |> List.map ~f:(fun diff4_to_catch_up ->
           diff4_to_catch_up
           |> Diff4_to_catch_up.diff4_in_session
           |> Diff4_in_session.diff4)
       in
       let diff4s =
         Command.Param.Which_files.restrict_exn which_files diff4s
           ~path_in_repo:Diff4.path_in_repo_at_f2
           ~from:"catch-up session"
       in
       let lines_required_to_separate_ddiff_hunks =
         Option.value lines_required_to_separate_ddiff_hunks_override
           ~default:lines_required_to_separate_ddiff_hunks
       in
       Cmd_review.print_diff4s
         ~repo_root
         ~diff4s
         ~reviewer:(`Reviewer reviewer_in_session)
         ~context
         ~lines_required_to_separate_ddiff_hunks)
;;

let is_needed =
  Command.async'
    ~summary:"print [true] or [false], depending on whether the catch-up review is needed"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = catch_up_feature_path_or_current_bookmark
     and for_ = for_
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind reaction =
         Get_catch_up_session.rpc_to_server_exn { feature_path; for_ }
       in
       let is_needed =
         match reaction with
         | `Up_to_date -> false
         | `Catch_up_session _ -> true
       in
       printf "%b\n" is_needed;
       return ()
    )
;;

let mark_file ~deprecated =
  let subsumed_by = "[fe catch-up mark-file]" in
  Command.async'
    ~summary:"mark as caught up files in a catch-up session"
    ~readme:(fun () ->
      if deprecated
      then concat [ "\
This command is deprecated and has been subsumed by " ; subsumed_by ; ".
"]
      else ""
    )
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path  = catch_up_feature_path
     and for_          = for_
     and which_session = which_session
     and paths_in_repo = paths_in_repo
     in
     fun () ->
       let open! Deferred.Let_syntax in
       if deprecated && am_functional_testing
       then failwithf "This command is deprecated.  \
                       Tests should be updated to use %s instead" subsumed_by ();
       let feature_path = ok_exn feature_path in
       match%bind Get_catch_up_session.rpc_to_server_exn { feature_path; for_ } with
       | `Up_to_date ->
         raise_s [%sexp "catch up is up to date, cannot mark file"
                      , (paths_in_repo : Path_in_repo.t list)]
       | `Catch_up_session
           { Get_catch_up_session.Catch_up_session.
             catch_up_session_id
           ; diff4s_to_catch_up
           ;  _ } ->
         (match which_session with
          | Current_session -> ()
          | This_session session_id ->
            ok_exn (Session_id.check ~actual:catch_up_session_id ~supplied:session_id));
         let diff4s_in_session =
           List.map diff4s_to_catch_up ~f:Diff4_to_catch_up.diff4_in_session
         in
         let table = Path_in_repo.Table.create () in
         List.iter diff4s_in_session ~f:(fun diff4_in_session ->
           let key = Diff4.path_in_repo_at_f2 (Diff4_in_session.diff4 diff4_in_session) in
           Hashtbl.set table ~key ~data:diff4_in_session);
         let ids_to_mark, invalid_files =
           List.partition_map paths_in_repo ~f:(fun path_in_repo ->
             match Hashtbl.find table path_in_repo with
             | None       -> `Snd path_in_repo
             | Some diff4 -> `Fst (Diff4_in_session.id diff4))
         in
         (if not (List.is_empty invalid_files)
          then raise_s [%sexp "files not found in the current session"
                            , (invalid_files : Path_in_repo.t list)]);
         Catch_up_diffs.rpc_to_server_exn
           { for_
           ; feature_path
           ; catch_up_session_id
           ; diff4_in_session_ids = ids_to_mark
           }
    )
;;

module Attribute = struct
  module T = struct
    type t =
      | Session_id
    [@@deriving enumerate, sexp]
  end

  include T

  let sequence =
    Command.Param.enum_no_args (module T)
      ~doc:(fun ~name _ -> sprintf "show %s" name)
  ;;
end

let show =
  Command.async'
    ~summary:"show a catch-up session or some of its attributes"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = catch_up_feature_path_or_current_bookmark
     and for_ = for_
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     and omit_header_and_description =
       no_arg_flag "-omit-header-and-description"
         ~doc:"don't include feature name and descriptions"
     and omit_attribute_table =
       no_arg_flag "-omit-attribute-table" ~doc:"don't include feature attributes"
     and attributes = Attribute.sequence
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%map ({ Get_catch_up_session.Catch_up_session.
                  catch_up_session_id
                ; catch_up_session_tip
                ; diff4s_to_catch_up
                ; reviewer_in_session
                ; _
                } as catch_up_session) =
         get_catch_up_session_exn feature_path for_
       in
       let diff4s_to_review =
         diff4s_to_catch_up
         |> List.map ~f:Diff4_to_review.catch_up
       in
       let get_attribute : Attribute.t -> Sexp.t = function
         | Session_id -> catch_up_session_id |> [%sexp_of: Session_id.t]
       in
       (match attributes with
        | [] ->
          if not omit_header_and_description then
            Cmd_review.print_catch_up_header_and_description catch_up_session;
          if not omit_attribute_table then
            Cmd_review.print_catch_up_attribute_table catch_up_session
              ~display_ascii ~max_output_columns;
          Cmd_review.print_introduction_summary_for_review
            ~feature_path
            ~review_session_tip:catch_up_session_tip
            ~reviewer_in_session
            ~warn_reviewer:None
            ~diff4s_to_review
            ~display_ascii
            ~max_output_columns;
        | [ attribute ] ->
          print_endline (get_attribute attribute |> Sexp.to_string_hum)
        | attributes ->
          attributes
          |> List.map ~f:(fun attribute -> attribute, get_attribute attribute)
          |> [%sexp_of: (Attribute.t * Sexp.t) list]
          |> Sexp.to_string_hum
          |> print_endline))
;;

let command =
  Command.group ~summary:"manage your catch up duties"
    [ "clear"    , clear
    ; "diff"     , diff
    ; "is-needed", is_needed
    ; "mark-file", mark_file ~deprecated:false
    ; "review"   , Cmd_review.catch_up_review_command
    ; "show"     , show
    ]
;;
