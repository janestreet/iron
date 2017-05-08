open! Core
open! Import
open! Iron_common.Std
open! Iron_hg.Std

module Thread_safe = Async.Thread_safe

module T = struct

include Command.Param
open! Command.Let_syntax

let bool =
  Arg_type.of_alist_exn (List.map [ true; false ] ~f:(fun b -> (Bool.to_string b, b)))
;;

let path_in_repo_arg_type =
  Arg_type.file (fun file ->
    match Repo_root.program_started_in with
    | Error _ -> Path_in_repo.of_string file
    | Ok repo_root ->
      Repo_root.relativize_exn repo_root
        (ok_exn (Abspath.simplify_dotdots_syntax
                   (Path.resolve_relative_to_program_started_in (Path.of_string file)))))
;;

let path_in_repo_anons = "FILE" %: path_in_repo_arg_type
let path_in_repo  = anon path_in_repo_anons
let paths_in_repo = anon (sequence path_in_repo_anons)

let time_span = Arg_type.create Time.Span.of_string
let time = Arg_type.create Time.of_string

module type Blangable = sig
  type t [@@deriving sexp]
  val to_string : t -> string
end

let blang_arg_type_with_completion
      (type a) (module Blangable : Blangable with type t = a) get_universe =
  let complete _ ~part =
    match get_universe () with
    | Error err ->
      Core.eprintf !"\n%{sexp:Error.t}\n%!" err;
      []
    | Ok universe ->
      let sexp_start, partial_atom =
        match String.rsplit2 part ~on:' ' with
        | Some (a, b) -> a ^ " ", b
        | None -> "", part
      in
      List.filter_map universe ~f:(fun value ->
        let value = Blangable.to_string value in
        if String.is_prefix ~prefix:partial_atom value
        then Some (sexp_start ^ value)
        else None)
  in
  Arg_type.create
    ~complete
    (fun str -> Sexp.of_string_conv_exn str [%of_sexp: Blangable.t Blang.t])
;;

let rec params = function
  | [] -> return []
  | p :: ps -> map2 p (params ps) ~f:(fun a list -> a :: list)
;;

let bool_with name ~default ~doc =
  flag name (optional_with_default default bool)
    ~doc:(sprintf "BOOL %s (default is %s)" doc (Bool.to_string default))
;;

let enum (type a) (m : a Enum.t) =
  let module M = (val m) in
  List.map M.all ~f:(fun a -> (Enum.to_string_hum m a, a))
;;

let projections =
  anon (sequence ("PROJECTION" %: Arg_type.create Build_projection_name.of_string))
;;

let enum_arg_type m = Arg_type.of_alist_exn (enum m)

let make_enum ~f switch ~doc m =
  let enum = enum m in
  let choices =
    enum
    |> List.map ~f:fst
    |> List.sort ~cmp:String.alphabetic_compare
    |> String.concat ~sep:" | "
  in
  let doc =
    if String.is_empty doc
    then concat [ "_ " ; choices ]
    else concat [ doc; ": "; choices ]
  in
  flag switch ~doc (f (Arg_type.of_alist_exn enum))
;;

let enum_optional switch ~doc m = make_enum ~f:optional switch ~doc m
let enum_required switch ~doc m = make_enum ~f:required switch ~doc m

let enum_anon      name m = anon           (name %: enum_arg_type m)
let enum_anon_list name m = anon (sequence (name %: enum_arg_type m))

let no_arg_flag ?aliases switch ~doc =
  let doc =
    if String.length doc = 0 || Char.equal doc.[ 0 ] ' '
    then doc
    else concat [ " "; doc ]
  in
  flag ?aliases switch no_arg ~doc
;;

let create_catch_up_for_me =
  let%map create_catch_up_for_me =
    no_arg_flag Switch.create_catch_up_for_me
      ~doc:"create catch-up review even when reviewing for oneself"
  in
  fun ~is_reviewing_for ->
    if create_catch_up_for_me
    && not (User_name.Or_all_or_all_but.mem is_reviewing_for User_name.unix_login)
    then
      Or_error.errorf "Cannot use [%s] when reviewing for other users only."
        Switch.create_catch_up_for_me
    else Ok create_catch_up_for_me
;;

let resolved_file_path_arg_type =
  Arg_type.file (fun string ->
    Path.resolve_relative_to_program_started_in (Path.of_string string))
;;

let resolved_file_path = anon ("FILE" %: resolved_file_path_arg_type)

let enum_no_args ?(aliases=const []) m ~doc =
  map ~f:List.filter_opt
    (params
       (List.map (enum m) ~f:(fun (name, value) ->
          map (no_arg_flag (concat [ "-"; name ])
                 ~aliases:(aliases value)
                 ~doc:(doc ~name value))
            ~f:(fun b -> if b then Some value else None))))
;;

let create_rev_from_local_repo string = Raw_rev.String string

let rev_arg_type : Raw_rev.t Arg_type.t =
  Arg_type.create create_rev_from_local_repo
;;

let rev_from_string_40_or_local_repo_arg_type =
  Arg_type.create (fun string ->
    match Rev.of_string_40 string with
    | exception _ -> create_rev_from_local_repo string
    | rev -> Raw_rev.Rev rev)
;;

let rev = anon ("REV" %: rev_arg_type)

let completion_problem error =
  Core.eprintf !"\n%{sexp:Error.t}\n%!" error;
  []
;;

let complete_or_error types =
  fun _ ~part:prefix ->
    (* If any problem, give up: no completions. *)
    match
      Thread_safe.block_on_async (fun () ->
        Iron_protocol.Complete.rpc_to_server_exn
          { types; prefix })
    with
    | Ok _ as completions -> completions
    | Error exn ->
      Or_error.error_s
        [%sexp
          "unless the server is down, please report this bug in Iron \
           server's completion"
        , { types  : Iron_protocol.Complete.Type.t list
          ; prefix : string
          ; exn    : Exn.t
          }
        ]
;;

let complete types =
  fun hmap ~part ->
    match complete_or_error types hmap ~part with
    | Ok completions -> completions
    | Error err -> completion_problem err
;;

let complete_in_sequence types =
  fun umap ~part ->
    List.find_map types ~f:(fun type_ ->
      match complete_or_error [type_] umap ~part with
      | Ok [] -> None
      | (Error _ | Ok (_::_)) as stop -> Some stop)
    |> function
    | None -> []
    | Some (Ok completions) -> completions
    | Some (Error err) -> completion_problem err
;;

let user_name =
  Arg_type.create User_name.of_string
    ~complete:(complete [User_info Existing_user])
;;

let user_name_or_all =
  User_name.Or_all.arg_type ~complete_user_name:(complete [User_info Existing_user])
;;

let alias =
  Arg_type.create Alternate_name.of_string ~complete:(complete [User_info Alias])
;;

let typo =
  Arg_type.create Alternate_name.of_string ~complete:(complete [User_info Typo])
;;

module Match_result = struct
  type t =
    | No_match
    | Match of Feature_path.t
    | Cannot_disambiguate
  [@@deriving compare, sexp_of]
end

let find_preferred_feature_path_heuristic ~matching_features ~partial_name_prefix
  : Match_result.t =
  match matching_features with
  | [] -> No_match
  | [ feature_path ] -> Match feature_path
  | _ :: _ :: _ ->
    let no_slash = String.try_chop_suffix partial_name_prefix ~suffix:"/" in
    let strict_equality feature_path =
      String.(=) no_slash (Feature_path.to_string feature_path)
    in
    let strict_suffix feature_path =
      String.is_suffix ~suffix:("/" ^ no_slash) (Feature_path.to_string feature_path)
    in
    let project feature_path =
      match Feature_path.parts feature_path with
      | [ _ ; project ] -> String.(=) no_slash (Feature_name.to_string project)
      | _ -> false
    in
    (* Heuristic in priority order to distinguish ties *)
    match
      List.find_map
        [ strict_equality
        ; strict_suffix
        ; project
        ]
        ~f:(fun pred ->
          match List.filter matching_features ~f:pred with
          | [ feature_path ] -> Some feature_path
          | [] | _ :: _ :: _ -> None)
    with
    | None -> Cannot_disambiguate
    | Some feature_path -> Match feature_path
;;

let%test_unit _ =
  let check list partial_name_prefix ~expect =
    let matching_features = List.map list ~f:Feature_path.of_string in
    [%test_result: Match_result.t]
      ~expect:(Match (Feature_path.of_string expect))
      (find_preferred_feature_path_heuristic ~matching_features ~partial_name_prefix)
  in
  check
    [ "jane/oculus"
    ; "jane/grass/oculus"
    ] "oculus" ~expect:"jane/oculus";
  check
    [ "jane/fe"
    ; "jane/fe/a"
    ] "fe" ~expect:"jane/fe"
;;

(* Check if [str] is a feature path of existing feature.  If it is not, try to find a
   single feature whose partial_name matches [str].  Fail if there are multiple matches *)
let feature_path_of_string_or_partial_name_internal_exn
      ?(parse_partial_name_if_no_match=false) partial_name_prefix ~namespace =
  let open Async in
  let%bind matching_features =
    Iron_protocol.Find_features_by_partial_name.rpc_to_server_exn
      { partial_name_prefix; namespace }
  in
  let matching_features = List.sort matching_features ~cmp:Feature_path.compare in
  match find_preferred_feature_path_heuristic ~matching_features ~partial_name_prefix with
  | No_match ->
    let fail () =
      raise_s [%sexp "no such feature", (partial_name_prefix : string)]
    in
    if not parse_partial_name_if_no_match
    then fail ()
    else (
      match Feature_path.of_string partial_name_prefix with
      | exception  _ -> fail ()
      | feature_path -> return feature_path)
  | Match feature_path -> return feature_path
  | Cannot_disambiguate ->
    (match Repo_root.program_started_in with
     | Error _ ->
       raise_s [%sexp "cannot disambiguate among features"
                    , (matching_features : Feature_path.t list)]
     | Ok repo_root ->
       let%map desired_family = Workspace_hgrc.extract_root_feature_from_hgrc repo_root in
       (match desired_family with
        | Error error ->
          raise_s
            [%sexp
              "cannot disambiguate among features, and cannot determine your repo family",
              { matching_features : Feature_path.t list
              ; error             : Error.t
              }
            ]
        | Ok desired_family ->
          let (matching_features_in_this_repo, matching_features_in_other_repos) =
            List.partition_tf matching_features ~f:(fun feature_path ->
              Feature_name.equal (Feature_path.root feature_path) desired_family)
          in
          (if List.is_empty matching_features_in_other_repos
           then
             raise_s [%sexp "cannot disambiguate among features"
                          , (matching_features_in_this_repo : Feature_path.t list)]);
          (match
             find_preferred_feature_path_heuristic
               ~matching_features:matching_features_in_this_repo
               ~partial_name_prefix
           with
           | Match feature_path -> feature_path
           | No_match ->
             raise_s
               [%sexp (sprintf "\
cannot disambiguate among features, and none of them are in the %s repo"
                         (Feature_name.to_string desired_family) : string)
                    , (matching_features_in_other_repos : Feature_path.t list)
               ]
           | Cannot_disambiguate ->
             raise_s
               [%sexp
                 (sprintf "cannot disambiguate among features in the %s repo"
                    (Feature_name.to_string desired_family) : string),
                 { matching_features_in_this_repo   : Feature_path.t list
                 ; matching_features_in_other_repos : Feature_path.t list
                 }
               ])))
;;

let feature_path_of_string_or_partial_name_exn partial_name_prefix ~namespace =
  if Iron_options.find_features_by_partial_name
  then feature_path_of_string_or_partial_name_internal_exn partial_name_prefix ~namespace
  else Async.return (Feature_path.of_string partial_name_prefix)
;;

let blocking_feature_path_of_string_or_partial_name partial_name_prefix ~namespace =
  match Thread_safe.block_on_async (fun () ->
    feature_path_of_string_or_partial_name_exn partial_name_prefix ~namespace)
  with
  | Ok _ as ok -> ok
  | Error exn  -> Or_error.of_exn (Async.Monitor.extract_exn exn)
;;

let feature_arg_type ~match_existing_feature =
  Arg_type.create
    (if match_existing_feature
     then blocking_feature_path_of_string_or_partial_name ~namespace:`Existing
     else (fun s -> Or_error.try_with (fun () -> Feature_path.of_string s)))
    ~complete:(complete [if match_existing_feature
                         then Feature_path
                         else Absolute_feature_path])
;;

let archived_feature_arg_type =
  Arg_type.create
    (blocking_feature_path_of_string_or_partial_name ~namespace:`Archived)
    ~complete:(complete [ Archived_feature_path ])
;;

let catch_up_feature_arg_type =
  Arg_type.create
    (blocking_feature_path_of_string_or_partial_name ~namespace:`All)
    ~complete:(complete [ Feature_path_with_catch_up ])
;;

let active_or_catch_up_feature_arg_type =
  Arg_type.create
    (blocking_feature_path_of_string_or_partial_name ~namespace:`All)
    ~complete:(complete [ Feature_path; Feature_path_with_catch_up ])
;;

let root_feature_arg_type =
  Arg_type.create Feature_name.of_string
    ~complete:(complete [Root_feature_path])
;;

let unverified_workspace_arg_type =
  Arg_type.create
    (fun s -> Or_error.try_with (fun () -> Feature_path.of_string s))
    ~complete:(complete [ Feature_path; Archived_feature_path ])
;;

let clone_of_root_feature_of ~doc =
  flag ~doc Switch.clone_of_root_feature_of (listed unverified_workspace_arg_type)
  |> map ~f:(fun list ->
    list
    |> Or_error.combine_errors
    |> Or_error.map ~f:(fun list ->
      list
      |> List.map ~f:Feature_path.root
      |> Feature_name.Set.of_list))
;;

let feature_anons_gen ~match_existing_feature =
  "FEATURE" %: (feature_arg_type ~match_existing_feature)
;;

let feature_anons = feature_anons_gen ~match_existing_feature:true

let feature_path_gen ~match_existing_feature =
  anon (feature_anons_gen ~match_existing_feature)
;;

let feature_path_option_gen ~match_existing_feature =
  map (anon (maybe (feature_anons_gen ~match_existing_feature)))
    ~f:(function
      | Some (Error _ as e) -> e
      | Some (Ok p) -> Ok (Some p)
      | None -> Ok None)
;;

let feature_path          = feature_path_gen ~match_existing_feature:true

let absolute_feature_path = feature_path_gen ~match_existing_feature:false

let absolute_feature_path_option =
  feature_path_option_gen ~match_existing_feature:false
;;

let catch_up_feature_anons = "FEATURE" %: catch_up_feature_arg_type

let active_or_catch_up_feature_anons = "FEATURE" %: active_or_catch_up_feature_arg_type

let catch_up_feature_path = anon catch_up_feature_anons

let archived_feature_path = anon ("FEATURE" %: archived_feature_arg_type)

let root_feature = anon ("ROOT-FEATURE" %: root_feature_arg_type)

let feature_path_option =
  feature_path_option_gen ~match_existing_feature:true
;;

let feature_path_flagged ~label ~doc =
  flag label (required (feature_arg_type ~match_existing_feature:true)) ~doc
;;

let feature_path_flagged_listed ~label ~doc =
  map (flag label (listed (feature_arg_type ~match_existing_feature:true)) ~doc)
    ~f:Or_error.combine_errors
;;

let current_bookmark () =
  let open Async in
  let client_config = Client_config.get () in
  let result =
    if Client_config.may_infer_feature_path_from_current_bookmark client_config
    then (
      match Repo_root.program_started_in with
      | Error _ as e -> return e
      | Ok repo_root ->
        (* The main reason that we use [current_bookmark] here rather than
           [active_bookmark] is because it is faster, and this is interacting with the
           user at the command line.  Also, the current bookmark was the most recently
           active, and will let us infer strictly more cases than if we used active
           bookmark. *)
        match%map Hg.current_bookmark repo_root with
        | Error _ as e -> e
        | Ok feature_path_string ->
          Or_error.try_with (fun () -> Feature_path.of_string feature_path_string))
    else
      Deferred.Or_error.error_string "feature-path inference is disabled via [.ferc]"
  in
  match%map result with
  | Ok _ as x -> x
  | Error e ->
    error "could not determine feature you want to use" e [%sexp_of: Error.t]
;;

let make_feature_path_or_current_bookmark feature_anons =
  map (anon (maybe feature_anons)) ~f:(function
    | Some (Ok feature_path) -> Ok feature_path
    | Some (Error _ as e) -> e
    | None ->
      match Thread_safe.block_on_async (fun () -> current_bookmark ()) with
      | Error exn -> Or_error.of_exn (Async.Monitor.extract_exn exn)
      | Ok result -> result)
;;

module Maybe_archived_feature_spec : sig
  val maybe_archived_feature : Maybe_archived_feature_spec.Command_line.t Or_error.t t
  val resolve_maybe_archived_feature_spec_exn
    : Maybe_archived_feature_spec.Command_line.t
    -> Maybe_archived_feature_spec.t Async.Deferred.t
end = struct

  let complete_with_archived_flag_u_key =
    Univ_map.With_default.Key.create ~name:Switch.archived
      [%sexp_of: bool] ~default:false
  ;;

  let complete_with_existing_or_most_recently_archived_flag_u_key =
    Univ_map.With_default.Key.create ~name:Switch.existing_or_most_recently_archived
      [%sexp_of: bool] ~default:false
  ;;

  let complete_with_archive_flag hmap =
    if Univ_map.With_default.find hmap complete_with_archived_flag_u_key
    then complete [Archived_feature_path] hmap
    else if Univ_map.With_default.find hmap
              complete_with_existing_or_most_recently_archived_flag_u_key
    then complete_in_sequence [Feature_path; Archived_feature_path] hmap
    else complete [Feature_path] hmap
  ;;

  let raw_feature_spec_of_string_or_partial_name string =
    Or_error.try_with (fun () ->
      match Option.try_with (fun () -> Feature_id.of_string string) with
      | Some id -> `Feature_id id
      | None ->
        if Iron_options.find_features_by_partial_name
        then `Partial_name string
        else `Feature_path (Feature_path.of_string string))
  ;;

  let raw_feature_spec_arg_type =
    Arg_type.create
      (raw_feature_spec_of_string_or_partial_name)
      ~complete:(complete_with_archive_flag)
  ;;

  let resolve_maybe_archived_feature_spec_exn
    { Maybe_archived_feature_spec.Command_line.
      feature_spec
    ; namespace
    } =
    let open Async in
    match feature_spec with
    | ( `Feature_id _ | `Feature_path _ ) as feature_spec ->
      return
        { Maybe_archived_feature_spec.
          feature_spec
        ; namespace
        }
    | `Partial_name partial_name ->
      (* we parse the partial_name in case of no match here because the reason might be
         that the namespace is incorrect.  The server is some cases is able to build a
         more precise error message while responding to some queries. *)
      let%map feature_path =
        feature_path_of_string_or_partial_name_internal_exn
          ~parse_partial_name_if_no_match:true partial_name ~namespace
      in
      { Maybe_archived_feature_spec.
        feature_spec = `Feature_path feature_path
      ; namespace
      }
    | `Current_bookmark ->
      let%map feature_path = current_bookmark () in
      { Maybe_archived_feature_spec.
        feature_spec = `Feature_path (ok_exn feature_path)
      ; namespace
      }
  ;;

  let maybe_archived_feature =
    return (fun raw_feature_path archived_flag existing_or_most_recently_archived_flag ->
      Or_error.try_with (fun () ->
        let (feature_spec : Maybe_archived_feature_spec.Command_line.Feature_spec.t) =
          match raw_feature_path with
          | None -> `Current_bookmark
          | Some raw_feature_spec ->
            (ok_exn raw_feature_spec :>
               Maybe_archived_feature_spec.Command_line.Feature_spec.t)
        in
        let namespace =
          match archived_flag, existing_or_most_recently_archived_flag with
          | true, true -> failwithf "The switches %s and %s are incompatible."
                            Switch.archived Switch.existing_or_most_recently_archived
                            ()
          | true, false -> `Archived
          | false, true -> `Existing_or_most_recently_archived
          | false, false ->
            match feature_spec with
            | `Current_bookmark | `Feature_id _ -> `All
            | `Feature_path _ | `Partial_name _ -> `Existing
        in
        { Maybe_archived_feature_spec.Command_line.
          feature_spec
        ; namespace
        }))
    <*> anon (maybe ("{FEATURE | UUID}" %: raw_feature_spec_arg_type))
    <*> flag Switch.archived ~doc:" lookup only among archived features"
          (no_arg_register ~key:complete_with_archived_flag_u_key ~value:true)
    <*> flag Switch.existing_or_most_recently_archived
          ~doc:" try finding the most recently archived feature if the feature \
                does not exist"
          (no_arg_register
             ~key:complete_with_existing_or_most_recently_archived_flag_u_key
             ~value:true)
  ;;
end

include Maybe_archived_feature_spec

let feature_path_or_current_bookmark =
  make_feature_path_or_current_bookmark feature_anons
;;

let catch_up_feature_path_or_current_bookmark =
  make_feature_path_or_current_bookmark catch_up_feature_anons
;;

let active_or_catch_up_feature_path_or_current_bookmark =
  make_feature_path_or_current_bookmark active_or_catch_up_feature_anons
;;

let anon_feature_paths =
  map (anon (sequence feature_anons)) ~f:Or_error.combine_errors
;;

let included_features_order =
  let release_time_switch =
    "-order-included-features-by-release-time"
  and release_time_decreasing_switch =
    "-order-included-features-by-decreasing-release-time"
  in
  let%map_open release_time =
    no_arg_flag release_time_switch
      ~doc:"display included features from oldest to newest"
  and release_time_decreasing =
    no_arg_flag release_time_decreasing_switch
      ~doc:"display included features from newest to oldest"
  in
  match release_time, release_time_decreasing with
  | false, false -> Ok `Name
  | true,  false -> Ok `Release_time
  | false, true  -> Ok `Release_time_decreasing
  | true,  true  ->
    Or_error.error_s
      [%sexp "These flags are mutually exclusive."
           , (release_time_decreasing_switch : string)
           , (release_time_switch            : string)
      ]
;;

let include_active_cr_soons =
  no_arg_flag Switch.include_active_cr_soons
    ~doc:"don't hide the cr-soons that are active in some feature"
;;

let interactive =
  map (bool_with "-interactive" ~default:!Async_interactive.interactive
         ~doc:"be interactive via the terminal")
    ~f:(fun bool -> Async_interactive.interactive := bool)
;;

let base =
  flag "-base" (optional rev_arg_type)
    ~doc:"REV the base revision of the feature (default is parent's tip)"
;;

let tip =
  flag "-tip" (optional rev_arg_type)
    ~doc:"REV the tip revision of the feature (default is base)"
;;

let dash_dash_rest ~doc =
  map ~f:(function None -> [] | Some l -> l)
    (flag "--" ~doc:(" " ^ doc) escape)
;;

let description =
  flag "-description" (optional string)
    ~doc:"TEXT what the feature will accomplish"
;;

let display_ascii =
  map ~f:(fun b -> b || Iron_options.display_ascii_always)
    (no_arg_flag "-display-ascii"
       ~doc:"display ASCII characters, with no ANSI color escapes or unicode characters")
;;

let terminal_width = Core_extended.Extended_unix.terminal_width

let terminal_width_command =
  Command.basic
    ~summary:"print the width in columns of the terminal of stderr"
    Command.Spec.empty
    (fun () ->
       printf "%d\n%!" (force terminal_width))
;;

let max_output_columns =
  map (flag "-max-output-columns" (optional int)
         ~doc:"INT maximum column width for table output")
    ~f:(function
      | Some i -> i
      | None   -> force terminal_width)
;;

let emacs = no_arg_flag "-emacs" ~doc:"visit files in emacs"

let even_though_empty =
  no_arg_flag Switch.even_though_empty ~doc:"allow seconding of an empty feature"
;;

let even_though_owner =
  no_arg_flag Switch.even_though_owner ~doc:"allow seconding by a feature owner"
;;

let even_if_locked =
  map ~f:(ignore : bool -> unit)
    (no_arg_flag "-even-if-locked"
       ~doc:" DEPRECATED -- will be removed soon in a subsequent roll")
;;

let even_if_some_files_are_already_reviewed =
  no_arg_flag Switch.even_if_some_files_are_already_reviewed
    ~doc:"do not fail if some files are already reviewed"
;;

let feature_id =
  flag Switch.feature_id ~doc:"FEATURE_ID feature id"
    (required (Arg_type.create Feature_id.of_string))
;;

let feature_id_option =
  flag Switch.feature_id ~doc:"FEATURE_ID feature id"
    (optional (Arg_type.create Feature_id.of_string))
;;

let feature_id_list =
  flag Switch.feature_id ~doc:"FEATURE_ID feature id"
    (listed (Arg_type.create Feature_id.of_string))
;;

let which_session =
  map ~f:(function
    | None    -> Which_session.Current_session
    | Some id -> This_session id)
    (flag "-session-id"
       ~doc:"SESSION_ID fail if the current session id is not the one supplied"
       (optional (Arg_type.create Session_id.of_string)))
;;

let session_id_required =
  flag "-session-id"
    ~doc:"SESSION_ID session id"
    (required (Arg_type.create Session_id.of_string))
;;

let diff4_in_session_ids =
  flag "-diff-id"
    ~doc:"DIFF_ID diff id"
    (listed Diff4_in_session.Id.arg_type)
;;

let for_ =
  flag Switch.for_ (optional user_name) ~doc:"USER for user (default is unix login)"
  |> map ~f:(Option.value ~default:User_name.unix_login)
;;

let review_reason =
  flag Switch.reason (optional_with_default "" string)
    ~doc:"REASON why you are reviewing this for someone else"
;;

let reason_for_archiving =
  flag Switch.reason (optional_with_default "" string)
    ~doc:"REASON why you are archiving this feature"
;;

let for_or_all_required =
  flag Switch.for_ (required user_name_or_all) ~doc:User_name.Or_all.arg_doc
;;

let for_or_all_gen_with_default ~default_value ~default_doc =
  flag Switch.for_ (optional user_name_or_all)
    ~doc:(sprintf "%s for user (default is %s)" User_name.Or_all.arg_doc default_doc)
  |> map ~f:(Option.value ~default:default_value)
;;

let for_or_all_default_me =
  for_or_all_gen_with_default
    ~default_value:(`User User_name.unix_login)
    ~default_doc:"unix login"
;;

let for_or_all_default_all =
  for_or_all_gen_with_default
    ~default_value:`All_users
    ~default_doc:(User_name.Or_all.to_string `All_users)
;;

let no_bookmark =
  no_arg_flag "-no-bookmark" ~doc:"only create the feature, not the bookmark"
;;

let permanent = no_arg_flag "-permanent" ~doc:"make this feature permanent"

let new_base         = flag "-new-base"         (optional rev_arg_type)    ~doc:"REV "
let set_base         = flag "-set-base"         (optional rev_arg_type)    ~doc:"REV "
let set_description  = flag "-set-description"  (optional string) ~doc:"TEXT "
let set_is_permanent = flag "-set-is-permanent" (optional bool)   ~doc:"BOOL "

module Which_diffs = struct
  type t =
    [ `All
    | `Reviewed
    | `Unreviewed
    | `Files of Path_in_repo.t list
    ]
  [@@deriving sexp_of]

  let param =
    return (fun files reviewed unreviewed ->
      match files, reviewed, unreviewed with
      | []     , false, false -> `All
      | _::_   , false, false -> `Files files
      | []     , true , false -> `Reviewed
      | []     , false, true  -> `Unreviewed
      | _ -> failwith "The flags to select which diffs to show are mutually exclusive"
    )
    <*> flag "-file" (listed path_in_repo_arg_type) ~doc:"FILE show only specified files"
    <*> no_arg_flag "-reviewed"   ~doc:"show only already reviewed diffs"
    <*> no_arg_flag "-unreviewed" ~doc:"show only unreviewed diffs"
  ;;
end

module Which_features = struct
  module Switch = struct
    let all  = "-all"
    let rec_ = "-rec"
  end

  let features_list features ~rec_ =
    Which_features.Features (List.map features ~f:(fun feature_path ->
      { Which_features.Feature.
        feature_path
      ; include_descendants = rec_
      }))
  ;;

  let param
        ?(allow_rec_flag = true)
        ?(allow_unexisting_features = false)
        ~allow_empty_selection ~default_to_current_bookmark
        () =
    let open Command.Let_syntax in
    let%map all = flag Switch.all no_arg ~doc:" include all features"
    and rec_ =
      if not allow_rec_flag
      then return false
      else flag Switch.rec_ no_arg
             ~doc:" include recursively each descendant of given features"
    and features =
      if not allow_unexisting_features
      then anon_feature_paths
      else
        anon (sequence ("FEATURE" %: unverified_workspace_arg_type))
        |> map ~f:Or_error.combine_errors
    in
    Lazy.from_fun (fun () ->
      let open Async in
      let features = ok_exn features in
      if all
      then
        if rec_
        then failwith (concat [ "the switches " ; Switch.all ; " and " ; Switch.rec_ ;
                                " are not compatible" ])
        else if not (List.is_empty features)
        then failwith (concat [ "don't list features when using " ; Switch.all ])
        else return Which_features.All_features
      else
      if List.is_empty features
      then (
        let%map features =
          if default_to_current_bookmark
          then (
            match%map current_bookmark () with
            | Error err  -> if not allow_empty_selection then Error.raise err else []
            | Ok feature -> [ feature ])
          else return []
        in
        if List.is_empty features && not allow_empty_selection
        then failwith (concat [ "specify some features or use " ; Switch.all ])
        else features_list features ~rec_)
      else
        return (features_list features ~rec_))
  ;;
end

module Which_files = struct
  type t =
    [ `All
    | `Files of Path_in_repo.t list
    ]
  [@@deriving sexp_of]

  let param =
    return (function
      | []   -> `All
      | (_::_) as files -> `Files files)
    <*> flag "-file" (listed path_in_repo_arg_type)
          ~doc:"FILE show only specified file(s)"
  ;;

  let restrict_exn t elts ~path_in_repo ~from =
    match t with
    | `All -> elts
    | `Files files ->
      let by_path_in_repo =
        elts
        |> List.map ~f:(fun elt -> path_in_repo elt, elt)
        |> Path_in_repo.Table.of_alist_multi
      in
      let files_not_found = ref [] in
      let elts =
        List.concat_map files ~f:(fun path_in_repo ->
          match Hashtbl.find by_path_in_repo path_in_repo with
          | Some elts -> elts
          | None -> files_not_found := path_in_repo :: !files_not_found; [])
      in
      match !files_not_found with
      | [] -> elts
      | (_::_) as not_found ->
        raise_s
          [%sexp
            (sprintf "no such file%s in %s"
               (if List.length not_found > 1 then "s" else "")
               from : string)
          , (List.sort not_found ~cmp:Path_in_repo.default_review_compare
             : Path_in_repo.t list)
          ]
  ;;
end

let which_diffs    = Which_diffs.param
let which_features = Which_features.param
let which_files    = Which_files.param

let sort_build_order =
  no_arg_flag "-sort-build-order"
    ~doc:"attempt to sort files in build order, computed via build artifacts"
;;

module Review_sort = struct
  type t =
    [ `By_increasing_review_lines
    | `By_decreasing_review_lines
    | `Using_file of Abspath.t
    | `Build_order
    ]
  [@@deriving sexp_of]

  let param =
    return (fun using_file by_increasing_review_lines by_decreasing_review_lines build_order ->
      match using_file
          , by_increasing_review_lines
          , by_decreasing_review_lines
          , build_order
      with
      | None     , false, false, false -> None
      | Some file, false, false, false -> Some (`Using_file file)
      | None     , true , false, false -> Some `By_increasing_review_lines
      | None     , false, true , false -> Some `By_decreasing_review_lines
      | None     , false, false, true  -> Some `Build_order
      | _ -> failwith "The different review sorting flags are mutually exclusive")
    <*> flag "-sort" (optional resolved_file_path_arg_type)
          ~doc:"FILE order the files to be reviewed using the list in this file."
    <*> no_arg_flag "-sort-by-increasing-lines"
          ~doc:" order the files to be reviewed by increasing number of lines to review."
    <*> no_arg_flag "-sort-by-decreasing-lines"
          ~doc:" order the files to be reviewed by increasing number of lines to review."
    <*> sort_build_order
  ;;
end

let maybe_sort_review_files = Review_sort.param

let context ?(default=12) () =
  map (flag "-context" (optional int)
         ~doc:(sprintf "N number of lines of context to show (default %d)"
                 default))
    ~f:(function
      | Some context -> context
      | None ->
        let client_config = Client_config.get () in
        Option.value (Client_config.Cmd.context client_config) ~default)
;;

let lines_required_to_separate_ddiff_hunks_override =
  flag Switch.lines_required_to_separate_ddiff_hunks (optional int)
    ~doc:"INT override the required spacing to separate two ddiff hunks"
;;

let lines_required_to_separate_ddiff_hunks_with_default =
  let default = Constants.lines_required_to_separate_ddiff_hunks_default in
  flag Switch.lines_required_to_separate_ddiff_hunks (optional_with_default default int)
    ~doc:(sprintf "INT required spacing to separate two ddiff hunks (default %d)" default)
;;

let email_address_list_arg_type =
  Arg_type.comma_separated (Arg_type.create Email_address.of_string)
    ~strip_whitespace:true
    ~unique_values:true
;;

let comma_delim_list_arg_type of_string complete =
  Arg_type.comma_separated ~strip_whitespace:false ~unique_values:true
    (Arg_type.create of_string ~complete:(fun univ_map ~part ->
       try complete univ_map ~part
       with exn ->
         completion_problem
           (Error.create "please report this bug in Iron completion"
              exn [%sexp_of: exn])))
;;

let enum_list_arg_type (type a) (m : a Enum.t) =
  let module M = (val m) in
  let all = enum m in
  let map = String.Map.of_alist_exn all in
  let of_string str =
    match Map.find map str with
    | Some value -> value
    | None -> failwithf "invalid value %s" str ()
  in
  let all_str = List.map all ~f:fst in
  let complete _univ_map ~part =
    List.filter all_str ~f:(String.is_prefix ~prefix:part)
  in
  comma_delim_list_arg_type of_string complete
;;

let enum_list switch ~doc enum =
  map (flag switch ~doc (optional (enum_list_arg_type enum)))
    ~f:(function
      | None -> []
      | Some list -> list)
;;

let user_list_arg_type =
  comma_delim_list_arg_type User_name.of_string (complete [User_info Existing_user])
;;

let email_addresses_option ~switch =
  map (flag switch ~doc:"EMAIL[,EMAIL...] " (optional email_address_list_arg_type))
    ~f:(Option.map ~f:Email_address.Set.of_list)
;;

let send_email_upon ~switch ~doc =
  map (flag switch ~doc:(concat [ "ACTION[,ACTION...] "; doc ])
         (optional (enum_list_arg_type (module Send_email_upon))))
    ~f:(function
      | None -> None
      | Some list -> Some (Send_email_upon.Set.of_list list))
;;

let user_list_option ~switch =
  flag switch ~doc:"USER[,USER...] "
    (optional user_list_arg_type)
;;

let users_option ~switch =
  map (user_list_option ~switch)
    ~f:(Option.map ~f:User_name.Set.of_list)
;;

let property_list_arg_type =
  Arg_type.comma_separated (Arg_type.create Property.of_string)
    ~strip_whitespace:true ~unique_values:true
;;

let properties_option ~switch ~doc =
  flag switch (optional property_list_arg_type) ~doc
;;

let properties_list_option ~switch ~verb =
  properties_option ~switch
    ~doc:(sprintf "ATTR[,ATTR...] %s user-defined properties" verb)
;;

let properties_set_option ~switch ~verb =
  map (properties_option ~switch
    ~doc:(sprintf "ATTR[,ATTR...] %s user-defined properties" verb))
    ~f:(Option.map ~f:Property.Set.of_list)
;;

let inheritable_properties_set_option ~switch =
  map (properties_option ~switch ~doc:"ATTR[,ATTR...] ")
    ~f:(Option.map ~f:Property.Set.of_list)
;;

let property_values_flag ~switch ~doc =
  let separator = '=' in
  let syntax = sprintf "ATTR%cSEXP" separator in
  map (flag switch ~doc:(sprintf "%s %s" syntax doc)
         (listed (Arg_type.create (fun x ->
            match String.lsplit2 x ~on:separator with
            | None ->
              raise_s
                [%sexp (sprintf "expected key value pair of the form %s but got"
                          syntax : string), (x : string)]
            | Some (key, value) ->
              (key, Sexp.of_string value)
          ))))
    ~f:(function
      | [] -> None
      | _::_ as alist -> Some (Property.Map.of_alist_exn alist))
;;

let owners =
  map (flag "-owners" (optional user_list_arg_type)
         ~doc:"OWNER[,OWNER...] the owner(s) of the feature")
    ~f:(function
      | None -> [ User_name.unix_login ]
      | Some other -> other)
;;

let remote_repo_path =
  flag Switch.remote_repo_path ~doc:"URL location of the shared repo (e.g., ssh://...)"
    (optional
       (Arg_type.create Remote_repo_path.of_string
          ~complete:(complete [Remote_repo_path])))
;;

let verbose = no_arg_flag "-verbose" ~doc:"be more verbose"

let may_modify_local_repo =
  map ~f:not
    (no_arg_flag Switch.do_not_modify_local_repo
       ~doc:"don't run hg commands that modify the local repo")
;;

let do_not_show_cr_soons =
  no_arg_flag Switch.do_not_show_cr_soons
    ~doc:"don't show cr soons"
;;

let do_not_show_unclean_workspaces =
  no_arg_flag Switch.do_not_show_unclean_workspaces
    ~doc:"don't show unclean workspaces"
;;

let may_repartition_crs =
  map ~f:not
    (no_arg_flag "-do-not-repartition-crs"
       ~doc:"do not repartition CRs after the change")
;;

let update_local_repo =
  no_arg_flag Switch.update_local_repo
    ~doc:"update the local repo to the feature's tip"
;;

let without_enough_whole_feature_reviewers =
  no_arg_flag Switch.without_enough_whole_feature_reviewers
    ~doc:"proceed with fewer than two whole-feature-reviewers"
;;

let lock_reason =
  flag "-reason" ~doc:"REASON reason for locking" (required string)
;;

let lock_names =
  enum_no_args (module Lock_name)
    ~doc:(fun ~name _ -> name)
;;

let metric_values =
  flag "-values" ~doc:"FLOAT[,FLOAT..] value"
    (required
       (Arg_type.comma_separated float
          ~strip_whitespace:true
          ~unique_values:false))

;;

let metric_name_arg_type =
  Arg_type.create Metric_name.of_string ~complete:(complete [Metric_name])
;;

let metric_switch = "-metric"

let metric_name =
  flag metric_switch ~doc:"METRIC metric" (required metric_name_arg_type)
;;

let metric_name_option =
  flag metric_switch ~doc:"METRIC metric" (optional metric_name_arg_type)
;;

let depth_option =
  map (flag "-depth" (optional string)
         ~doc:"{DEPTH|max} show descendants DEPTH levels down (default is 1)")
    ~f:(Option.map ~f:(function
      | "max" -> Int.max_value
      | n     -> Int.of_string n))
;;

let metric_name_anchored_regex_list_arg_type =
  comma_delim_list_arg_type
    (fun patt -> Regex.create ("^" ^ patt ^ "$")) (complete [Metric_name])
;;

let metric_name_regex_list_option ~doc =
  flag "-metrics" ~doc
    (optional metric_name_anchored_regex_list_arg_type)
  |> map ~f:(Option.map ~f:Or_error.all)
;;

let for_or_all_or_all_but_default_me =
  let open Command.Let_syntax in
  let%map_open for_or_all =
    flag Switch.for_ (optional user_name_or_all)
      ~doc:(sprintf "%s (default is unix login)" User_name.Or_all.arg_doc)
  and for_all_but =
    flag Switch.for_all_but (optional user_list_arg_type)
      ~doc:"USER[,USER...] for all but these users"
  in
  match for_or_all, for_all_but with
  | None  , None   -> Ok (`User User_name.unix_login)
  | Some x, None   -> Ok (x :> User_name.Or_all_or_all_but.t)
  | None  , Some x -> Ok (`All_users_but (User_name.Set.of_list x))
  | Some _, Some _ ->
    Or_error.errorf "The flags [%s] and [%s] are mutually exclusive."
      Switch.for_
      Switch.for_all_but
;;
end

include T

module Let_syntax = struct
  module Let_syntax = struct
    include T
    module Open_on_rhs = T
  end
end
