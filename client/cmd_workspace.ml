open Core.Std
open Async.Std
open Import

let workspaces_are_enabled () = Client_config.(get () |> Workspaces.are_enabled)

module If_enabled = struct

  let if_enabled f =
    if not (workspaces_are_enabled ())
    then Deferred.unit
    else f ()
  ;;

  let create_workspace feature =
    if_enabled (fun () ->
      let%map (_ : Feature_share.t) = Feature_share.force feature in
      ())
  ;;

  let delete_workspace f =
    if_enabled (fun () ->
      match%bind Feature_share.find f with
      | None -> Deferred.unit
      | Some s -> Feature_share.delete s)
  ;;

  let rename_workspaces renames =
    if_enabled (fun () ->
      (* do the moves in non-increasing order of feature-path length, so that we
         update children before their parents -- using increasing order would cause
         us to skip features. *)
      let renames =
        List.sort renames ~cmp:(fun (r1 : Rename.t) r2 ->
          Int.compare
            (Feature_path.num_parts r2.from)
            (Feature_path.num_parts r1.from))
      in
      Deferred.List.iter ~how:`Sequential renames
        ~f:(fun { Rename. feature_id; from; to_ } ->
          match%bind Feature_share.find from with
          | None -> Deferred.unit
          | Some src_share -> Feature_share.move_to src_share feature_id to_))
  ;;

  let update_satellite_repos ~center_repo_root =
    if_enabled (fun () ->
      match%bind Scaffold.load ~center_repo_root with
      | None -> Deferred.unit
      | Some scaffold ->
        Scaffold.update_satellite_repos scaffold ~center_repo_root)
  ;;
end

let features_in_my_todo_switch     = "-features-in-my-todo"
let features_assigned_to_me_switch = "-features-assigned-to-me"
let features_owned_by_me_switch    = "-features-owned-by-me"
let features_watched_by_me_switch  = "-features-watched-by-me"

let dir_command =
  Command.async' ~summary:"output a feature's workspace directory if it exists \
                           or fail otherwise"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and what_to_print =
       let%map feature_path_option = feature_path_option
       and basedir = no_arg_flag "-basedir" ~doc:" print the user's workspaces basedir"
       in
       Or_error.try_with (fun () ->
         let feature_path_option = ok_exn feature_path_option in
         if basedir
         then
           if Option.is_some feature_path_option
           then failwith "you cannot supply both a FEATURE and -basedir"
           else `Basedir
         else
           match feature_path_option with
           | None -> failwith "specify a feature or supply -basedir"
           | Some feature_path -> `Feature_dir feature_path
       )
     in
     fun () ->
       let open! Deferred.Let_syntax in
       match ok_exn what_to_print with
       | `Basedir ->
         print_endline (Repo_clone.workspaces_basedir () |> Abspath.to_string);
         Deferred.unit
       | `Feature_dir feature_path ->
         match%map Feature_share.find feature_path with
         | Some share ->
           print_endline (Repo_root.to_string (Feature_share.center_repo_root share))
         | None ->
           raise_s
             [%sexp "you don't have a workspace for", (feature_path : Feature_path.t)]
    )
;;

let repo_for_hg_operations_exn feature_path ~use =
  if not (workspaces_are_enabled ())
  then begin
    let repo_root = ok_exn Repo_root.program_started_in in
    let check_repo_family_exn =
      let%bind repo_root_feature =
        Workspace_hgrc.extract_root_feature_from_hgrc repo_root
      in
      let feature_root = Feature_path.root feature_path in
      match repo_root_feature with
      | Error e1 ->
        (* that way we can still test the functionality by creating a valid .hgrc but it
           will still be ok most of the tests where we do not have such as .hgrc *)
        begin
          if am_functional_testing
          then return ()
          else
            match%bind
              Deferred.Or_error.try_with ~extract_exn:true (fun () ->
                Workspace_util.find_remote_repo_path_exn feature_path)
            with
            | Ok remote_repo_path ->
              Hg.ensure_local_repo_is_in_family repo_root remote_repo_path
            | Error e2 ->
              raise_s
                [%sexp
                  (sprintf !"\
your working directory must be inside a clone of the [%{Feature_name}] family,
and fe cannot determine the repo family it's in" feature_root : string)
                , (e1 : Error.t)
                , (e2 : Error.t)
                ]
        end
      | Ok repo_root_feature ->
        if Feature_name.equal repo_root_feature feature_root
        then return ()
        else
          failwithf
            !"\
your working directory must be inside a clone of the [%{Feature_name}] family,
but is inside a clone of the [%{Feature_name}] family"
            feature_root repo_root_feature ()
    in
    let%map () = check_repo_family_exn in
    repo_root
  end else
    let use_clone () =
      let%map repo_clone =
        Repo_clone.force ~root_feature:(Feature_path.root feature_path)
      in
      Repo_clone.repo_root repo_clone
    in
    let default_workspace_logic () =
      match use with
      | `Clone -> use_clone ()
      | `Share | `Share_or_clone_if_share_does_not_exist as use ->
        match%bind Feature_exists.rpc_to_server_exn feature_path with
        | No -> use_clone ()
        | Yes feature_id ->
          match use with
          | `Share ->
            let%map share = Feature_share.force { feature_id; feature_path } in
            Feature_share.center_repo_root share
          | `Share_or_clone_if_share_does_not_exist ->
            match%bind Feature_share.find ~feature_id feature_path with
            | None -> use_clone ()
            | Some feature_share ->
              return (Feature_share.center_repo_root feature_share)
    in
    match Repo_root.program_started_in with
    | Error _ -> default_workspace_logic ()
    | Ok repo_root ->
      let%bind repo_root_is_of_correct_family =
        match%map Workspace_hgrc.extract_root_feature_from_hgrc repo_root with
        | Error _ -> false
        | Ok repo_root_family ->
          Feature_name.equal repo_root_family (Feature_path.root feature_path)
      in
      match Feature_share.extract_feature_from_workspace_share_path repo_root with
      | None ->
        if repo_root_is_of_correct_family
        then return repo_root
        else default_workspace_logic ()
      | Some current_workspace ->
        if repo_root_is_of_correct_family
           && Feature_name.(<>)
                (Feature_path.root current_workspace)
                (Feature_path.root feature_path)
        then
          (* We are in a satellite and the user wants to use the satellite to work on
             some satellite feature.  Let it be. *)
          return repo_root
        else if Feature_path.(=) current_workspace feature_path
                || begin match use with
                  | `Share -> false
                  | `Clone | `Share_or_clone_if_share_does_not_exist -> true
                end
        then default_workspace_logic ()
        else
          raise_s
            [%sexp
              "disallowed due to current workspace being different from supplied feature",
              { current_workspace = (current_workspace : Feature_path.t)
              ; supplied_feature  = (feature_path      : Feature_path.t)
              }
            ]
;;

let repo_for_hg_operations_command =
  Command.async'
    ~summary:"output the workspace that will be used by fe commands"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and use_clone_instead_of_share =
       map (no_arg_flag "-use-clone-instead-of-share"
              ~doc:" use clone of feature root instead of the feature share")
         ~f:(fun bool -> if bool then Some () else None)
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%map repo_root =
         repo_for_hg_operations_exn feature_path
           ~use:(if is_some use_clone_instead_of_share then `Clone else `Share)
       in
       print_endline (Repo_root.to_string repo_root)
    )
;;

let repo_for_hg_operations_use_clone_exn features =
  let default () = return (ok_exn Repo_root.program_started_in) in
  if not (workspaces_are_enabled ())
  then default ()
  else
    match List.hd features with
    | None -> default ()
    | Some feature ->
      if List.exists features ~f:(fun feature' ->
        Feature_name.(<>) (Feature_path.root feature) (Feature_path.root feature'))
      then default ()
      else
        let%map repo_clone = Repo_clone.force ~root_feature:(Feature_path.root feature) in
        Repo_clone.repo_root repo_clone
;;

module Select_features_in_my_todo = struct
  type t =
    | Assigned
    | Owned
    | Watched
  [@@deriving enumerate, sexp_of]


  let feature_paths
        { Todo.Reaction.
          assigned
        ; unclean_workspaces        = _
        ; owned
        ; watched
        ; cr_soons                  = _
        ; bookmarks_without_feature = _
        } t =
    Feature_path.Set.of_list
      (match t with
       | Assigned -> List.map assigned ~f:Todo.Assigned.feature_path
       | Owned    -> List.map owned    ~f:Todo.Feature_info.feature_path
       | Watched  -> List.map watched  ~f:Todo.Feature_info.feature_path)
  ;;

  let select
        ~features_in_my_todo
        ~features_assigned_to_me
        ~features_owned_by_me
        ~features_watched_by_me
    =
    if features_in_my_todo
    then
      if features_assigned_to_me || features_owned_by_me || features_watched_by_me
      then failwithf "the switch %s implies ( %s && %s && %s )"
             features_in_my_todo_switch
             features_assigned_to_me_switch
             features_owned_by_me_switch
             features_watched_by_me_switch
             ()
      else all
    else
      List.filter_map ~f:(fun (cons, bool) -> Option.some_if bool cons)
        [ Assigned, features_assigned_to_me
        ; Owned   , features_owned_by_me
        ; Watched , features_watched_by_me
        ]
  ;;
end

let get_features_in_my_todo ~for_ ~which_features_in_my_todo =
  match (which_features_in_my_todo : Select_features_in_my_todo.t list) with
  | [] -> return Feature_path.Set.empty
  | (_::_ as select) ->
    let%map todo =
      Todo.rpc_to_server_exn
        { for_
        ; include_active_cr_soons    = false
        ; include_all_owned_features = true
        }
    in
    List.map select ~f:(Select_features_in_my_todo.feature_paths todo)
    |> Feature_path.Set.union_list
;;

let run_concurrent_actions_exn ~get_feature_path ~action ~max_concurrent_jobs list ~f =
  let%map failed_features =
    Deferred.List.map ~how:(`Max_concurrent_jobs max_concurrent_jobs) list ~f:(fun elt ->
      match%map Monitor.try_with ~extract_exn:true (fun () -> f elt) with
      | Ok () -> None
      | Error exn ->
        let feature_path = get_feature_path elt in
        prerr_endline ((feature_path, exn)
                       |> [%sexp_of: Feature_path.t * Exn.t]
                       |> Sexp.to_string_hum);
        Some feature_path)
  in
  let failed_features = List.filter_opt failed_features in
  if not (List.is_empty failed_features)
  then
    raise_s
      [%sexp
        (sprintf "failed to %s workspace%s" action
           (if List.length failed_features > 1 then "s" else "") : string)
      , (failed_features : Feature_path.t list)
      ]
;;

let create_command =
  Command.async'
    ~summary:"force the creation of user's workspaces (idempotent)"
    ~readme:(fun () ->
      concat [ "\
Create specified workspace(s).  If ["; features_in_my_todo_switch; "] is supplied,
then a workspace is set up for each features you own or have to read, and a
hg pull is executed in all root features clones.

In addition to manual uses of the command, it is encouraged to run it often
in the background (e.g. via a crontab job) to help reducing the latency of
running other workspace commands."
             ]
    )
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and () = interactive
     and max_concurrent_jobs =
       flag "max-concurrent-jobs" (optional_with_default 5 int) ~doc:"NUM (default 5)"
     and desired_num_spares =
       flag "num-spares" (optional int)
         ~doc:"INT target number of spare shares"
     and for_ = for_
     and clone_of_root_feature_of =
       clone_of_root_feature_of
         ~doc:"FEATURE clone the root feature repos of specified feature"
     and which_features_in_my_todo =
       let%map features_in_my_todo =
         no_arg_flag features_in_my_todo_switch
           ~doc:"create a workspace for each feature assigned or owned \
                 in the user's todo"
       and features_assigned_to_me =
         no_arg_flag features_assigned_to_me_switch
           ~doc:"create a workspace for each feature assigned in the user's todo"
       and features_owned_by_me =
         no_arg_flag features_owned_by_me_switch
           ~doc:"create a workspace for each feature owned in the user's todo"
       and features_watched_by_me =
         no_arg_flag features_watched_by_me_switch
           ~doc:"create a workspace for each feature watched in the user's todo"
       in
       Select_features_in_my_todo.select
         ~features_in_my_todo
         ~features_assigned_to_me
         ~features_owned_by_me
         ~features_watched_by_me
     and feature_path_option = feature_path_option
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind () =
         if List.is_empty which_features_in_my_todo
         then Deferred.unit
         else
           (* pull in all clones and replenish spare shares *)
           let%bind clones = Repo_clone.list () in
           Deferred.List.iter clones ~how:(`Max_concurrent_jobs max_concurrent_jobs)
             ~f:(fun clone ->
               let%bind () = Repo_clone.pull_all_revs clone in
               match desired_num_spares with
               | None -> Deferred.unit
               | Some desired_num_spares ->
                 Repo_clone.refresh_spare_shares clone
                   ~desired_num_spares ~update_to:(Repo_clone.tip_on_server clone))
       in
       let%bind features =
         get_features_in_my_todo ~for_ ~which_features_in_my_todo
       in
       let features =
         (match ok_exn feature_path_option with
          | None -> features
          | Some feature -> Set.add features feature
         )
         |> Set.to_list
         |> List.map ~f:(fun feature -> `Feature feature)
       in
       let clones =
         clone_of_root_feature_of
         |> ok_exn
         |> Set.to_list
         |> List.map ~f:(fun clone -> `Clone clone)
       in
       let get_feature_path = function
         | `Feature feature -> feature
         | `Clone   clone   -> Feature_path.of_root clone
       in
       run_concurrent_actions_exn ~get_feature_path ~action:"create"
         ~max_concurrent_jobs (clones @ features) ~f:(function
           | `Clone root_feature ->
             let%bind existing_root_features =
               Workspace_util.root_features_exn_with_memo ()
             in
             if not (Map.mem existing_root_features root_feature)
             then failwith "No such root feature";
             let%map (_ : Repo_clone.t) = Repo_clone.force ~root_feature in
             ()

           | `Feature feature_path ->
             match%bind Feature_share.find feature_path with
             | Some _ -> return ()
             | None ->
               match%bind Feature_exists.rpc_to_server_exn feature_path with
               | Yes feature_id ->
                 let%map (_ : Feature_share.t) =
                   Feature_share.force { feature_id; feature_path }
                 in
                 ()

               | No ->
                 (* The feature may not exist, because assigned features includes
                    features with catch up.  In that case the user will need at least the
                    clone of the family, if it still exists. *)
                 if Feature_path.is_root feature_path
                 then return () (* we already know that feature_path does not exist *)
                 else
                   let%map (_ : Repo_clone.t Or_error.t) =
                     Monitor.try_with_or_error (fun () ->
                       Repo_clone.force ~root_feature:(Feature_path.root feature_path))
                   in
                   ())
    )
;;

let list_command =
  Command.async' ~summary:"list all workspaces"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map feature_shares = Feature_share.list () in
       List.iter feature_shares ~f:(fun share ->
         print_endline (Feature_path.to_string (Feature_share.feature_path share)))
    )
;;

let check_shares shares ~f =
  let errors = ref [] in
  let%map () =
    Deferred.List.iter ~how:(`Max_concurrent_jobs 10) shares ~f:(fun share ->
    match%map Deferred.Or_error.try_with_join (fun () -> f share) with
    | Ok () -> ()
    | Error err -> errors := (Feature_share.feature_path share, err) :: !errors)
  in
  List.sort !errors ~cmp:(fun (f1, _) (f2, _) -> Feature_path.compare f1 f2)
;;

let check_workspaces which_workspaces ~f =
  match which_workspaces with
  | `All ->
    let%bind shares = Feature_share.list () in
    check_shares shares ~f
  | `Feature_path feature_path ->
    match%bind Feature_share.find feature_path with
    | Some share -> check_shares [ share ] ~f
    | None ->
      return [ feature_path
             , Error.createf "You don't have a workspace for [%s]"
                 (Feature_path.to_string feature_path)
             ]
;;

let check_workspaces_invariant_command =
  Command.async' ~summary:"check invariant on local workspaces"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_opt = feature_path_option
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map errors =
         check_workspaces ~f:Feature_share.check_workspace_invariant
           (match ok_exn feature_opt with
            | None -> `All
            | Some feature -> `Feature_path feature)
       in
       if not (List.is_empty errors)
       then
         raise_s
           [%sexp "invalid workspaces"
                , (errors : (Feature_path.t * Error.t) list)
           ]
    )
;;

let confirm_or_dry_run shares ~dry_run ~action ~process_share =
  let no_shares = List.is_empty shares in
  let%bind () =
    if not (dry_run || !Interactive.interactive)
    then return ()
    else
      let printf f =
        ksprintf (fun s ->
          if dry_run
          then (print_string s; return ())
          else Interactive.printf "%s" s)
          f
      in
      if no_shares
      then printf "No workspace selected\n"
      else
        let is_plural = List.length shares > 1 in
        printf "Proceeding will %s %s:\n%s"
          action
          (if is_plural then "these workspaces" else "this workspace")
          (concat
             (List.concat_map shares ~f:(fun share ->
                [ "  "
                ; Feature_path.to_string (Feature_share.feature_path share)
                ; "\n"
                ])))
  in
  let%bind should_proceed =
    if no_shares
    then return false (* do not bother asking yn for an empty selection *)
    else
    if dry_run || not !Interactive.interactive
    then return (not dry_run)
    else begin
      let%bind should_proceed = Interactive.ask_yn "Proceed? " ~default:false in
      let%map () =
        if not should_proceed
        then Interactive.print_endline "Aborted.\n"
        else Deferred.unit
      in
      should_proceed
    end
  in
  if not should_proceed
  then Deferred.unit
  else
    run_concurrent_actions_exn shares ~f:process_share
      ~get_feature_path:Feature_share.feature_path ~action
      ~max_concurrent_jobs:10
;;

let dry_run_switch = "-dry-run"
let dry_run ~done_ =
  let open Command.Param in
  no_arg_flag dry_run_switch
    ~doc:(sprintf " only print what features would have otherwise been %s" done_)
;;
let exclude_switch = "-exclude"
let exclude =
  let open Command.Param in
  feature_path_flagged_listed ~label:exclude_switch
    ~doc:"FEATURE leave this feature alone \
          (this flag may be passed serveral times)"
;;

let exclude_features_in_my_todo_param =
  let open Command.Let_syntax in
  let%map_open () = return ()
  and features_in_my_todo =
    no_arg_flag (concat [ "-exclude" ; features_in_my_todo_switch ])
      ~doc:(sprintf "exclude features assigned or owned in the user's todo")
  and features_assigned_to_me =
    no_arg_flag (concat [ "-exclude" ; features_assigned_to_me_switch ])
      ~doc:"exclude features assigned in the user's todo"
  and features_owned_by_me =
    no_arg_flag (concat [ "-exclude" ; features_owned_by_me_switch ])
      ~doc:"exclude features owned in the user's todo"
  and features_watched_by_me =
    no_arg_flag (concat [ "-exclude" ; features_watched_by_me_switch ])
      ~doc:"exclude features watched in the user's todo"
  in
  Select_features_in_my_todo.select
    ~features_in_my_todo
    ~features_assigned_to_me
    ~features_owned_by_me
    ~features_watched_by_me
;;

let select_shares ~which_features ~exclusions ~for_ ~exclude_features_in_my_todo =
  let%bind which_features = force which_features in
  let exclusions = ok_exn exclusions in
  let%bind exclude_features_in_my_todo =
    get_features_in_my_todo ~for_
      ~which_features_in_my_todo:exclude_features_in_my_todo
  in
  let exclusions =
    Feature_path.Set.union_list
      [ Client_config.(get () |> Workspaces.do_not_distclean)
      ; Feature_path.Set.of_list exclusions
      ; exclude_features_in_my_todo
      ]
  in
  let%map shares = Feature_share.list () in
  List.filter shares ~f:(fun share ->
    let share_path = Feature_share.feature_path share in
    not (Set.mem exclusions share_path)
    && Which_features.mem which_features share_path)
;;

let distclean_command =

  Command.async'
    ~summary:"delete build artifacts in selected workspaces"
    ~readme:(fun () -> concat [ "\
Iter through the intersection of your local workspaces and the feature selected via the
command line and in each repo root run:

  $ hg distclean

The intended usage of this command is to be run in the background to keep the local disk
use reasonable, or to be run manually from time to time on selected workspaces.

To prevent some workspaces from being cleaned, add a [do_not_distclean] stanza to
your .ferc, e.g:

  (workspaces (
    (do_not_distclean (
      jane/fe
      jane/oculus
    ))
  ))

Like [" ; exclude_switch ; "], this will take priority over the features selected via the
command line, and result in skipping those features.

In doubt, run the command with [" ; dry_run_switch ; "] to see what would be done.
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and () = interactive
     and which_features =
       which_features ~allow_empty_selection:false ~default_to_current_bookmark:false ()
     and exclusions =
       exclude
     and dry_run =
       dry_run ~done_:"distcleaned"
     and for_ = for_
     and exclude_features_in_my_todo = exclude_features_in_my_todo_param
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind shares =
         select_shares ~which_features ~exclusions ~for_ ~exclude_features_in_my_todo
       in
       confirm_or_dry_run ~dry_run ~action:"distclean" shares
         ~process_share:Feature_share.distclean
    )
;;

let delete_command =
  let workspaces_without_feature_switch = "-workspaces-without-feature" in
  Command.async' ~summary:"delete selected workspaces"
    ~readme:(fun () -> concat [ "\
This command deletes one's selected workspace directories on the local machine.  It does
not archive the associated feature(s), and leave the remote repository untouched.

Before deleting the directories, the command will check the cleanliness of the workspace.
Refer to [fe workspace unclean check -help] to read more about the cleanliness status.
Essentially, the command will not attempt to delete a workspace that has uncommitted or
unpushed changes and fail instead, in order to preserve those changes.
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and () = interactive
     and which_features =
       which_features ~allow_empty_selection:true ~default_to_current_bookmark:false ()
     and exclude = exclude
     and dry_run = dry_run ~done_:"deleted"
     and workspaces_without_feature =
       flag workspaces_without_feature_switch no_arg
         ~doc:" delete all workspaces whose feature doesn't exist"
     and for_ = for_
     and exclude_features_in_my_todo = exclude_features_in_my_todo_param
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind which_features = force which_features in
       begin match which_features with
       | Features [] when not workspaces_without_feature ->
         failwithf "Specify some features, or use [%s]."
           workspaces_without_feature_switch ()
       | _ -> ()
       end;
       let%bind exclude_features_in_my_todo =
         get_features_in_my_todo ~for_
           ~which_features_in_my_todo:exclude_features_in_my_todo
       in
       let excluded =
         Feature_path.Set.union_list
           [ Feature_path.Set.of_list (ok_exn exclude)
           ; exclude_features_in_my_todo
           ]
       in
       let%bind shares =
         let%bind shares = Feature_share.list () in
         let%map existing_features =
           List_feature_names.rpc_to_server_exn
             { descendants_of = Any_root
             ; depth          = Int.max_value
             ; use_archived   = false
             }
         in
         let existing_features =
           Feature_path.Set.of_list existing_features
         in
         List.filter shares ~f:(fun share ->
           let share_path = Feature_share.feature_path share in
           not (Set.mem excluded share_path)
           && begin
             Which_features.mem which_features share_path
             || (workspaces_without_feature
                 && not (Set.mem existing_features share_path))
           end)
       in
       confirm_or_dry_run shares ~dry_run ~action:"delete"
         ~process_share:Feature_share.delete
    )
;;

let exists_command =
  Command.async'
    ~summary:"test the existence of user workspaces"
    ~readme:(fun () -> concat [ "\
Check on the localhost that the user has a workspace for a specified feature or clone.
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and () = interactive
     and clone_or_feature =
       let%map feature =
         anon (maybe ("FEATURE" %: unverified_workspace_arg_type))
       and clone_of_root_feature_of =
         flag Switch.clone_of_root_feature_of (optional unverified_workspace_arg_type)
           ~doc:"FEATURE check for the clone associated with the specified \
                 feature's root instead of checking for the feature share"
       in
       Or_error.try_with (fun () ->
         match feature, clone_of_root_feature_of with
         | None, None -> failwith "one should supply at least one feature or repo clone"
         | Some feature, None -> `Feature feature
         | None, Some feature -> `Clone_of_root_feature_of feature
         | Some _, Some _ -> failwith "one should supply either a feature or a \
                                       repo clone but not both"
       )
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map exists =
         match clone_or_feature |> ok_exn with
         | `Clone_of_root_feature_of feature ->
           let root_feature = feature |> ok_exn |> Feature_path.root in
           begin match%map Repo_clone.find ~root_feature with
           | Some (_ : Repo_clone.t) -> true
           | None -> false
           end

         | `Feature feature ->
           begin match%map Feature_share.find (ok_exn feature) with
           | Some (_ : Feature_share.t) -> true
           | None -> false
           end
       in
       printf "%b\n" exists
    )
;;

let kill_build_command =
  Command.async'
    ~summary:"kill running build process in selected workspaces"
    ~readme:(fun () -> concat [ "\
Attempt to kill the build process running in the selected workspace(s).

The command tries first to send the kill request to the build manager running in emacs (if
applicable) and then defaults to killing the jenga process directly.  The intent is for
this command to work for non emacs users, while offering a cleaner kill for emacs users.
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and () = interactive
     and which_features =
       which_features ~allow_empty_selection:false ~default_to_current_bookmark:false ()
     and exclusions =
       exclude
     and dry_run =
       dry_run ~done_:"distcleaned"
     and for_ = for_
     and exclude_features_in_my_todo = exclude_features_in_my_todo_param
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind shares =
         select_shares ~which_features ~exclusions ~for_ ~exclude_features_in_my_todo
       in
       confirm_or_dry_run shares ~dry_run ~action:"kill-build"
         ~process_share:Feature_share.kill_build
    )
;;

let workspace_commands =
  [ "create"           , create_command
  ; "delete"           , delete_command
  ; "dir"              , dir_command
  ; "distclean"        , distclean_command
  ; "exists"           , exists_command
  ; "kill-build"       , kill_build_command
  ; "list"             , list_command
  ; "unclean"          , Cmd_workspace_unclean.command
  ]
;;
