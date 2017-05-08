open! Core
open! Async
open! Import

let default_description feature_path =
  let default () =
    sprintf !"WRITE ME (replace with a description of %{Feature_path})"
      feature_path
  in
  if am_functional_testing
  then
    (* do not make the tests depends on a prod etc file that is likely to change *)
    return (default ())
  else (
    let template_filename =
      Abspath.extend Iron_config.prod_etc (File_name.of_string "description.txt")
      |> Abspath.to_string
    in
    let%map result =
      Monitor.try_with (fun () -> Reader.file_contents template_filename)
    in
    match result with
    | Error _ -> default ()
    | Ok from_template ->
      let buffer = Buffer.create 256 in
      (* Offer some variable if needed, using syntax ${VAR} in the txt *)
      let subst = function
        | "FEATURE_PATH" -> Feature_path.to_string feature_path
        | s -> s
      in
      Buffer.add_substitute buffer subst from_template;
      Buffer.contents buffer)
;;

let main { Fe.Create.Action.
           feature_path
         ; base
         ; tip
         ; description
         ; owners
         ; is_permanent
         ; remote_repo_path
         ; no_bookmark
         ; add_whole_feature_reviewers
         ; reviewing
         ; allow_non_cr_clean_base
         ; properties
         } =
  (match Feature_path.is_root feature_path, remote_repo_path with
   | true , None ->
     failwithf "must supply %s when creating a root feature"
       Switch.remote_repo_path ()
   | false, Some _ ->
     failwithf "cannot supply %s when creating a non-root feature"
       Switch.remote_repo_path ()
   | true , Some _
   | false, None   -> ());
  let if_bookmark f = if not no_bookmark then f () else Deferred.unit in
  let%bind repo_root =
    if not (Feature_path.is_root feature_path)
    then Cmd_workspace.repo_for_hg_operations_exn feature_path ~use:`Clone
    else (
      (* When we're creating a root feature, it doesn't exist, and so the workspace
         logic can't be used yet. *)
      match Repo_root.program_started_in with
      | Ok repo_root -> return repo_root
      | Error error ->
        raise_s [%sexp "must be in the repo for the root feature", (error : Error.t)])
  in
  let%bind repo_is_clean =
    if no_bookmark
    then return None
    else (
      let%map status_cleanliness = Hg.status_cleanliness repo_root in
      status_cleanliness
      |> ok_exn
      |> Option.some)
  in
  let%bind rev_zero = Hg.create_rev_zero repo_root in
  let%bind () =
    if am_functional_testing
    then
      (* The heuristic used to allow the local repo to be used is to extract
         successfully the repo family from the [.hg/hgrc] file.  In test, we clone
         repository locally with no hg repo root, causing this heuristic to fail.
         This code here is just to help the heuristic to apply.  The variable
         [IRON_FUNCTIONAL_TESTING_FORCE_WORKSPACES] is honored during functional
         testing only, and in addition to IRON_OPTIONS -- because some of the tests
         set [IRON_OPTIONS], using a independent variable allow the same tests to be
         run with or without the workspaces forced. *)
      if Feature_path.is_root feature_path
      && Option.is_some (Sys.getenv "IRON_FUNCTIONAL_TESTING_FORCE_WORKSPACES")
      then (
        let fake_remote_repo_path =
          Remote_repo_path.of_string
            (sprintf "ssh://hg//hg/%s/fake-submissions"
               (Feature_name.to_string (Feature_path.root feature_path)))
        in
        Workspace_hgrc.save { repo_root
                            ; remote_repo_path = fake_remote_repo_path
                            ; kind             = `Fake_for_testing
                            })
      else return ()
    else (
      match remote_repo_path with
      | None -> ensure_can_access_remote_repo ~for_root_of:feature_path
      | Some remote_repo_path ->
        (match Remote_repo_path.family remote_repo_path with
         | None ->
           raise_s
             [%sexp
               "Cannot extract a feature family from that remote repo path"
             , (remote_repo_path : Remote_repo_path.t)
             ]
         | Some family ->
           if String.(<>) family (Feature_path.to_string feature_path)
           then
             raise_s
               [%sexp
                 "Inconsistency between feature and remote_repo_path family",
                 { remote_repo_path : Remote_repo_path.t
                 ; family           : string
                 ; feature_path     : Feature_path.t
                 }
               ]);
        Hg.ensure_local_repo_is_in_family repo_root remote_repo_path)
  in
  let%bind base = Raw_rev.resolve_opt_exn base ~in_:(Ok repo_root) in
  let%bind tip = Raw_rev.resolve_opt_exn tip  ~in_:(Ok repo_root) in
  let%bind () =
    if_bookmark (fun () ->
      let%map bm_exists = Hg.bookmark_exists repo_root (Feature feature_path) in
      if bm_exists
      then failwithf "repository already contains a bookmark named [%s]"
             (Feature_path.to_string feature_path) ())
  in
  let%bind (description_used, description) =
    match description with
    | Some description ->
      (* We turn [""] into [None] so that a user who tries to put in an empty
         description instead gets the default description.  We keep [`Provided_by_user]
         so that we don't open an editor which is probably what they wanted. *)
      let%map description =
        (if not (String.is_empty (String.strip description))
         then return description
         else default_description feature_path
        )
      in
      `Provided_by_user, description
    | None ->
      let%map description = default_description feature_path in
      `Default, description
  in
  let%bind { Create_feature.Reaction. feature_id; remote_repo_path ; tip } =
    Create_feature.rpc_to_server_exn
      { feature_path
      ; owners
      ; is_permanent
      ; description
      ; base
      ; tip
      ; add_whole_feature_reviewers =
          Option.value add_whole_feature_reviewers ~default:User_name.Set.empty
      ; reviewing
      ; rev_zero
      ; remote_repo_path
      ; allow_non_cr_clean_base
      ; properties
      }
  in
  let%bind () =
    if_bookmark (fun () ->
      let feature_was_created_in_local_repo =
        match Repo_root.program_started_in with
        | Error _ -> false
        | Ok started_in -> Repo_root.have_same_abspath started_in repo_root
      in
      if feature_was_created_in_local_repo
      then
        Hg.create_bookmark_and_update_to_it ?repo_is_clean
          repo_root remote_repo_path feature_path tip
      else (
        (* When a feature is created outside of the local repo, there is no point in
           updating to the bookmark -- e.g. in the clone with enabled workspaces. *)
        let%bind () =
          Hg.pull repo_root ?repo_is_clean ~from:remote_repo_path (`Rev tip)
        in
        Hg.set_bookmark repo_root (Feature feature_path) ~to_:(`Rev tip)
          (`Push_to_and_overwrite remote_repo_path)))
  in
  let%bind () =
    if_bookmark (fun () ->
      (* If there is no bookmark, creating the workspace is likely to fail.
         Otherwise, we first create the workspace and then invoke the editor so that
         once the editor is invoked, the workspace exists.  Doing it in the other
         order puts the description buffer in the user's face and would encourage one
         to start work on a feature before the workspace exists. *)
      Cmd_workspace.If_enabled.create_workspace { feature_id; feature_path })
  in
  match description_used with
  | `Provided_by_user -> Deferred.unit
  | `Default ->
    if not !Async_interactive.interactive
    then Deferred.unit
    else (
      let%bind description = Editor.invoke_editor description in
      Cmd_description.set_description_exn feature_path (ok_exn description))
;;

let command =
  Command.async'
    ~summary:"create a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and () = interactive
     and feature_path = absolute_feature_path
     and base = base
     and tip = tip
     and description = description
     and owners = owners
     and is_permanent = permanent
     and remote_repo_path = remote_repo_path
     and no_bookmark = no_bookmark
     and add_whole_feature_reviewers =
       users_option ~switch:Switch.add_whole_feature_reviewers
     and set_reviewing_first_owner_only =
       no_arg_flag Switch.set_reviewing_first_owner_only ~doc:""
     and set_reviewing_whole_feature_only =
       no_arg_flag Switch.set_reviewing_whole_feature_only ~doc:""
     and allow_non_cr_clean_base =
       no_arg_flag Switch.allow_non_cr_clean_base
         ~doc:"proceed even though the base of the new feature won't be CR clean"
     and properties =
       property_values_flag ~switch:"property" ~doc:"user-defined properties"
     in
     fun () ->
       let reviewing =
         if set_reviewing_whole_feature_only
         && set_reviewing_first_owner_only
         then
           failwithf "The switches [%s] and [%s] are mutually exclusive"
             Switch.set_reviewing_first_owner_only
             Switch.set_reviewing_whole_feature_only ()
         else
         if set_reviewing_whole_feature_only
         then `Whole_feature_reviewers
         else if set_reviewing_first_owner_only
         then `First_owner
         else
           match Client_config.(get () |> Cmd.Create.reviewing) with
           | Some value -> value
           | None ->
             if am_functional_testing
             then `First_owner
             else `Whole_feature_reviewers
       in
       main { feature_path = ok_exn feature_path
            ; base
            ; tip
            ; description
            ; owners
            ; is_permanent
            ; remote_repo_path
            ; no_bookmark
            ; add_whole_feature_reviewers
            ; reviewing
            ; allow_non_cr_clean_base
            ; properties
            })
;;
