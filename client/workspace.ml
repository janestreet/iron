open! Core
open! Async
open! Import

let verbose = Verbose.workspaces

let workspaces_basedir = Repo_clone.workspaces_basedir

let find_root_feature feature =
  let%map root_features = Workspace_util.root_features_exn_with_memo () in
  Map.find root_features (Feature_path.root feature)
;;

let maybe_regenerate_hgrc repo_root feature_id feature_path =
  let%bind status =
    match%map Workspace_hgrc.extract_info repo_root with
    | Error _ -> `Stale None
    | Ok { generated_by; kind; remote_repo_path = _ } ->
      match kind with
      | `Clone | `Satellite_repo | `Fake_for_testing -> `Stale None
      | `Feature { feature_path = feature_path' ; feature_id = feature_id' } ->
        if String.equal generated_by Version_util.version
        && Feature_path.equal feature_path feature_path'
        && (match feature_id with
          | None -> true
          | Some feature_id -> Feature_id.equal feature_id feature_id')
        then `Up_to_date feature_id'
        else `Stale (Some feature_id')
  in
  match status with
  | `Up_to_date feature_id -> return (Some feature_id)
  | `Stale feature_id' ->
    let feature_id = Option.first_some feature_id feature_id' in
    match%bind find_root_feature feature_path with
    | None -> return None
    | Some { remote_repo_path ; _ } ->
      let%bind feature_id =
        match feature_id with
        | (Some _) as some -> return some
        | None ->
          match%map Feature_exists.rpc_to_server_exn feature_path with
          | No -> None
          | Yes feature_id -> Some feature_id
      in
      let%map () =
        match feature_id with
        | None -> return ()
        | Some feature_id ->
          Workspace_hgrc.save
            { repo_root
            ; remote_repo_path
            ; kind = `Feature { feature_id; feature_path }
            }
      in
      feature_id
;;

module T : sig

  (* [t] is private to ensure that the repo's hgrc is regenerated if necessary upon
     creation *)
  type t = private
    { feature_id                        : Feature_id.t option
    ; feature_path                      : Feature_path.t
    ; enclosing_repo_root               : Repo_root.t
    ; center_relative_to_enclosing_repo : Relpath.t
    }
  [@@deriving compare, fields, sexp_of]

  val create
    :  feature_id:Feature_id.t option
    -> feature_path:Feature_path.t
    -> enclosing_repo_root:Repo_root.t
    -> center_relative_to_enclosing_repo:Relpath.t
    -> t Deferred.t

  val center_repo_root : t -> Repo_root.t

end = struct

  type t =
    { feature_id                        : Feature_id.t option
    ; feature_path                      : Feature_path.t
    ; enclosing_repo_root               : Repo_root.t
    ; center_relative_to_enclosing_repo : Relpath.t
    }
  [@@deriving fields, sexp_of]

  let compare t1 t2 = Feature_path.compare t1.feature_path t2.feature_path

  let center_repo_root_internal
        ~feature_path
        ~enclosing_repo_root
        ~center_relative_to_enclosing_repo
    =
    Repo_root.append enclosing_repo_root
      (Path_in_repo.of_relpath center_relative_to_enclosing_repo)
    |> Repo_root.of_abspath
         ~human_readable:(concat [ Feature_path.to_string feature_path
                                 ; "'s workspace"
                                 ])
  ;;

  let center_repo_root { feature_id = _
                       ; feature_path
                       ; enclosing_repo_root
                       ; center_relative_to_enclosing_repo
                       } =
    center_repo_root_internal
      ~feature_path
      ~enclosing_repo_root
      ~center_relative_to_enclosing_repo
  ;;

  let create ~feature_id ~feature_path
        ~enclosing_repo_root ~center_relative_to_enclosing_repo =
    let repo_root =
      center_repo_root_internal ~feature_path
        ~enclosing_repo_root
        ~center_relative_to_enclosing_repo
    in
    let%map feature_id = maybe_regenerate_hgrc repo_root feature_id feature_path in
    { feature_id
    ; feature_path
    ; enclosing_repo_root
    ; center_relative_to_enclosing_repo
    }
  ;;
end

include T

let share = File_name.of_string "+share+"

let enclosing_repo_root_abspath feature_path =
  Abspath.append
    (workspaces_basedir ())
    (Relpath.extend
       (Feature_path.to_relpath feature_path)
       share)
;;

let extract_feature_from_workspace_share_path_aux ~basedir repo_root =
  match Abspath.chop_prefix ~prefix:basedir (Repo_root.to_abspath repo_root) with
  | Error _ -> None
  | Ok relpath ->
    let parts = Relpath.parts relpath in
    if not (List.exists parts ~f:(fun name -> File_name.equal share name))
    then None
    else
      Option.try_with (fun () ->
        List.take_while parts ~f:(fun name ->
          not (File_name.equal share name))
        |> List.map ~f:(fun name -> Feature_name.of_string (File_name.to_string name))
        |> Feature_path.of_parts_exn)
;;

let%test_unit _ =
  List.iter ~f:(fun (path, expected) ->
    [%test_result: Feature_path.t option]
      ~expect:(Option.map ~f:Feature_path.of_string expected)
      (extract_feature_from_workspace_share_path_aux
         ~basedir:(Abspath.of_string "/basedir")
         (Repo_root.of_abspath (Abspath.of_string path))))
    [ "/basedir/blah/fe/+share+"        , Some "blah/fe"
    ; "/basedir/foo/+share+"            , Some "foo"
    ; "/basedir/blah/+clone+"           , None
    ; "/basedir/foo/bar/+share+/foo"    , Some "foo/bar"
    ]
;;

let extract_feature_from_workspace_share_path repo_root =
  extract_feature_from_workspace_share_path_aux ~basedir:(workspaces_basedir ()) repo_root
;;

module Clone_from = struct
  type t =
    | Local_clone of Feature_name.t
    | Remote      of Remote_repo_path.t
end

(* [share_or_clone_and_update] creates a share or clone in [dst_repo_root_abspath], with
   working directory updated to [revision].  It creates a share if at all possible,
   i.e. if the repo is in some feature family (in which case it creates the workspace
   clone if necessary). *)
let share_or_clone_and_update
      ~dst_repo_root_abspath
      ~dst_repo_root_human_readable_name
      ~revision
      ~kind
      (clone_from : Clone_from.t) =
  let from_local_clone root_feature =
    let%bind clone = Repo_clone.force ~root_feature in
    let%map dst_repo_root =
      Repo_clone.create_share_exn clone ~dst_repo_root_abspath
    in
    dst_repo_root, Repo_clone.remote_repo_path clone
  in
  let%bind (dst_repo_root, remote_repo_path) =
    match clone_from with
    | Local_clone root_feature -> from_local_clone root_feature
    | Remote remote_repo_path ->
      (* Scan fe to find out whence to clone it from on the fly *)
      let%bind root_features = Workspace_util.root_features_exn_with_memo () in
      match
        List.find_map (Map.data root_features)
          ~f:(fun { List_root_features.Reaction.
                    root_feature
                  ; remote_repo_path = remote_repo_path'
                  ; _ } ->
               if Remote_repo_path.equal remote_repo_path' remote_repo_path
               then Some root_feature
               else None)
      with
      | Some root_feature -> from_local_clone root_feature
      | None ->
        if true
        then (
          (* couldn't find it in fe, just clone from the remote path *)
          let%map dst_repo_root =
            Hg.clone remote_repo_path
              ~dst_repo_root_abspath__delete_if_exists:dst_repo_root_abspath
          in
          ok_exn dst_repo_root, remote_repo_path)
        else
          raise_s
            [%sexp
              "Fail to create workspace.  Iron does not know about this remote_repo_path",
              { remote_repo_path = (remote_repo_path : Remote_repo_path.t)
              ; known_by_iron = (Map.data root_features : List_root_features.Reaction.t)
              }
            ]
  in
  let dst_repo_root =
    Repo_root.with_human_readable dst_repo_root dst_repo_root_human_readable_name
  in
  let%bind revision =
    match revision with
    | `Feature _ as updatable_and_pullable -> return updatable_and_pullable
    | `Unresolved_from_scaffold string ->
      let%map rev =
        Hg.Scaffold.resolve_revision remote_repo_path ~revision:string
          ~scaffold_requires_global_tag_or_rev_hash:false
      in
      `Revset (Revset.of_string (ok_exn rev))
  in
  let revision =
    (* Workaround type error below.  We could name the types taken by Iron's pull & update
       and use :> *)
    match revision with
    | (`Rev _ | `Feature _ | `Revset _) as updatable_and_pullable ->
      updatable_and_pullable
  in
  (* We might need to pull in case we've used an existing spare share as opposed to a
     fresh share.  In both cases, we supply [~even_if_unclean:true] to suppress the
     cleanliness check, which is unnecessary because we're using a spare share. *)
  let%bind () =
    Hg.pull dst_repo_root ~even_if_unclean:true ~from:remote_repo_path revision
  in
  let%bind () = Hg.update dst_repo_root revision ~clean_after_update:No in
  let%map () =
    Workspace_hgrc.save
      { repo_root        = dst_repo_root
      ; remote_repo_path
      ; kind
      }
  in
  dst_repo_root
;;

let force { Workspace_hgrc.Feature. feature_id;  feature_path } =
  if verbose
  then Debug.ams [%here] "Workspace.force" feature_path [%sexp_of: Feature_path.t];
  let enclosing_repo_root_abspath = enclosing_repo_root_abspath feature_path in
  let tmp_location =
    let n = ref 0 in
    fun () ->
      incr n;
      Abspath.of_string (concat [ Abspath.to_string enclosing_repo_root_abspath
                                ; ".tmp"
                                ; Int.to_string !n
                                ])
  in
  let%bind enclosing_repo_root =
    Workspace_util.create_repo_if_it_does_not_exist (`Share_of feature_path)
      ~repo_root_abspath:enclosing_repo_root_abspath
      ~create_repo:(fun () ->
        let%bind () =
          Async_interactive.printf !"setting up share of %{Feature_path} ...\n" feature_path
        in
        (* We first create the center repo in a temporary location, [tmp_center], and then
           create the scaffolded root in a temporary location, [tmp_enclosing_repo_root],
           and then later rename that to [enclosing_repo_root_abspath]. *)
        let tmp_center = tmp_location () in
        let%bind () = Abspath.rm_rf_exn tmp_center in
        let%bind center_repo_root =
          share_or_clone_and_update
            ~dst_repo_root_abspath:tmp_center
            ~dst_repo_root_human_readable_name:"workspace"
            ~revision:(`Feature feature_path)
            ~kind:(`Feature { feature_id; feature_path })
            (Local_clone (Feature_path.root feature_path))
        in
        let%bind tmp_enclosing_repo_root =
          match%bind Scaffold.load ~center_repo_root with
          | None -> return tmp_center
          | Some scaffold ->
            let tmp_enclosing_repo_root = tmp_location () in
            (* go sequentially because we need to visit parent directories first *)
            let%bind () = Abspath.rm_rf_exn tmp_enclosing_repo_root in
            let%bind () =
              Deferred.List.iter (Scaffold.satellites scaffold) ~how:`Sequential
                ~f:(fun repo ->
                  let%map (_ : Repo_root.t) =
                    share_or_clone_and_update
                      ~dst_repo_root_abspath:
                        (Abspath.append tmp_enclosing_repo_root repo.repo_root)
                      ~dst_repo_root_human_readable_name:repo.human_readable
                      ~revision:(`Unresolved_from_scaffold repo.revision)
                      ~kind:`Satellite_repo
                      (Remote repo.remote_repo_path)
                  in
                  ())
            in
            let center_relative_to_enclosing_repo =
              Scaffold.center_relative_to_enclosing_repo scaffold
            in
            let%bind () =
              Abspath.rename_exn
                ~src:tmp_center
                ~dst__delete_if_exists:
                  (Abspath.append tmp_enclosing_repo_root
                     center_relative_to_enclosing_repo)
            in
            let%bind () =
              Scaffold.Center_relative_to_enclosing_repo.save_exn
                ~enclosing_repo_root_abspath:tmp_enclosing_repo_root
                center_relative_to_enclosing_repo
            in
            return tmp_enclosing_repo_root
        in
        let%bind () =
          Abspath.rename_exn
            ~src:tmp_enclosing_repo_root
            ~dst__delete_if_exists:enclosing_repo_root_abspath
        in
        let%bind () =
          if am_functional_testing
          then Deferred.unit
          else (
            match%bind
              Abspath.file_exists_exn
                (Abspath.extend enclosing_repo_root_abspath
                   (File_name.of_string "jenga.conf"))
            with
            | false -> Deferred.unit
            | true ->
              (* We [jenga libmap.sexp] so that [Jane.find-library-in-project] works.  We
                 [jenga .omake-ocaml-bin] because this file is useful for tools that need
                 to know which OCaml is being used. *)
              let%map jenga_process =
                Process.create
                  ~working_dir:(Abspath.to_string enclosing_repo_root_abspath)
                  ~prog:"/j/office/app/jenga/prod/bin/jenga"
                  ~args:[ "libmap.sexp"; ".omake-ocaml-bin" ]
                  ()
              in
              (* We allow this call to fail.  For example, we do not want to prevent the
                 creation of a workspace if the jenga conf files have conflicts or
                 compilation errors.  The target can be created later.  We do not wait on
                 the command so that a slow building of the jengaroot does not slow down
                 [fe create]. *)
              jenga_process
              |> ok_exn
              |> (ignore : Process.t -> unit))
        in
        Async_interactive.printf !"done setting up share of %{Feature_path}\n" feature_path)
  in
  let%bind center_relative_to_enclosing_repo =
    Scaffold.Center_relative_to_enclosing_repo.load_exn
      ~enclosing_repo_root_abspath:(Repo_root.to_abspath enclosing_repo_root)
  in
  create ~feature_id:(Some feature_id) ~feature_path
    ~enclosing_repo_root ~center_relative_to_enclosing_repo
;;

let find ?feature_id feature_path =
  if verbose
  then Debug.ams [%here] "Workspace.find" feature_path [%sexp_of: Feature_path.t];
  let enclosing_repo_root_abspath = enclosing_repo_root_abspath feature_path in
  match%bind Sys.is_directory_exn (Abspath.to_string enclosing_repo_root_abspath) with
  | false -> return None
  | true ->
    let enclosing_repo_root = Repo_root.of_abspath enclosing_repo_root_abspath in
    let%bind center_relative_to_enclosing_repo =
      Scaffold.Center_relative_to_enclosing_repo.load_exn ~enclosing_repo_root_abspath
    in
    let%map t =
      create ~feature_id ~feature_path
        ~enclosing_repo_root ~center_relative_to_enclosing_repo
    in
    Some t
;;

let unix_find_dirs_exn ~in_ args : Relpath.t list Deferred.t =
  match%map
    Process.run_lines
      ~working_dir:"/"
      ~prog:"/bin/find"
      ~args:([ Abspath.to_string in_; "-type"; "d" ] @ args)
      ()
  with
  | Error err -> Error.raise err
  | Ok lines ->
    List.map lines ~f:(fun line ->
      let line = Abspath.of_string line in
      match Abspath.chop_prefix ~prefix:in_ line with
      | Ok dir -> dir
      | Error err ->
        raise_s [%sexp "unexpected path returned by find", (err : Error.t)])
;;

let character_not_leading_feature_names = '+'

let%test "no recursion in clones" =
  Char.equal
    (File_name.to_string Repo_clone.clone_file_name).[0]
    character_not_leading_feature_names
;;

let list () =
  let basedir = workspaces_basedir () in
  match%bind Sys.is_directory_exn (Abspath.to_string basedir) with
  | false -> return []
  | true ->
    let%bind basedirs =
      unix_find_dirs_exn ~in_:basedir
        [ (* print but do not recur in directories ending in [+share+] *)
          "-name"; File_name.to_string share
          ; "-prune"
          ; "-print"
          (* do not print or recur in directories ending in [+clone+] *)
          ; "-or"
          ; "("
          ; "-name"; Char.to_string character_not_leading_feature_names ^ "*"
          ; "-or"
          ; "-name"; ".hg"
          ; ")"
          ; "-prune"
        ]
    in
    let%map shares =
      Deferred.List.map basedirs ~how:(`Max_concurrent_jobs 10)
        ~f:(fun scaffold_relpath ->
          let feature_path =
            scaffold_relpath
            |> Relpath.parent_exn
            |> Relpath.to_string
            |> Feature_path.of_string
          in
          let enclosing_repo_root_abspath = Abspath.append basedir scaffold_relpath in
          let enclosing_repo_root = Repo_root.of_abspath enclosing_repo_root_abspath in
          let%bind center_relative_to_enclosing_repo =
            Scaffold.Center_relative_to_enclosing_repo.load_exn
              ~enclosing_repo_root_abspath
          in
          create ~feature_id:None ~feature_path
            ~enclosing_repo_root ~center_relative_to_enclosing_repo
        )
    in
    shares
    |> List.sort ~cmp:compare
;;

let find_repos t =
  let in_ = Repo_root.to_abspath t.enclosing_repo_root in
  let%map dirs = unix_find_dirs_exn ~in_ [ "-name"; ".hg"; "-prune" ] in
  List.map dirs ~f:(fun dir ->
    dir
    |> Abspath.append in_
    |> Abspath.parent_exn
    |> Repo_root.of_abspath)
;;

let distclean t =
  let%bind repo_roots = find_repos t in
  Deferred.List.iter repo_roots ~how:(`Max_concurrent_jobs 5) ~f:(fun repo_root ->
    let%map result = Hg.distclean repo_root in
    ok_exn result
  )
;;

let kill_build t =
  match%bind
    Build_manager.Jenga.find_in_enclosing_repo_root t.enclosing_repo_root
  with
  | None -> return ()
  | Some jenga ->
    (* Try to kill it via jane-elisp build manager *)
    let%bind build_manager_err =
      match%map
        Build_manager.kill_project (Build_manager.Project_id.of_jenga jenga)
      with
      | Ok stdout -> Error.of_string stdout
      | Error err -> err
    in
    match%bind
      Build_manager.Jenga.find_in_enclosing_repo_root t.enclosing_repo_root
    with
    | None -> return ()
    | Some jenga ->
      match%bind Build_manager.Jenga.kill jenga with
      | Ok () -> return ()
      | Error killing_jenga_err ->
        Async_interactive.print_s
          [%sexp
            "jenga seems to be running but failed to terminate",
            { workspace     = (t.feature_path    : Feature_path.t)
            ; build_manager = (build_manager_err : Error.t)
            ; killing_jenga = (killing_jenga_err : Error.t)
            }
          ]
;;

let removing t ~f =
  let%bind () = kill_build t in
  let basedir = workspaces_basedir () in
  (* if this is false, definitely shouldn't continue onward into the [rmdir] calls *)
  assert (Abspath.equal
            (enclosing_repo_root_abspath t.feature_path)
            (Repo_root.to_abspath t.enclosing_repo_root));
  let deleted_repo_relpath =
    Abspath.chop_prefix ~prefix:basedir (Repo_root.to_abspath t.enclosing_repo_root)
    |> ok_exn
  in
  let%bind () = f t in
  Deferred.repeat_until_finished deleted_repo_relpath
    (fun dir ->
       match Relpath.parent dir with
       | None -> return (`Finished ())
       | Some dir ->
         if Relpath.is_empty dir
         then return (`Finished ())
         else (
           let dir_abspath = Abspath.append basedir dir in
           let%bind children = Sys.ls_dir (Abspath.to_string dir_abspath) in
           if List.is_empty children
           then (
             let%map () = Unix.rmdir (Abspath.to_string dir_abspath) in
             `Repeat dir)
           else return (`Finished ())))
;;

let check_current_bookmark t =
  let%map result =
    Monitor.try_with ~extract_exn:true (fun () ->
      match%map Hg.current_bookmark (center_repo_root t) with
      | Error err ->
        raise_s [%sexp "cannot determine current bookmark", (err : Error.t)]
      | Ok bookmark ->
        if not (String.equal bookmark (Feature_path.to_string t.feature_path))
        then
          raise_s
            [%sexp
              "unexpected current bookmark",
              { current_bookmark = (bookmark       : string)
              ; expected         = (t.feature_path : Feature_path.t)
              }
            ]
    )
  in
  Result.map_error result ~f:(fun exn ->
    Error.create_s
      [%sexp
        { feature_path = (t.feature_path     : Feature_path.t)
        ; repo_root    = (center_repo_root t : Repo_root.t)
        ; error        = (exn                : Exn.t)
        }
      ])
;;

module Unclean_status = struct
  type t =
    | Clean
    | Unclean of Unclean_workspace_reason.t
end

module Log_in_dot_hg : sig
  val sexp
    :  ?level : Log.Level.t
    -> ?tags : (string * string) list
    -> Repo_root.t
    -> Sexp.t
    -> unit
end = struct

  let filename repo_root =
    Unix.getpid ()
    |> sprintf !".hg/iron.%{Pid}.log"
    |> Path_in_repo.of_string
    |> Repo_root.append repo_root
    |> Abspath.to_string
  ;;

  let get_log = Memo.general ~hashable:Repo_root.Hash_by_path.hashable (fun repo_root ->
    let output = Log.Output.file `Sexp ~filename:(filename repo_root) in
    Log.create ~level:`Debug ~output:[ output ] ~on_error:`Raise)
  ;;

  let sexp ?level ?tags repo_root sexp =
    (* Logs automatically flush when they are collected or when async shuts down. *)
    Log.sexp ?level ?tags (get_log repo_root) sexp;
  ;;
end

let unclean_status_throttle = Memo.unit (fun () ->
  let max_concurrent_jobs =
    Client_config.(get () |> Workspaces.unclean_workspaces_detection_max_concurrent_jobs)
  in
  Throttle.create ~continue_on_error:true ~max_concurrent_jobs)
;;

let unclean_status_internal t =
  let center_repo_root = center_repo_root t in
  let or_error f =
    match%map Monitor.try_with_or_error ~extract_exn:true (fun () -> f ()) with
    | Ok reasons -> reasons
    | Error err  -> [ Unclean_workspace_reason.One_reason.Error err ]
  in
  let%map reasons =
    [ or_error (fun () ->
        match%map Hg.status_cleanliness center_repo_root with
        | Ok Repo_is_clean -> []
        | Error error      ->
          Log_in_dot_hg.sexp center_repo_root
            [%sexp "Uncommitted changes", (error : Error.t)];
          [ Unclean_workspace_reason.One_reason.Uncommitted_changes ])
    ; or_error (fun () ->
        (* This hack of not using [hg outgoing] is not foolproof: one can still push to
           whatever repository they want by simply using the clone, or some share of the
           clone not managed by workspace (we won't complain when we should), or by
           pushing in a clone of the clone, or by pulling from the destination rather than
           pushing to it (we may complain when we shouldn't). These situations don't seem
           likely though. *)
        match%map Hg.phase center_repo_root (`Feature t.feature_path) with
        | `Public -> []
        (* We don't give any notification here since the user would have to manually set
           the phase of the revision to secret, so they probably already know it's
           outgoing.

           It's also the case that this could hide an unpushed ancestor changeset in draft
           phase. But if the user is manually setting phases, they are doing something
           tricky and so hopefully know what they are doing. *)
        | `Secret -> []
        | `Draft -> [ Unclean_workspace_reason.One_reason.Unpushed_changesets ])
    ; or_error (fun () ->
        match%map check_current_bookmark t with
        | Ok ()   -> []
        | Error _ -> [ Unclean_workspace_reason.One_reason.Invalid_current_bookmark ])
    ; or_error (fun () ->
        match
          Client_config.
            (get () |> Workspaces.unclean_workspaces_detection_includes_shelved_changes)
        with
        | false -> return []
        | true  ->
          match%map Hg.list_shelves_exn center_repo_root with
          | []     -> []
          | (_::_) -> [ Unclean_workspace_reason.One_reason.Shelved_changes ])
    ]
    |> Deferred.List.all
  in
  let reasons = List.concat reasons in
  match Unclean_workspace_reason.create reasons with
  | None        -> Unclean_status.Clean
  | Some reason -> Unclean_status.Unclean reason
;;

let unclean_status t =
  Throttle.enqueue (unclean_status_throttle ()) (fun () -> unclean_status_internal t)
;;

let delete t =
  match%map
    Monitor.try_with ~extract_exn:true (fun () ->
      let%bind () =
        match%map unclean_status t with
        | Clean -> ()
        | Unclean reason ->
          raise_s
            [%sexp
              "unclean workspace"
            , (Unclean_workspace_reason.to_string_hum reason : string)
            ]
      in
      removing t ~f:(fun t ->
        Async_interactive.Job.run !"deleting share of %{Feature_path}" t.feature_path
          ~f:(fun () -> Abspath.rm_rf_exn (Repo_root.to_abspath t.enclosing_repo_root)))
    )
  with
  | Ok () -> ()
  | Error exn ->
    raise_s
      [%sexp
        "Failed to delete workspace",
        { feature_path = (t.feature_path        : Feature_path.t)
        ; directory    = (t.enclosing_repo_root : Repo_root.t)
        ; error        = (exn                   : Exn.t)
        }
      ]
;;

let move_to src feature_id dst_feature =
  if verbose
  then Debug.ams [%here] "Workspace.move_to" (src, dst_feature)
         [%sexp_of: t * Feature_path.t];
  let enclosing_repo_root_abspath = enclosing_repo_root_abspath dst_feature in
  match%bind Sys.is_directory_exn (Abspath.to_string enclosing_repo_root_abspath) with
  | true ->
    (* dst exists already, so just delete src *)
    delete src
  | false ->
    (* move +share+ *)
    let%bind () =
      Unix.mkdir ~p:() (Filename.dirname (Abspath.to_string enclosing_repo_root_abspath))
    in
    let%bind () =
      removing src ~f:(fun src ->
        Unix.rename
          ~src:(Repo_root.to_string src.enclosing_repo_root)
          ~dst:(Abspath.to_string enclosing_repo_root_abspath))
    in
    let enclosing_repo_root = Repo_root.of_abspath enclosing_repo_root_abspath in
    let%bind center_relative_to_enclosing_repo =
      Scaffold.Center_relative_to_enclosing_repo.load_exn ~enclosing_repo_root_abspath
    in
    let%bind { remote_repo_path ; _ } =
      Get_feature_revs.rpc_to_server_exn { feature_path = dst_feature; rev_zero = None }
    in
    let repo_root =
      Repo_root.of_abspath
        (Repo_root.append enclosing_repo_root
           (Path_in_repo.of_relpath center_relative_to_enclosing_repo))
    in
    let%bind () =
      Workspace_hgrc.save
        { repo_root
        ; remote_repo_path
        ; kind = `Feature { feature_id
                          ; feature_path = dst_feature
                          }
        }
    in
    (* Preserve workspaces invariant, update to the new bookmark *)
    let revision = `Feature dst_feature in
    let%bind () =
      Hg.pull repo_root ~even_if_unclean:true ~from:remote_repo_path revision
    in
    Hg.update repo_root revision ~clean_after_update:No
;;
