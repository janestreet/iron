open! Core
open! Async
open! Import

let root_features_exn_with_memo =
  Memo.unit (fun () ->
    let%map root_features = List_root_features.rpc_to_server_exn () in
    root_features
    |> List.map ~f:(fun t -> t.root_feature, t)
    |> Feature_name.Map.of_alist_exn
  )
;;

let find_remote_repo_path_exn feature_path =
  let%map root_features = root_features_exn_with_memo () in
  match Map.find root_features (Feature_path.root feature_path) with
  | Some root_feature -> root_feature.remote_repo_path
  | None ->
    raise_s
      [%sexp
        "Iron does not know about the root feature of this feature"
      , (feature_path : Feature_path.t)
      ]
;;

(* there are locks on clone and share creation to prevent concurrent processes from
   stepping on each other when they are both trying to create them. *)
let create_repo_if_it_does_not_exist name ~repo_root_abspath ~create_repo =
  let name =
    match name with
    | `Clone_of root_feature -> sprintf !"clone of %{Feature_name}" root_feature
    | `Share_of feature      -> sprintf !"share of %{Feature_path}" feature
  in
  let%bind is_directory =
    Sys.is_directory_exn (Abspath.to_string repo_root_abspath)
  in
  let%map () =
    if is_directory
    then Deferred.unit (* already created. nothing to do. *)
    else (
      let lockfile = Abspath.to_string repo_root_abspath ^ ".lock" in
      match%bind
        Deferred.Or_error.try_with ~extract_exn:true (fun () ->
          Unix.mkdir ~p:() (Filename.dirname lockfile))
      with
      | Error e -> Error.raise (Error.tag e ~tag:(sprintf "failed to create %s" name))
      | Ok () ->
        if Verbose.workspaces
        then Debug.ams [%here] "create_repo" lockfile [%sexp_of: string];
        (* We reduce a bit the amount of logging "waiting for lock...". *)
        let waiting_for_lock_event =
          Clock.Event.run_after (Time.Span.of_sec 20.) (fun () ->
            Async_interactive.printf !"waiting for lock on %s ...\n" name)
            ()
        in
        Lock_file.Nfs.critical_section lockfile
          ~abort:(Clock.after Time.Span.(of_min 2.))
          ~f:(fun () ->
            let%bind () =
              match Clock.Event.abort waiting_for_lock_event () with
              | Ok | Previously_aborted () -> return ()
              | Previously_happened continue ->
                let%bind () = continue in
                Async_interactive.printf !"done waiting for lock on %s\n" name
            in
            (* Whoever we were waiting on may already have done the job for us, so check
               one last time if the directory exists already. *)
            match%bind Sys.is_directory_exn (Abspath.to_string repo_root_abspath) with
            | true -> Deferred.unit (* already created. nothing to do. *)
            | false -> create_repo ()))
  in
  Repo_root.of_abspath repo_root_abspath
;;
