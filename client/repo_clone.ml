open! Core
open! Async
open! Import

let verbose = Verbose.workspaces

let clone_file_name = File_name.of_string "+clone+"

type t =
  { root_feature     : Feature_name.t
  ; repo_root        : Repo_root.t
  ; remote_repo_path : Remote_repo_path.t
  ; tip_on_server    : Rev.t
  }
[@@deriving fields, sexp_of]

let compare t1 t2 = Feature_name.compare t1.root_feature t2.root_feature

let workspaces_basedir () = Client_config.(get () |> Workspaces.basedir)

let clone_repo_root_abspath ~root_feature =
  Abspath.append (workspaces_basedir ())
    (Relpath.of_list
       [ Feature_name.to_file_name root_feature
       ; clone_file_name
       ])
;;

(* The memoization is used to ensure sure that we don't concurrently try to clone the same
   repo multiple times to the same destination directory, as can happen otherwise when we
   have multiple [Workspace.force] functions running concurrently.  Cloning at most
   once each root feature is enough *)
let force_memo =
  Memo.general ~hashable:Feature_name.hashable (fun root_feature ->
    Monitor.try_with_or_error ~extract_exn:true (fun () ->
      let feature_path = Feature_path.of_root root_feature in
      let repo_root_abspath = clone_repo_root_abspath ~root_feature in
      let%bind feature_exists =
        Feature_exists.rpc_to_server_exn (Feature_path.of_root root_feature)
      in
      (match feature_exists with
       | Yes (_ : Feature_id.t) -> ()
       | No ->
         raise_s [%sexp "root feature doesn't exist", (root_feature : Feature_name.t)]);
      let%bind { remote_repo_path; tip = tip_on_server; _ } =
        Get_feature_revs.rpc_to_server_exn
          { feature_path
          ; rev_zero     = None
          }
      in
      let%map repo_root =
        Workspace_util.create_repo_if_it_does_not_exist (`Clone_of root_feature)
          ~repo_root_abspath
          ~create_repo:(fun () ->
            let%bind () =
              Async_interactive.printf !"setting up clone for %{Feature_name} ...\n%!"
                root_feature
            in
            let%bind repo_root =
              Hg.clone remote_repo_path
                ~dst_repo_root_abspath__delete_if_exists:repo_root_abspath
            in
            let repo_root = ok_exn repo_root in
            let%bind () =
              Hg.update repo_root (`Feature feature_path) ~clean_after_update:No
            in
            Workspace_hgrc.save
              { repo_root
              ; remote_repo_path
              ; kind = `Clone
              })
      in
      { root_feature
      ; repo_root
      ; remote_repo_path
      ; tip_on_server
      }))
;;

let force ~root_feature =
  Deferred.map (force_memo root_feature) ~f:ok_exn
;;

let find_internal root_features_by_name ~root_feature =
  match Map.find root_features_by_name root_feature with
  | None -> return None
  | Some (feature : List_root_features.Reaction.one) ->
    let repo_root_abspath = clone_repo_root_abspath ~root_feature in
    match%map Sys.is_directory_exn (Abspath.to_string repo_root_abspath) with
    | false -> None
    | true  -> Some { root_feature
                    ; repo_root        = Repo_root.of_abspath repo_root_abspath
                    ; remote_repo_path = feature.remote_repo_path
                    ; tip_on_server    = feature.tip
                    }
;;

let list () =
  let basedir = workspaces_basedir () in
  match%bind Sys.is_directory_exn (basedir |> Abspath.to_string) with
  | false -> return []
  | true ->
    let%bind subdirs = Sys.readdir (basedir |> Abspath.to_string) in
    let%bind root_features_by_name = Workspace_util.root_features_exn_with_memo () in
    let%map workspaces =
      Deferred.List.filter_map (Array.to_list subdirs) ~how:`Parallel ~f:(fun subdir ->
        match Option.try_with (fun () -> Feature_name.of_string subdir) with
        | None -> return None
        | Some root_feature -> find_internal root_features_by_name ~root_feature)
    in
    List.sort workspaces ~cmp:compare
;;

let find ~root_feature =
  let%bind root_features_by_name = Workspace_util.root_features_exn_with_memo () in
  find_internal root_features_by_name ~root_feature
;;

let pull_all_revs t =
  (* We set [even_if_unclean:true] because it seems unreasonable to have a big number of
     workspaces commands to start failing the minute a user has an uncommitted change in
     the corresponding repo clone. *)
  Hg.pull ~even_if_unclean:true t.repo_root ~from:t.remote_repo_path `All_revs
;;

module Spare_shares = struct

  let spare_share_dir t =
    Repo_root.append t.repo_root (Path_in_repo.of_string ".hg/spare-shares")
  ;;

  let is_spare_dir path =
    is_some (Option.try_with (fun () -> Uuid.of_string path))
  ;;

  let move_one t ~dst_repo_root_abspath =
    let spare_share_dir = spare_share_dir t in
    let%map result =
      Deferred.repeat_until_finished 0 (fun tries ->
        match%bind
          Monitor.try_with (fun () -> Sys.ls_dir (Abspath.to_string spare_share_dir))
        with
        | Error e ->
          return (`Finished (Error (Error.tag (Error.of_exn e) ~tag:"Couldn't ls")))
        | Ok l ->
          let l =
            l
            |> List.filter ~f:(fun d -> is_spare_dir d)
            |> List.permute
          in
          match l with
          | [] -> return (`Finished (Or_error.error_string "no spare shares"))
          | share :: _ ->
            let share = Abspath.extend spare_share_dir (File_name.of_string share) in
            if verbose
            then Verbose.message "trying to claim share" share [%sexp_of: Abspath.t];
            match%map
              Monitor.try_with (fun () ->
                Sys.rename (Abspath.to_string share)
                  (Abspath.to_string dst_repo_root_abspath))
            with
            | Error _ ->
              if tries > 3
              then `Finished (Or_error.error_string "got too many errors")
              else `Repeat (tries + 1)
            | Ok () ->
              if verbose
              then Verbose.message "succeeded in claiming share" share
                     [%sexp_of: Abspath.t];
              `Finished (Ok (Repo_root.of_abspath dst_repo_root_abspath)))
    in
    if verbose
    then (
      match result with
      | Ok _ -> ()
      | Error error ->
        Verbose.message "failed to claim share" error [%sexp_of: Error.t]);
    result
  ;;

  let refresh t ~desired_num_spares ~update_to =
    let spare_share_dir = spare_share_dir t in
    let staging_dir = Abspath.extend spare_share_dir (File_name.of_string "staging") in
    let%bind () = Unix.mkdir ~p:() (Abspath.to_string staging_dir) in
    let%bind () =
      (* When this refresh is run as part of [fe workspace create -features-in-my-todo]
         the revision is already known by the clone, so this pull is an no op.  Just there
         for robustness in case the function is called via another context.*)
      Hg.pull ~even_if_unclean:true t.repo_root ~from:t.remote_repo_path (`Rev update_to)
    in
    let%bind existing_spares =
      let%map ls_dir = Sys.ls_dir (Abspath.to_string spare_share_dir) in
      List.filter_map ls_dir
        ~f:(fun dir -> Option.try_with (fun () -> Uuid.of_string dir))
    in
    let num_existing_spares = List.length existing_spares in
    let share_repo_root_abspath ~in_ share =
      let dir =
        match in_ with
        | `Staging -> staging_dir
        | `Spare   -> spare_share_dir
      in
      Abspath.extend dir (Uuid.to_file_name share)
    in
    let move share ~from ~to_ =
      Sys.rename
        (Abspath.to_string (share_repo_root_abspath ~in_:from share))
        (Abspath.to_string (share_repo_root_abspath ~in_:to_  share))
    in
    (* Update existing shares, one at a time.  The temporary [move] to the staging
       directory is a way to allow concurrency while avoiding locking, so that one process
       can quickly acquire a share without having to wait for a process that is adding a
       share. *)
    let%bind () =
      Deferred.List.iter existing_spares ~f:(fun share ->
        let%bind () = move share ~from:`Spare ~to_:`Staging in
        let repo_root =
          Repo_root.of_abspath (share_repo_root_abspath ~in_:`Staging share)
        in
        let%bind share_rev = Hg.parent repo_root in
        let%bind () =
          (* Avoid noisy no-op updates in the trace of the command; without this, it
             appears to be doing a lot of redundant things. *)
          if Rev.equal_node_hash update_to share_rev
          then Deferred.unit
          else Hg.update repo_root (`Rev update_to) ~clean_after_update:No
        in
        move share ~from:`Staging ~to_:`Spare)
    in
    Deferred.repeat_until_finished num_existing_spares (fun num_spares_available ->
      if num_spares_available >= desired_num_spares
      then return (`Finished ())
      else (
        let share = Uuid.create () in
        let%bind repo_root =
          Hg.share t.repo_root
            ~dst_repo_root_abspath__delete_if_exists:
              (share_repo_root_abspath ~in_:`Staging share)
        in
        let repo_root = ok_exn repo_root in
        let%bind () = Hg.update repo_root (`Rev update_to) ~clean_after_update:No in
        let%map () = move share ~from:`Staging ~to_:`Spare in
        `Repeat (num_spares_available + 1)))
  ;;
end

let refresh_spare_shares = Spare_shares.refresh

let create_share_exn t ~dst_repo_root_abspath =
  match%bind Spare_shares.move_one t ~dst_repo_root_abspath with
  | Ok repo_root -> return repo_root
  | Error _ ->
    Hg.share (repo_root t) ~dst_repo_root_abspath__delete_if_exists:dst_repo_root_abspath
    |> Deferred.map ~f:ok_exn
;;
