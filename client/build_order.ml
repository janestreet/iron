open! Core
open! Async
open! Import

let verbose = Iron_options.Verbose.build_order

let libmap_file = File_name.of_string "libmap.sexp"
let jbuild_file = File_name.of_string "jbuild"

(* [Path_in_project] is like [Path_in_repo].  It is useful to have a different type
   because we support scaffolded repos, in which [Path_in_repo != Path_in_project]. *)
module Path_in_project : sig
  type t [@@deriving sexp]
  include Identifiable with type t := t
  val root : t
  val append : t -> Relpath.t -> t
  val extend : t -> File_name.t -> t
  val split_dir_file_exn : t -> t * File_name.t
  val of_list : File_name.t list -> t
  val to_relpath : t -> Relpath.t
end = Path_in_repo

module Project_root : sig
  type t
  val to_abspath : t -> Abspath.t
  val append : t -> Path_in_project.t -> Abspath.t

  (** [find repo_root] returns the project root containing [repo_root] and the
      path-in-project of [repo_root] in the project.  It works by visiting ancestors of
      [repo_root] until it finds [libmap_file]. *)
  val find : Repo_root.t -> (t * Path_in_project.t) Or_error.t Deferred.t
end = struct
  include Abspath
  let of_abspath t = t
  let to_abspath t = t
  let append t path_in_project = append t (Path_in_project.to_relpath path_in_project)

  let find repo_root =
    let rec loop abspath arcs =
      match%bind Abspath.file_exists_exn (Abspath.extend abspath libmap_file) with
      | true -> return (Ok (of_abspath abspath,
                            Path_in_project.of_list (List.rev arcs)))
      | false ->
        match Abspath.split_dir_file_exn abspath with
        | (parent, arc) -> loop parent (arc :: arcs)
        | exception _ ->
          return (error "could not find" libmap_file [%sexp_of: File_name.t])
    in
    loop (Repo_root.to_abspath repo_root) []
  ;;
end

module Libname : Identifiable = String

module Libmap : sig
  type t [@@deriving sexp_of]

  val load : Project_root.t -> t Deferred.t

  val libname_dir : t -> Libname.t -> Path_in_project.t Or_error.t
end = struct
  type t = Path_in_project.t Libname.Table.t [@@deriving sexp_of]

  let libname_dir t libname =
    match Hashtbl.find t libname with
    | Some dir -> Ok dir
    | None -> error "Libmap.libname_dir" libname [%sexp_of: Libname.t]
  ;;

  let load project_root =
    let%map list =
      Reader.load_sexp_exn
        (Abspath.to_string
           (Abspath.extend (Project_root.to_abspath project_root) libmap_file))
        [%of_sexp: (Libname.t * Path_in_project.t) list]
    in
    Libname.Table.of_alist_exn list
  ;;
end

module Dir_dependencies : sig
  (** [Hashtbl.find t path_in_project] is the directories that [path_in_project] directly
      depends on. *)
  type t = Path_in_project.Set.t Path_in_project.Table.t [@@deriving sexp_of]

  (** [create dirs ...] returns the directory dependencies of [dirs].  The resulting table
      has at least the keys in [dirs], but may have other keys, which arise from
      intermediate dependencies (e.g. if [dirs = [A; C]], and A-->B-->C).

      [additional_dependencies] comes from [directory_order] stanza in [.ferc].

      The implementation is to do a depth-first search from all [d] in [dirs], following
      along "edges" in [*.libdeps] files. *)
  val create
    :  Project_root.t
    -> dirs                    : Path_in_project.t list
    -> additional_dependencies : (Path_in_project.t * Path_in_project.t) list
    -> Libmap.t
    -> t Deferred.t
end = struct
  type t = Path_in_project.Set.t Path_in_project.Table.t [@@deriving sexp_of]

  let create project_root ~dirs ~additional_dependencies libmap =
    let dir_dependencies = Path_in_project.Table.create () in
    let additional_dependencies =
      Path_in_project.Table.of_alist_multi additional_dependencies
    in
    if verbose
    then Debug.ams [%here] "additional_dependencies" additional_dependencies
           [%sexp_of: Path_in_project.t list Path_in_project.Table.t];
    let rec compute_dir_dependencies path_in_project =
      if Hashtbl.mem dir_dependencies path_in_project
      then return ()
      else (
        (if verbose
         then Debug.ams [%here] "compute_dir_dependencies" path_in_project
                [%sexp_of: Path_in_project.t]);
        (* We set the key here to avoid recomputation -- we set it to the correct value
           later, once we've computed it. *)
        Hashtbl.set dir_dependencies ~key:path_in_project ~data:Path_in_project.Set.empty;
        let dir = Project_root.append project_root path_in_project in
        let%bind depends_ons =
          match%bind
            Monitor.try_with (fun () -> Sys.ls_dir (Abspath.to_string dir))
          with
          | Error _ -> return []
          | Ok files ->
            Deferred.List.map files ~how:`Parallel ~f:(fun file ->
              let%bind depends_on =
                if not (String.is_suffix file ~suffix:".libdeps")
                then return []
                else (
                  let libdeps_file = Abspath.extend dir (File_name.of_string file) in
                  Reader.load_sexps_exn (Abspath.to_string libdeps_file)
                    [%of_sexp: Libname.t])
              in
              let depends_on =
                List.filter_map depends_on ~f:(fun libname ->
                  match Libmap.libname_dir libmap libname with
                  | Ok dir -> Some dir
                  | Error _ ->
                    (* If the build isn't up to date, then build artifacts can mention
                       libraries that don't exist.  In that case, is is better to press on
                       and sort what can be sorted, rather than fail. *)
                    None)
                @ Option.value (Hashtbl.find additional_dependencies path_in_project)
                    ~default:[]
              in
              let%bind () =
                Deferred.List.iter ~how:`Parallel depends_on ~f:compute_dir_dependencies
              in
              return (Path_in_project.Set.of_list depends_on))
        in
        Hashtbl.set dir_dependencies ~key:path_in_project
          ~data:(Path_in_project.Set.union_list depends_ons);
        return ())
    in
    let%bind () =
      Deferred.List.iter ~how:`Parallel dirs ~f:compute_dir_dependencies
    in
    return dir_dependencies
  ;;
end

let dirs_in_build_order project_root ~dirs ~additional_dependencies libmap =
  let%map dir_dependencies =
    Dir_dependencies.create project_root ~dirs ~additional_dependencies libmap
  in
  if verbose
  then Debug.ams [%here] "dir_dependencies" dir_dependencies
         [%sexp_of: Dir_dependencies.t];
  if Hashtbl.for_all dir_dependencies ~f:Set.is_empty
  then Or_error.error_string "no directory dependencies -- missing *.libdeps files"
  else (
    let module Edge = Topological_sort.Edge in
    let nodes = Hashtbl.keys dir_dependencies in
    let edges =
      List.concat_map (Hashtbl.to_alist dir_dependencies)
        ~f:(fun (path_in_project, depends_on) ->
          List.map (Set.to_list depends_on) ~f:(fun depends_on ->
            { Edge.
              from = depends_on
            ; to_  = path_in_project
            }))
    in
    if verbose
    then Debug.ams [%here] "edges" edges [%sexp_of: Path_in_project.t Edge.t list];
    Topological_sort.sort (module Path_in_project) nodes edges ~verbose)
;;

module File_dependencies : sig
  (** [t] represents (some of the) dependencies among files in a directory.  [Hashtbl.find
      t file_name] is the files that [file_name] direcly depends on. *)
  type t = File_name.Set.t File_name.Table.t [@@deriving sexp_of]

  (** [create dir] returns file dependencies of files in [dir], as per all [.d] files. *)
  val create : dir:Abspath.t -> t Deferred.t
end = struct
  type t = File_name.Set.t File_name.Table.t [@@deriving sexp_of]

  let create ~dir =
    match%bind Monitor.try_with (fun () -> Sys.ls_dir (Abspath.to_string dir)) with
    | Error _ -> return (File_name.Table.create ())
    | Ok files ->
      let%map dependencies =
        Deferred.List.filter_map files ~how:`Parallel ~f:(fun dot_d_file ->
          match String.chop_suffix dot_d_file ~suffix:".d" with
          | None -> return None
          | Some file ->
            match%map try_with (fun () ->
              Reader.load_sexps_exn
                (Abspath.to_string (Abspath.extend dir (File_name.of_string dot_d_file)))
                [%of_sexp: string])
            with
            | Error _ -> None
            | Ok depends_on ->
              Some (File_name.of_string file,
                    depends_on
                    |> List.map ~f:(fun depends_on ->
                      File_name.of_string (concat [ depends_on; ".ml" ]))
                    |> File_name.Set.of_list))
      in
      File_name.Table.of_alist_exn dependencies
  ;;
end

let pos_in_class file =
  let string = File_name.to_string file in
  match
    List.findi [ ".mli"; ".ml" ] ~f:(fun _ suffix -> String.is_suffix string ~suffix)
  with
  | None -> assert false
  | Some (i, _) -> i
;;

(* A representative of an equivalence class of files, ignoring suffix. *)
module Representative : sig
  type t

  include Identifiable with type t := t

  val of_file_name : File_name.t -> t
end = struct
  include File_name

  let of_file_name file =
    match String.chop_suffix (File_name.to_string file) ~suffix:".mli" with
    | None -> file
    | Some prefix -> of_string (concat [ prefix; ".ml" ])
  ;;

  let%test_unit _ =
    List.iter
      [ "a.ml"
      ; "a.mli"
      ]
      ~f:(fun string ->
        [%test_result: t] ~expect:(of_string "a.ml")
          (of_file_name (File_name.of_string string)))
  ;;
end

let files_in_build_order ~dir files =
  (* 1. Create dependencies among all files, using *.d files.
     2. Coarsen into dependencies among representatives of equivalence classes, where
     foo.ml and foo.mli are equivalent.
     3. Topsort the coarse dependencies.
     4. Output files in order of the topsorted representatives, with .mli before .mli,
     putting foo_intf files just in front foo files. *)
  let%map file_dependencies = File_dependencies.create ~dir in
  let dependencies =
    Hashtbl.to_alist file_dependencies
    |> List.map ~f:(fun (file, depends_on) ->
      (Representative.of_file_name file,
       Representative.Set.map depends_on ~f:Representative.of_file_name))
  in
  let dependencies =
    (dependencies
     @ List.map files ~f:(fun file ->
       (Representative.of_file_name file, Representative.Set.empty)))
    |> Representative.Table.of_alist_multi
    |> Hashtbl.mapi ~f:(fun ~key:representative ~data:sets ->
      Set.remove (Representative.Set.union_list sets) representative)
  in
  if verbose then Debug.ams [%here] "dependencies" dependencies
                    [%sexp_of: Representative.Set.t Representative.Table.t];
  let module Edge = Topological_sort.Edge in
  let nodes = Hashtbl.keys dependencies in
  let edges =
    List.concat_map (Hashtbl.to_alist dependencies)
      ~f:(fun (representative, depends_on) ->
        List.map (Set.to_list depends_on) ~f:(fun depends_on ->
          { Edge.
            from = depends_on
          ; to_  = representative
          }))
  in
  let sorted_representatives =
    ok_exn (Topological_sort.sort (module Representative) nodes edges ~verbose)
  in
  let files_by_representative =
    files
    |> List.map ~f:(fun file -> (Representative.of_file_name file, file))
    |> Representative.Table.of_alist_multi
    |> Hashtbl.map ~f:(fun files ->
      List.sort files ~cmp:(fun file1 file2 -> pos_in_class file1 - pos_in_class file2))
  in
  (* Put [foo_intf.ml] with [foo.ml] if that exists. *)
  List.iter sorted_representatives ~f:(fun representative ->
    match
      String.chop_suffix (Representative.to_string representative) ~suffix:"_intf.ml"
    with
    | None -> ()
    | Some prefix ->
      let desired_representative = Representative.of_string (concat [ prefix; ".ml" ]) in
      match
        Hashtbl.find files_by_representative representative,
        Hashtbl.find files_by_representative desired_representative
      with
      | None, _ | _, None -> ()
      | Some files1, Some files2  ->
        Hashtbl.set files_by_representative ~key:representative ~data:[];
        Hashtbl.set files_by_representative ~key:desired_representative
          ~data:(files1 @ files2));
  let files =
    List.concat_map sorted_representatives ~f:(fun representative ->
      Option.value (Hashtbl.find files_by_representative representative) ~default:[])
  in
  let files_to_put_first = [ File_name.dot_fe; jbuild_file ] in
  let files =
    files_to_put_first
    @ List.filter files ~f:(fun file ->
      not (List.mem files_to_put_first file ~equal:File_name.equal))
  in
  (if verbose
   then Debug.ams [%here] "files_in_build_order" (dir, files)
          [%sexp_of: Abspath.t * File_name.t list]);
  files, file_dependencies
;;

module Compute_result = struct
  type t =
    | No_build_order of Error.t
    | Build_order of Path_in_project.t list * Path_in_project.t Hash_set.t
  [@@deriving sexp_of]
end

let expect_dependencies file =
  let string = File_name.to_string file in
  List.exists [ ".ml"; ".mli" ] ~f:(fun suffix -> String.is_suffix string ~suffix)
;;

(* [compute project_root paths] returns a sorted list of paths containing all the paths in
   [paths], and possibly more.  It first sorts by directories using [dirs_in_build_order],
   and then within directory using [files_in_build_order]. *)
let compute
      (project_root : Project_root.t)
      (paths : Path_in_project.t list)
      ~(additional_dir_dependencies : (Path_in_project.t * Path_in_project.t) list)
  : Compute_result.t Deferred.t =
  match%bind try_with ~extract_exn:true (fun () -> Libmap.load project_root) with
  | Error e -> return (Compute_result.No_build_order (Error.of_exn e))
  | Ok libmap ->
    if verbose then Debug.ams [%here] "libmap" libmap [%sexp_of: Libmap.t];
    let files_by_dir =
      List.map paths ~f:(fun path ->
        let dir, file =
          match Path_in_project.split_dir_file_exn path with
          | (dir, file) -> (dir, file)
          | exception _ ->
            (Path_in_project.root, Relpath.last_exn (Path_in_project.to_relpath path))
        in
        (dir, file))
      |> Path_in_project.Table.of_alist_multi
    in
    if verbose then Debug.ams [%here] "files_by_dir" files_by_dir
                      [%sexp_of: File_name.t list Path_in_project.Table.t];
    match%bind
      dirs_in_build_order project_root ~dirs:(Hashtbl.keys files_by_dir)
        ~additional_dependencies:additional_dir_dependencies libmap
    with
    | Error e -> return (Compute_result.No_build_order e)
    | Ok dirs_in_build_order ->
      if verbose then Debug.ams [%here] "dirs_in_build_order" dirs_in_build_order
                        [%sexp_of: Path_in_project.t list];
      let files_missing_dependencies = Path_in_project.Hash_set.create () in
      let%bind all_files =
        Deferred.List.map dirs_in_build_order ~how:`Parallel ~f:(fun dir ->
          match Hashtbl.find files_by_dir dir with
          | None -> return []
          | Some files ->
            (* The input can contain duplicates -- we dedup to prevent the same
               path-in-repo from occurring twice, which breaks the construction of a map
               from path-in-repo to index that is used for sorting. *)
            let files = File_name.Set.stable_dedup_list files in
            let%map (files, file_dependencies) =
              files_in_build_order ~dir:(Project_root.append project_root dir) files
            in
            List.map files ~f:(fun file ->
              let file_in_project = Path_in_project.extend dir file in
              if not (Hashtbl.mem file_dependencies file)
              && expect_dependencies file
              then Hash_set.add files_missing_dependencies file_in_project;
              file_in_project))
      in
      let all_files = List.concat all_files  in
      return (Compute_result.Build_order (all_files, files_missing_dependencies))
;;

module Staged_sort_result = struct
  type 'a t =
    | No_build_order of Error.t
    | Build_order of ('a list -> 'a list * Path_in_repo.t list)
end

let staged_sort repo_root list (path_in_repo : _ -> Path_in_repo.t) =
  match%bind Project_root.find repo_root with
  | Error error -> return (Staged_sort_result.No_build_order error)
  | Ok (project_root, repo_root_path_in_project) ->
    let to_path_in_project path_in_repo =
      Path_in_project.append repo_root_path_in_project
        (Path_in_repo.to_relpath path_in_repo)
    in
    let additional_dir_dependencies =
      Client_config.(directory_order (get ()))
      |> List.concat_map
           ~f:(function
             | [] -> []
             | path :: paths ->
               List.fold paths ~init:(path, []) ~f:(fun (prev, ac) path ->
                 (path, (path, prev) :: ac))
               |> snd)
      |> List.map ~f:(fun (p1, p2) -> (to_path_in_project p1, to_path_in_project p2))
    in
    match%map
      compute project_root
        (List.map list ~f:(fun x -> to_path_in_project (path_in_repo x)))
        ~additional_dir_dependencies
    with
    | No_build_order error -> Staged_sort_result.No_build_order error
    | Build_order (build_order, missing_dependencies) ->
      (* Now that we have the paths sorted in [build_order], we can sort [list], using the
         index of paths in [build_order]. *)
      let build_index_by_path =
        build_order
        |> List.mapi ~f:(fun i p -> (p, i))
        |> Path_in_project.Table.of_alist_exn
      in
      let build_index x =
        let path = path_in_repo x in
        match Hashtbl.find build_index_by_path (to_path_in_project path) with
        | Some i -> i
        | None -> raise_s [%sexp "Build_order.sort missing", (path : Path_in_repo.t)]
      in
      Build_order
        (fun list ->
           let sorted =
             List.sort list ~cmp:(fun d1 d2 ->
               Int.compare (build_index d1) (build_index d2))
           in
           let missing_dependencies =
             List.filter_map list ~f:(fun x ->
               let path_in_repo = path_in_repo x in
               if Hash_set.mem missing_dependencies (to_path_in_project path_in_repo)
               then Some path_in_repo
               else None)
           in
           (sorted, missing_dependencies))
;;

let sort_raised exn =
  eprintf "%s"
    (concat ["\
Warning: files are in alphabetical order rather than build order due to an exception:

"
            ; exn
              |> [%sexp_of: exn]
              |> Sexp.to_string_hum
            ; "
------------------------------------------------------------------------------------------
"
            ])
;;

let staged_sort repo_root_and_kind_or_error list path_in_repo =
  if List.length list <= 1
  then
    (* There's no sorting to do, and we'd like to avoid spurious errors in case the sort
       fails. *)
    return Fn.id
  else (
    let default_sort list =
      List.sort list ~cmp:(fun a1 a2 ->
        Path_in_repo.default_review_compare (path_in_repo a1) (path_in_repo a2))
    in
    let repo_root_option =
      match repo_root_and_kind_or_error with
      | Error _ -> None
      | Ok (repo_root, repo_root_kind) ->
        let repo_may_have_applicable_build_artifacts =
          match (repo_root_kind : Cmd_workspace.Repo_root_kind.t) with
          | Clone -> false
          | Program_started_in -> not (Cmd_workspace.workspaces_are_enabled ())
          | Satellite -> true
          | Workspace -> true
        in
        if repo_may_have_applicable_build_artifacts
        then Some repo_root
        else None
    in
    match repo_root_option with
    | None ->
      return (fun list ->
        eprintf "\
Warning: files are in alphabetical order because don't have a repo with build artifacts.

";
        default_sort list)
    | Some repo_root ->
      match%map
        try_with ~extract_exn:true (fun () -> staged_sort repo_root list path_in_repo)
      with
      | Error exn ->
        (fun list ->
           sort_raised exn;
           default_sort list)
      | Ok (No_build_order error) ->
        fun list ->
          eprintf "\
Warning: files are in alphabetical order because there are no build artifacts.

";
          if verbose then Debug.ams [%here] "error" error [%sexp_of: Error.t];
          default_sort list
      | Ok (Build_order sort) ->
        fun list ->
          match sort list with
          | exception exn ->
            sort_raised exn;
            default_sort list;
          | (list, missing_dependencies) ->
            if not (List.is_empty missing_dependencies)
            then (
              eprintf "\
Warning: files may not be in build order because of missing build artifacts.

";
              if verbose
              then Debug.ams [%here] "missing_dependencies" missing_dependencies
                     [%sexp_of: Path_in_repo.t list]);
            list)
;;

let sort repo_root_and_kind_or_error list path_in_repo =
  let%map sort = staged_sort repo_root_and_kind_or_error list path_in_repo in
  sort list
;;
