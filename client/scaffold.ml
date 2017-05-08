open Core
open Async
open Import

let verbose = Verbose.workspaces

module Satellite = struct
  type t =
    { repo_root        : Relpath.t
    ; remote_repo_path : Remote_repo_path.t
    ; human_readable   : string
    ; revision         : string
    }
  [@@deriving fields, sexp_of]
end

type t =
  { center_relative_to_enclosing_repo : Relpath.t
  ; satellites                        : Satellite.t list
  }
[@@deriving fields, sexp_of]

(* We do not want a dependency into scaffold lib or bin and the scaffold files Iron
   care about are simple enough.  Let's just implement here a minimal scaffold
   standalone parser customized for Iron's needs *)
module Standalone_scaffold_reader : sig
  val load_exn : Abspath.t -> t Deferred.t
  val parse_exn : string -> t
end = struct
  (* Historical type for scaffold file on disk *)
  module On_disk = struct
    type t =
      { dir                 : string sexp_option
      ; repo                : string
      ; repo_push           : Sexp.t sexp_option
      ; id                  : string sexp_option
      ; refresh_id_scaffold : Sexp.t sexp_option
      ; others              : t sexp_list
      }
    [@@deriving sexp]
  end

  module Satellite_internal = struct
    type t =
      { remote_repo_path : Remote_repo_path.t
      ; revision         : string
      }
    [@@deriving sexp_of]
  end

  (* Temporary representation easier to deal with *)
  type t =
    { repo      : [ `Center | `Satellite of Satellite_internal.t ]
    ; subtrees  : t Relpath.Map.t (* directory -> subtree in that directory*)
    }
  [@@deriving sexp_of]

  let of_disk_repr on_disk =
    let rec aux ({ On_disk. repo; id; others; _ } as on_disk) =
      let repo =
        if String.(=) repo "self"
        then `Center
        else (
          let revision =
            match id with
            | Some id -> id
            | None ->
              raise_s [%sexp "id expected for satellite", (on_disk : On_disk.t)]
          in
          `Satellite { Satellite_internal.
                       remote_repo_path = Remote_repo_path.of_string repo
                     ; revision
                     })
      in
      let subtrees =
        List.map others ~f:(fun sub ->
          match sub.dir with
          | None -> raise_s [%sexp "dir expected for satellite"
                                 , (on_disk : On_disk.t), (sub : On_disk.t)]
          | Some dir ->
            Relpath.of_string dir, aux sub)
        |> Relpath.Map.of_alist_exn
      in
      { repo
      ; subtrees
      }
    in
    let t = aux on_disk in
    if verbose
    then
      Debug.eprint_s
        [%sexp
          [%here],
          "Iron scaffold reader",
          { on_disk : On_disk.t
          ; t       : t
          }
        ];
    t
  ;;

  let to_list t =
    let rec aux relpath t acc =
      let acc = (relpath, t.repo) :: acc in
      Map.fold t.subtrees ~init:acc ~f:(fun ~key ~data ->
        aux (Relpath.append relpath key) data)
    in
    aux Relpath.empty t []
  ;;

  let of_on_disk on_disk =
    let repos = to_list (of_disk_repr on_disk) in
    let center_relative_to_enclosing_repo, satellites =
      List.partition_map repos ~f:(fun (repo_root, repo) ->
        match repo with
        | `Center -> `Fst repo_root
        | `Satellite { remote_repo_path; revision } ->
          let human_readable =
            let name =
              match Remote_repo_path.family remote_repo_path with
              | Some family -> family
              | None -> Remote_repo_path.to_string remote_repo_path
            in
            concat [ name; " satellite" ]
          in
          `Snd { Satellite.
                 repo_root
               ; remote_repo_path
               ; human_readable
               ; revision
               })
    in
    let center_relative_to_enclosing_repo =
      match center_relative_to_enclosing_repo with
      | [] | _ :: _ :: _ -> failwith "Scaffold.load did not get exactly one center"
      | [ center ] -> center
    in
    let satellites =
      List.sort satellites ~cmp:(fun (r1 : Satellite.t) r2 ->
        Relpath.alphabetic_compare r1.repo_root r2.repo_root)
    in
    { center_relative_to_enclosing_repo
    ; satellites
    }
  ;;

  let load_exn abspath =
    let%map on_disk =
      Reader.load_sexp_exn (Abspath.to_string abspath) [%of_sexp: On_disk.t]
    in
    of_on_disk on_disk
  ;;

  let parse_exn s =
    of_on_disk ([%of_sexp: On_disk.t] (Sexp.of_string s))
  ;;

end

let load_scaffold_file_exn = Standalone_scaffold_reader.load_exn
let parse_scaffold_contents_exn = Standalone_scaffold_reader.parse_exn

let load ~center_repo_root:center =
  let scaffold_file_dir = Repo_root.to_abspath center in
  let scaffold_file =
    Abspath.extend scaffold_file_dir (File_name.of_string "scaffold.sexp")
  in
  match%bind Abspath.file_exists_exn scaffold_file with
  | false -> return None
  | true ->
    match%map
      Monitor.try_with ~extract_exn:true (fun () ->
        load_scaffold_file_exn scaffold_file)
    with
    | Ok t ->
      if verbose then Debug.ams [%here] "Iron scaffold reader. t" t [%sexp_of: t];
      Some t
    | Error exn ->
      raise_s
        [%sexp
          "error loading scaffold file",
          { file = (scaffold_file : Abspath.t)
          ; exn  = (exn           : Exn.t)
          }
        ]
;;

module Center_relative_to_enclosing_repo : sig
  val load_exn : enclosing_repo_root_abspath:Abspath.t -> Relpath.t Deferred.t
  val save_exn : enclosing_repo_root_abspath:Abspath.t -> Relpath.t -> unit Deferred.t
end = struct

  let pointer_file ~enclosing_repo_root_abspath =
    Abspath.append enclosing_repo_root_abspath (Relpath.of_string ".hg/scaffolded-subdir")
  ;;

  let load_exn ~enclosing_repo_root_abspath =
    let pointer_file = pointer_file ~enclosing_repo_root_abspath in
    match%bind Abspath.file_exists_exn pointer_file with
    | false -> return Relpath.empty
    | true ->
      let%bind relative_path =
        Reader.file_contents (Abspath.to_string pointer_file)
      in
      return (Relpath.of_string (String.strip relative_path))
  ;;

  let save_exn ~enclosing_repo_root_abspath center_repo_root =
    Writer.save (Abspath.to_string (pointer_file ~enclosing_repo_root_abspath))
      ~contents:(concat [ Relpath.to_string center_repo_root; "\n" ])
  ;;
end

let find_enclosing_repo_root t ~center_repo_root =
  let scaffold_dir = Repo_root.to_abspath center_repo_root in
  match Abspath.chop_suffix scaffold_dir ~suffix:t.center_relative_to_enclosing_repo with
  | Error _ -> return `Isolated_repo_has_no_enclosing_repo_root
  | Ok enclosing_repo_root_abspath ->
    match%map
      Monitor.try_with (fun () ->
        Center_relative_to_enclosing_repo.load_exn ~enclosing_repo_root_abspath)
    with
    | Error _ -> `Isolated_repo_has_no_enclosing_repo_root
    | Ok relpath ->
      if Relpath.equal relpath t.center_relative_to_enclosing_repo
      then `Enclosing_repo_root (Repo_root.of_abspath enclosing_repo_root_abspath)
      else `Isolated_repo_has_no_enclosing_repo_root
;;

let update_satellite_repos t ~center_repo_root =
  match%bind find_enclosing_repo_root t ~center_repo_root with
  | `Isolated_repo_has_no_enclosing_repo_root -> Deferred.unit
  | `Enclosing_repo_root enclosing_repo_root ->
    let do_nothing = return (Ok (Lazy_deferred.create (fun () -> Deferred.unit))) in
    let%bind updates =
      Deferred.List.map t.satellites ~f:(fun repo ->
        let repo_root =
          Abspath.append (Repo_root.to_abspath enclosing_repo_root) repo.repo_root
          |> Repo_root.of_abspath ~human_readable:repo.human_readable
        in
        let%bind revision =
          let%map revision =
            Hg.Scaffold.resolve_revision repo.remote_repo_path
              ~revision:repo.revision
              ~scaffold_requires_global_tag_or_rev_hash:false
          in
          revision
          |> ok_exn
          |> Revset.of_string
        in
        let%bind parent_is_at_right_revision =
          match%bind Hg.create_rev repo_root revision with
          | Error _ -> return false
          | Ok rev ->
            let%map current_revision = Hg.parent repo_root in
            Rev.Compare_by_hash.equal current_revision rev
        in
        if parent_is_at_right_revision
        then do_nothing
        else
          Hg.status_cleanliness repo_root
          >>|? fun repo_is_clean ->
          Lazy_deferred.create (fun () ->
            let%bind () =
              Hg.pull
                repo_root
                ~repo_is_clean
                ~from:repo.remote_repo_path
                (`Revset revision)
            in
            Hg.update repo_root (`Revset revision)
              ~clean_after_update:(Yes repo_is_clean)))
    in
    updates
    |> Or_error.combine_errors
    |> ok_exn
    |> Deferred.List.iter ~how:(`Max_concurrent_jobs 5)
         ~f:(fun job -> Lazy_deferred.force_exn job)
;;
