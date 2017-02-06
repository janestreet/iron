module Stable_format = struct
  open! Core.Core_stable
  open! Import_stable

  module Obligations_repo = Obligations_repo.Stable
  module Review_attributes = Review_attributes.Stable

  module V5 = struct
    type t =
      { obligations_repo : Obligations_repo.V5.t
      ; by_path          : (Path_in_repo.V1.t * Review_attributes.V2.t) list
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 6a390a0210853740209cfa70538866e7 |}]
    ;;
  end

  module Model = V5
end

open! Core
open! Async
open! Import

(* The sharing across values of that type in the worker_cache on the server is
   implemented in the separate module [Obligations_consing] *)
type t =
  { obligations_repo : [ `Fake | `Actual of Obligations_repo.t ]
  ; by_path          : Review_attributes.t Path_in_repo.Table.t
  }

let sorted_files_alist t =
  t.by_path
  |> Hashtbl.to_alist
  |> List.sort ~cmp:(fun (p1, _) (p2, _) ->
    String.alphabetic_compare (Path_in_repo.to_string p1) (Path_in_repo.to_string p2))
;;

let sexp_of_t t =
  sorted_files_alist t
  |> [%sexp_of: (Path_in_repo.t * Review_attributes.t) list]
;;

let file_owner (t : t) path_in_repo =
  match Hashtbl.find t.by_path path_in_repo with
  | Some a -> Ok a.owner
  | None -> error "no such file" path_in_repo [%sexp_of: Path_in_repo.t]
;;

let fake ~manifest review_attributes =
  if not am_functional_testing
  then Rpc_to_server_prevention.disable_rpc_to_server Use_of_fake_obligations;
  let by_path =
    Path_in_repo.Table.of_alist_exn
      (List.map manifest ~f:(fun path_in_repo -> (path_in_repo, review_attributes)))
  in
  { by_path; obligations_repo = `Fake }
;;

let maybe_relativize repo_root path_in_repo ~cwd : Path.t =
  let abspath = Repo_root.append repo_root path_in_repo in
  match Abspath.chop_prefix ~prefix:cwd abspath with
  | Ok relpath -> Relpath relpath
  | Error _    -> Abspath abspath
;;

let report_unused_definitions (type a) t label
      (module V : Validated_string.S with type t = a)
      of_obligations_repo of_review_attributes =
  match t.obligations_repo with
  | `Fake -> ()
  | `Actual obligations_repo ->
    let unused_definitions =
      V.Hash_set.of_list (of_obligations_repo obligations_repo)
    in
    Hashtbl.iter t.by_path ~f:(fun review_attributes ->
      Set.iter (of_review_attributes review_attributes) ~f:(fun use ->
        Hash_set.remove unused_definitions use));
    if not (Hash_set.is_empty unused_definitions)
    then
      raise_s
        [%sexp
          (sprintf "unused %s -- please remove from obligations-repo.sexp" label : string)
        , (unused_definitions
           |> Hash_set.to_list
           |> List.sort ~cmp:V.compare
           : V.t list)
        ]
;;

let report_unused_projections t =
  report_unused_definitions t "build projections" (module Build_projection_name)
    (fun t -> t.build_projections |> Map.keys)
    Review_attributes.build_projections
;;

let report_unused_tags
      t ~(defined_in_local_repo : Obligations_repo.Defined_in_local_repo.t) =
  report_unused_definitions t "tags" (module Tag)
    (const (Set.to_list defined_in_local_repo.tags))
    Review_attributes.tags
;;

let low_review t ~in_:(build_projection : Build_projection.t) =
  Path_in_repo.Set.of_list
    (list_of_iter (fun ~f ->
       Hashtbl.iteri t.by_path ~f:(fun ~key:path_in_repo ~data:attributes ->
         if Set.mem attributes.build_projections build_projection.name
         && (attributes.fewer_than_min_reviewers
             || not (Scrutiny_name.equal attributes.scrutiny_name
                       build_projection.default_scrutiny.name))
         then f path_in_repo)));
;;

let low_review_files t =
  match t.obligations_repo with
  | `Fake -> Build_projection_name.Map.empty
  | `Actual obligations_repo ->
    Map.filter_map obligations_repo.build_projections ~f:(fun build_projection ->
      if not build_projection.require_low_review_file
      then None
      else Some (low_review t ~in_:build_projection))
;;

let check_low_review_files t repo_root ~manifest ~cwd =
  match t.obligations_repo with
  | `Fake -> return ()
  | `Actual obligations_repo ->
    Deferred.List.iter ~how:(`Max_concurrent_jobs 30)
      (Map.data obligations_repo.build_projections)
      ~f:(fun build_projection ->
        if not build_projection.require_low_review_file
        then return ()
        else (
          let low_review_file_in_repo =
            Path_in_repo.low_review_file build_projection.name
          in
          let low_review_file = maybe_relativize repo_root low_review_file_in_repo ~cwd in
          let%bind low_review_file_exists =
            Sys.file_exists_exn (Path.to_string low_review_file)
          in
          match Hash_set.mem manifest low_review_file_in_repo, low_review_file_exists with
          | false, false ->
            raise_s
              [%sexp
                "missing low-review file"
              , (low_review_file_in_repo : Path_in_repo.t)
              ]
          | false, true ->
            raise_s
              [%sexp
                "untracked low-review file"
              , (low_review_file_in_repo : Path_in_repo.t)
              ]
          | true, false ->
            raise_s
              [%sexp
                "deleted but tracked low-review file"
              , (low_review_file_in_repo : Path_in_repo.t)
              ]
          | true, true ->
            let%map stated_exceptions =
              Reader.load_sexps_exn (Path.to_string low_review_file)
                [%of_sexp: Path_in_repo.t]
            in
            Error_context.within ~file:low_review_file (fun e ->
              let actual = low_review t ~in_:build_projection in
              let stated = Path_in_repo.Set.of_list stated_exceptions in
              let missing = Set.diff actual stated in
              if not (Set.is_empty missing)
              then
                Error_context.raise_s e
                  [%sexp "missing low-review files", (missing : Path_in_repo.Set.t)];
              let extraneous = Set.diff stated actual in
              if not (Set.is_empty extraneous)
              then
                Error_context.raise_s e
                  [%sexp "files are not low review", (extraneous : Path_in_repo.Set.t)])
            |> ok_exn))
;;

module type Error = sig
  type t [@@deriving compare, sexp_of]
end

let report_errors (type x)
      singular_message ?(plural_message = singular_message ^ "s") xs x =
  let module X = (val x : Error with type t = x) in
  match xs with
  | [] -> ()
  | [ x ] -> raise_s [%sexp (singular_message : string), (x : X.t)]
  | _ ->
    let xs = List.sort xs ~cmp:X.compare in
    raise_s [%sexp (plural_message : string), (xs : X.t list)]
;;

let create hg ?skip_full_repo_checks ~repo_root ~dirs ~manifest ~aliases () =
  let%bind obligations_repo_or_error =
    Obligations_repo.load hg repo_root ~aliases
  in
  let obligations_version_or_error =
    Or_error.map obligations_repo_or_error
      ~f:(fun (repo, _) -> repo.obligations_version)
  in
  let t_or_error_def =
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      let obligations_repo, defined_in_local_repo = ok_exn obligations_repo_or_error in
      let%bind cwd = Sys.getcwd () in
      let cwd = Abspath.of_string cwd in
      let manifest = manifest |> Path_in_repo.Hash_set.of_list in
      let dirs_to_compute =
        manifest
        |> Hash_set.to_list
        |> List.filter_map ~f:(fun file ->
          let dir, file = Path_in_repo.split_dir_file_exn file in
          let should_keep =
            match dirs with
            | `All -> true
            | `Below below -> Path_in_repo.is_prefix ~prefix:below dir
            | `Only_this dir2 -> Path_in_repo.equal dir dir2
          in
          if should_keep then Some (dir, file) else None)
        |> Path_in_repo.Table.of_alist_multi
      in
      (* Determine the .fe.sexp for each dir we need to compute. *)
      let dot_fes_to_load = Path_in_repo.Hash_set.create () in
      let maybe_untracked_dot_fes = Path_in_repo.Hash_set.create () in
      let dot_fes_used_in_subdirectory = Path_in_repo.Hash_set.create () in
      let no_dot_fe = ref [] in
      let dirs_to_compute =
        Hashtbl.mapi dirs_to_compute ~f:(fun ~key:dir ~data:files ->
          let rec loop ancestor ~is_proper_ancestor =
            let dot_fe = Path_in_repo.extend ancestor File_name.dot_fe in
            if Hash_set.mem manifest dot_fe
            then (
              Hash_set.add dot_fes_to_load dot_fe;
              if is_proper_ancestor then Hash_set.add dot_fes_used_in_subdirectory dot_fe;
              dot_fe)
            else (
              Hash_set.add maybe_untracked_dot_fes dot_fe;
              match Path_in_repo.parent ancestor with
              | Some parent -> loop parent ~is_proper_ancestor:true
              | None ->
                no_dot_fe := dir :: !no_dot_fe;
                (* doesn't matter what's here, since we'll report an error *)
                dot_fe)
          in
          let dot_fe = loop dir ~is_proper_ancestor:false in
          (dot_fe, files))
      in
      (* Check for untracked .fe.sexp files. *)
      let untracked_dot_fes = ref [] in
      let%bind () =
        Deferred.List.iter ~how:(`Max_concurrent_jobs 30)
          (Hash_set.to_list maybe_untracked_dot_fes)
          ~f:(fun dot_fe ->
            let dot_fe = maybe_relativize repo_root dot_fe ~cwd in
            let%map dot_fe_exists = Sys.file_exists_exn (Path.to_string dot_fe) in
            if dot_fe_exists
            then untracked_dot_fes := dot_fe :: !untracked_dot_fes)
      in
      (* We first report if there are any untracked .fe.sexp files, and only if there
         aren't do we report missing ones. *)
      report_errors "untracked .fe.sexp file" !untracked_dot_fes (module Path);
      report_errors
        "no .fe.sexp in or above directory"
        ~plural_message:"no .fe.sexp in or above directories"
        !no_dot_fe
        (module Path_in_repo);
      (* Load all the needed .fe.sexp files. *)
      let deleted = ref [] in
      let syntax_errors = ref [] in
      let load_dot_fe dot_fe =
        let dot_fe_path = maybe_relativize repo_root dot_fe ~cwd in
        let%bind dot_fe_exists = Sys.file_exists_exn (Path.to_string dot_fe_path) in
        let declarations =
          if not dot_fe_exists
          then (
            deleted := dot_fe_path :: !deleted;
            return [])
          else (
            match%map
              Reader.load_annotated_sexps (Path.to_string dot_fe_path)
                Dot_fe.Declaration.of_annotated_sexp
            with
            | Error error -> syntax_errors := error :: !syntax_errors; []
            | Ok declarations -> declarations)
        in
        let%map declarations = declarations in
        (dot_fe, declarations)
      in
      let%bind declarations_by_dot_fe =
        Deferred.List.map ~how:(`Max_concurrent_jobs 30)
          (Hash_set.to_list dot_fes_to_load) ~f:load_dot_fe
      in
      let declarations_by_dot_fe =
        declarations_by_dot_fe |> Path_in_repo.Table.of_alist_exn
      in
      report_errors "deleted but tracked .fe.sexp file" !deleted (module Path);
      report_errors "syntax error" !syntax_errors (module Error);
      (* Report as missing .fe.sexp files whose closest ancestor .fe.sexp file doesn't
         have a [Used_in_subdirectory] declaration. *)
      let missing =
        List.filter_map (Hashtbl.to_alist dirs_to_compute) ~f:(fun (dir, (dot_fe, _)) ->
          if Path_in_repo.equal dir (Path_in_repo.parent_exn dot_fe)
          || Dot_fe.has_used_in_subdirectory
               (Hashtbl.find_exn declarations_by_dot_fe dot_fe)
          then None
          else Some dir)
      in
      report_errors
        "directory missing .fe.sexp file"
        ~plural_message:"directories missing .fe.sexp file"
        missing (module Path_in_repo);
      (* Evaluate the .fe.sexp file in each directory we need. *)
      let useless_dot_fes = Hash_set.copy dot_fes_to_load in
      let invalid = ref [] in
      let scrutiny_names_of_files_affected_by_dot_fe = Path_in_repo.Table.create () in
      let by_path =
        Hashtbl.to_alist dirs_to_compute
        |> List.concat_map ~f:(fun (dir, (dot_fe, files_in_directory)) ->
          let scrutiny_names_of_files_affected_by_dot_fe =
            Hashtbl.find_or_add scrutiny_names_of_files_affected_by_dot_fe dot_fe
              ~default:Scrutiny_name.Hash_set.create
          in
          let declarations = Hashtbl.find_exn declarations_by_dot_fe dot_fe in
          let used_in_subdirectory =
            ok_exn
              (Path_in_repo.chop_prefix dir ~prefix:(Path_in_repo.parent_exn dot_fe))
          in
          let files_in_directory = File_name.Set.of_list files_in_directory in
          let has_dot_fe = Set.mem files_in_directory File_name.dot_fe in
          let files_in_directory =
            (* We already checked for missing .fe.sexp files, and we would like it to be
               OK for a .fe.sexp used in an ancestor to refer to .fe.sexp and still be
               used in a descendant (which of course does not have a .fe.sexp).  So, we
               pretend that a .fe.sexp is always present. *)
            if has_dot_fe
            then files_in_directory
            else Set.add files_in_directory File_name.dot_fe
          in
          if Set.length files_in_directory > 1
          then Hash_set.remove useless_dot_fes dot_fe;
          match
            Dot_fe.eval declarations
              ~dot_fe:(maybe_relativize repo_root dot_fe ~cwd)
              ~used_in_subdirectory
              ~used_in_subdirectory_declaration_is_allowed:
                (match dirs with
                 | `Only_this _ ->
                   (* We have not processed subdirectories, so we must allow
                      [Used_in_subdirectory], since we don't know whether it's
                      necessary. *)
                   true
                 | `All | `Below _ ->
                   (* We have processed subdirectories, so we should only allow
                      [Used_in_subdirectory] if [dot_fe] is used in a subdirectory. *)
                   Hash_set.mem dot_fes_used_in_subdirectory dot_fe)
              ~files_in_directory
              ~obligations_repo
              ~aliases
          with
          | Error e -> invalid := e :: !invalid; []
          | Ok attributes_by_file ->
            let attributes_by_file =
              if has_dot_fe
              then attributes_by_file
              else Map.remove attributes_by_file File_name.dot_fe
            in
            let attributes_by_file = Map.to_alist attributes_by_file in
            List.iter attributes_by_file ~f:(fun (_, attributes) ->
              Hash_set.add scrutiny_names_of_files_affected_by_dot_fe
                attributes.scrutiny_name);
            List.map attributes_by_file ~f:(fun (file, attributes) ->
              (Path_in_repo.extend dir file, attributes)))
        |> Path_in_repo.Table.of_alist_exn
      in
      report_errors "invalid .fe.sexp file" !invalid (module Error);
      Hashtbl.iteri scrutiny_names_of_files_affected_by_dot_fe
        ~f:(fun ~key:dot_fe ~data:scrutiny_names ->
          Hashtbl.change by_path dot_fe ~f:(Option.map ~f:(fun dot_fe_attributes ->
            let obligations_read_by =
              List.map (Hash_set.to_list scrutiny_names) ~f:(fun scrutiny_name ->
                match Map.find obligations_repo.scrutinies scrutiny_name with
                | Some scrutiny -> scrutiny.obligations_read_by
                | None ->
                  raise_s
                    [%sexp "This [.fe.sexp] file affects file of undefined scrutiny"
                         , [%here]
                         , { dot_fe        : Path_in_repo.t
                           ; scrutiny_name : Scrutiny_name.t
                           }
                    ])
            in
            let augmented_obligations =
              Review_obligation.and_
                (Review_attributes.review_obligation dot_fe_attributes
                 :: obligations_read_by)
            in
            Review_attributes.with_review_obligation dot_fe_attributes
              ~review_obligation:augmented_obligations
          )));
      let t =
        { obligations_repo = `Actual obligations_repo
        ; by_path
        }
      in
      let do_full_repo_checks =
        Option.is_none skip_full_repo_checks
        && (match dirs with
          | `All -> true
          | `Below dir -> Path_in_repo.equal dir Path_in_repo.root
          | `Only_this _ -> false)
      in
      let%bind () =
        if not do_full_repo_checks
        then return ()
        else (
          report_unused_projections t;
          report_unused_tags t ~defined_in_local_repo;
          if obligations_repo.disallow_useless_dot_fe
          && not (Hash_set.is_empty useless_dot_fes)
          then report_errors "useless .fe.sexp file" (useless_dot_fes |> Hash_set.to_list)
                 (module Path_in_repo);
          check_low_review_files t repo_root ~manifest ~cwd)
      in
      return t
    )
  in
  let%map t_or_error = t_or_error_def in
  t_or_error, obligations_version_or_error
;;

module Stable = struct
  module V5 = struct

    module Stable_format = Stable_format.V5

    let of_model { obligations_repo
                 ; by_path
                 } =
      match obligations_repo with
      | `Fake ->
        failwith "Obligations.Stable.V4.of_model: \
                  unexpected serialization of fake obligations"
      | `Actual obligations_repo ->
        { Stable_format.
          obligations_repo
        ; by_path = Hashtbl.to_alist by_path
        }
    ;;

    let to_model { Stable_format.
                   obligations_repo
                 ; by_path
                 } =
      { obligations_repo = `Actual obligations_repo
      ; by_path = Path_in_repo.Table.of_alist_exn by_path
      }
    ;;

    include Stable_format
  end
end
