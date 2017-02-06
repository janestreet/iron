open! Core
open! Async
open! Import

module Tagged_file = struct
  type t =
    { file_name : File_name.t
    ; tags      : Tag.Set.t
    ; lines     : string list
    }
  [@@ deriving fields, sexp_of]

  let compare_by_file_name t1 t2 = File_name.compare t1.file_name t2.file_name
  ;;

  let create ~file_name ~tags ~lines =
    { file_name
    ; tags
    ; lines
    }
  ;;

  let header t =
    List.find t.lines ~f:(fun line -> not (String.is_empty line))
    |> Option.value ~default:""
  ;;
end

module Project = struct
  type t =
    { enclosing_dir : Path_in_repo.t
    ; tagged_files  : Tagged_file.t list
    ; all_tags      : Tag.Set.t
    }
  [@@deriving fields, sexp_of]

  let create ~enclosing_dir ~tagged_files =
    { enclosing_dir
    ; tagged_files = List.sort tagged_files ~cmp:Tagged_file.compare_by_file_name
    ; all_tags     = Tag.Set.union_list (List.map tagged_files ~f:Tagged_file.tags)
    }
  ;;

  let alphabetic_compare t1 t2 =
    Path_in_repo.default_review_compare t1.enclosing_dir t2.enclosing_dir
  ;;
end

type t =
  { projects_by_dir : Project.t Path_in_repo.Table.t
  ; obligations     : Obligations.t
  }
[@@deriving sexp_of]

let load repo_root : t Deferred.t =
  let below = Repo_root.relativize_exn repo_root Abspath.program_started_in in
  let%bind obligations =
    Cmd_obligations.create_obligations repo_root ~dirs:(`Below below)
      ~skip_full_repo_checks:()
  in
  let read_table = Path_in_repo.Table.create () in
  let read_throttle = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:20 in
  Hashtbl.iteri obligations.by_path ~f:(fun ~key:path_in_repo ~data:review_attributes ->
    let tags = review_attributes.tags in
    if not (Set.is_empty tags)
    then (
      let dir, file_name = Path_in_repo.split_dir_file_exn path_in_repo in
      Hashtbl.add_multi read_table
        ~key:dir
        ~data:(Throttle.enqueue read_throttle (fun () ->
          let%map lines =
            Reader.file_lines
              (Repo_root.append repo_root path_in_repo |> Abspath.to_string)
          in
          Tagged_file.create ~file_name ~tags ~lines
        ))));
  let%map alist =
    Hashtbl.to_alist read_table
    |> Deferred.List.map ~f:(fun (enclosing_dir, tagged_files) ->
      let%map tagged_files = Deferred.List.map tagged_files ~f:Fn.id in
      enclosing_dir, Project.create ~enclosing_dir ~tagged_files)
  in
  { projects_by_dir = Path_in_repo.Table.of_alist_exn alist
  ; obligations
  }
;;

let known_tags_exn repo_root =
  let%map obligations, _ =
    Obligations_repo.load (module Hg) repo_root
      ~aliases:User_name_by_alternate_name.not_available
    >>| ok_exn
  in
  obligations.tags
;;

let tag_blang_arg = Command.Param.blang_arg_type_with_completion (module Tag) (fun () ->
  match Thread_safe.block_on_async (fun () ->
    known_tags_exn (ok_exn Repo_root.program_started_in))
  with
  | Ok tags -> Ok (Set.to_list tags)
  | Error exn -> Or_error.error "cannot load obligations_repo.sexp"
                   exn [%sexp_of: exn])
;;

let list =
  let show_readme_switch = "-show-readme" in
  let directory_only_switch = "-directory-only" in
  Command.async'
    ~summary:"list projects found in the repo"
    ~readme:(fun () -> "\
List projects.  Restrict to projects satisfying a given tag blang expression.")
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and max_output_columns = max_output_columns
     and directory_only =
       no_arg_flag directory_only_switch
         ~doc:"only output the directories of the projects"
     and show_readme =
       no_arg_flag show_readme_switch
         ~doc:"include the entire README file in the output"
     and tags =
       flag "-tag" (optional tag_blang_arg)
         ~doc:"(TAG-BLANG) restrict to projects satisfying a tag blang expression"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       if directory_only && show_readme
       then failwithf "The flags [%s] and [%s] are mutually exclusive"
              directory_only_switch show_readme_switch ();
       let repo_root = ok_exn Repo_root.program_started_in in
       let%bind t = load repo_root in
       let undefined_tags =
         match tags, t.obligations.obligations_repo with
         | None, _
         | Some _, `Fake -> []
         | Some tags, `Actual obligations_repo ->
           let known_tags = obligations_repo.tags in
           List.filter (Blang.to_list tags) ~f:(fun tag -> not (Set.mem known_tags tag))
       in
       let undefined_tags = Tag.Set.of_list undefined_tags in
       if not (Set.is_empty undefined_tags)
       then raise_s [%sexp "undefined tags", (undefined_tags : Tag.Set.t)];
       let include_project =
         match tags with
         | None -> const true
         | Some tags ->
           (fun (project : Project.t) ->
              Blang.eval tags (fun tag -> Set.mem project.all_tags tag))
       in
       let projects =
         t.projects_by_dir
         |> Hashtbl.data
         |> List.filter ~f:include_project
         |> List.sort ~cmp:Project.alphabetic_compare
         |> List.map ~f:(fun project ->
           Path_in_repo.to_string project.enclosing_dir, project)
       in
       let max_name_length =
         List.map projects ~f:(fun (name, _) -> String.length name)
         |> List.max_elt ~cmp:Int.compare
         |> Option.value ~default:0
       in
       List.iter projects ~f:(fun (name, project) ->
         if show_readme
         then List.iter project.tagged_files ~f:(fun tagged_file ->
           let title =
             sprintf "%s/%s:" name
               (File_name.to_string tagged_file.file_name)
           in
           print_endline
             (sprintf "%s (tags %s)\n%s\n%s\n"
                title
                (concat ~sep:", "
                   (List.map (Set.to_list tagged_file.tags) ~f:Tag.to_string))
                (String.make (String.length title) '-')
                (concat ~sep:"\n" (List.map tagged_file.lines ~f:(fun s -> "  " ^ s)))))
         else
           print_endline
             (if directory_only
              then name
              else
                String.prefix (sprintf "%-*s  %s" max_name_length name
                                 (Tagged_file.header (List.hd_exn project.tagged_files)))
                  max_output_columns));
       return ()
    )
;;

let search =
  Command.async'
    ~summary:"search for a regexp in the project tags and README contents"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and regex = anon ("REGEXP" %: (Arg_type.create Regex.create_exn))
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let repo_root = ok_exn Repo_root.program_started_in in
       let%map t = load repo_root in
       let matching_files =
         Hashtbl.data t.projects_by_dir
         |> List.concat_map ~f:(fun project ->
           List.filter_map project.tagged_files ~f:(fun tagged_file ->
             if Set.exists tagged_file.tags
                  ~f:(fun tag -> Regex.matches regex (Tag.to_string tag))
             || List.exists tagged_file.lines ~f:(Regex.matches regex)
             then Some (Path_in_repo.extend project.enclosing_dir tagged_file.file_name)
             else None))
         |> List.sort ~cmp:Path_in_repo.default_review_compare
       in
       if List.is_empty matching_files
       then raise_s [%sexp "no projects matching regex", (regex : Regex.t)]
       else
         List.iter matching_files ~f:(fun path_in_repo ->
           print_endline (Path_in_repo.to_string path_in_repo))
    )
;;

let list_tags =
  Command.async'
    ~summary:"output all known tags, one per line"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map tags = known_tags_exn (ok_exn Repo_root.program_started_in) in
       Set.iter tags ~f:(fun tag -> print_endline (Tag.to_string tag))
    )
;;

let command =
  Command.group ~summary: "handle tagged files and projects directories"
    [ "list"      , list
    ; "list-tags" , list_tags
    ; "search"    , search
    ]
;;
