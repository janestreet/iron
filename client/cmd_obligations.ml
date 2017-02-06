open! Core
open! Async
open! Import

let create_obligations
      ?skip_full_repo_checks
      ?(resolve_aliases = false)
      repo_root ~dirs =
  let%bind (manifest, aliases) =
    Deferred.both
      (Hg.manifest repo_root `Dirstate)
      (if resolve_aliases
       then Get_alternate_names.rpc_to_server_exn `Aliases
       else return User_name_by_alternate_name.not_available)
  in
  let%map (t, _cr_format) =
    Obligations.create (module Hg) ~repo_root ~dirs ~manifest
      ?skip_full_repo_checks ~aliases ()
  in
  ok_exn t
;;

let check =
  Command.async'
    ~summary:"check obligations of the current subtree"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       let repo_root = ok_exn Repo_root.program_started_in in
       let below = Repo_root.relativize_exn repo_root Abspath.program_started_in in
       let%map (_ : Obligations.t) =
         create_obligations repo_root ~dirs:(`Below below)
       in
       ()
    )
;;

let list_projections =
  Command.async'
    ~summary:"output all build-projection names, one per line"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       let repo_root = ok_exn Repo_root.program_started_in in
       let%map obligations =
         Obligations_repo.load (module Hg) repo_root
           ~aliases:User_name_by_alternate_name.not_available
       in
       let { Obligations_repo. build_projections; _ }, _ = ok_exn obligations in
       List.iter (Map.keys build_projections) ~f:(fun build_projection_name ->
         print_endline (Build_projection_name.to_string build_projection_name));
    )
;;

let load_obligations { Fe.Obligations.Which_obligations.
                       repo_root
                     ; file_tree_of
                     ; aliases_resolution
                     } =
  let repo_root = Repo_root.of_abspath repo_root in
  let%bind aliases =
    match aliases_resolution with
    | `None -> return User_name_by_alternate_name.not_available
    | `Using_latest_aliases_from_iron_server ->
      Get_alternate_names.rpc_to_server_exn `Aliases
  in
  let load repo_root =
    let%map obligations, _ =
      Obligations_repo.load (module Hg) repo_root ~aliases
      >>| ok_exn
    in
    obligations
  in
  match file_tree_of with
  | `Working_copy -> load repo_root
  | `Rev raw_rev  ->
    let%bind rev = Raw_rev.resolve_exn raw_rev ~in_:(Ok repo_root) in
    Hg.with_temp_share repo_root ~f:(fun repo_root ->
      let%bind () = Hg.update ~clean_after_update:No repo_root (`Rev rev) in
      load repo_root
    )
;;

let which_obligations_param =
  let open Command.Let_syntax in
  let%map_open () = return ()
  and file_tree_of =
    flag "-rev" (optional rev_arg_type)
      ~doc:"REV load the obligations at REV (default: working directory)"
    |> map ~f:(function
      | None     -> `Working_copy
      | Some rev -> `Rev rev)
  and aliases_resolution =
    no_arg_flag "-resolve-aliases"
      ~doc:" resolve user names using latest aliases from iron server"
    |> map ~f:(fun de_alias ->
      if de_alias
      then `Using_latest_aliases_from_iron_server
      else `None)
  in
  { Fe.Obligations.Which_obligations.
    repo_root = Repo_root.program_started_in |> ok_exn |> Repo_root.to_abspath
  ; file_tree_of
  ; aliases_resolution
  }
;;

module List_users = struct
  let main which_obligations =
    let%map obligations = load_obligations which_obligations in
    obligations.users
  ;;
end

let list_users =
  Command.async'
    ~summary:"output all users defined in obligations files, one per line"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and which_obligations = which_obligations_param in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map users = List_users.main which_obligations in
       Set.iter users ~f:(fun user -> print_endline (Unresolved_name.to_string user))
    )
;;

module List_groups = struct
  let main which_obligations =
    let%map obligations = load_obligations which_obligations in
    obligations.groups
  ;;
end

let list_groups =
  Command.async'
    ~summary:"output all groups defined in obligations files, one per line"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and which_obligations = which_obligations_param in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map groups = List_groups.main which_obligations in
       groups
       |> Map.keys
       |> List.iter ~f:(fun user -> print_endline (Group_name.to_string user))
    )
;;

let list_users_in_groups =
  let group_blang_arg =
    Command.Param.blang_arg_type_with_completion (module Group_name)
      (fun () ->
         match Thread_safe.block_on_async (fun () ->
           let repo_root = Repo_root.program_started_in |> ok_exn in
           List_groups.main
             { repo_root          = Repo_root.to_abspath repo_root
             ; file_tree_of       = `Working_copy
             ; aliases_resolution = `None
             })
         with
         | Ok groups -> Ok (Map.keys groups)
         | Error exn -> Or_error.error "cannot load obligations_repo.sexp"
                          exn [%sexp_of: exn])
  in
  Command.async'
    ~summary:"output all users verifying a group blang, one per line"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and which_obligations = which_obligations_param
     and group_blang = anon ("<GROUP-BLANG>" %: group_blang_arg)
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map groups = List_groups.main which_obligations in
       Obligations_groups.get_users_exn groups group_blang
       |> Set.iter ~f:(fun user -> print_endline (Unresolved_name.to_string user))
    )
;;

let allow_review_for =
  Command.async'
    ~summary:"output the permission settings for review done by others"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and which_obligations = which_obligations_param
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map obligations = load_obligations which_obligations in
       print_endline (Sexp.to_string_hum
                        [%sexp (obligations.allow_review_for : Allow_review_for.t)]))
;;

let projection =
  Command.async'
    ~summary:"output build projection(s)"
    ~readme:(fun () -> "\
Output union of build projections, one file per line, sorted alphabetically.")
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and projections = projections
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let repo_root = ok_exn Repo_root.program_started_in in
       (* We skip full-repo checks, since jenga needs to run it after a repo has been
          projected, and full-repo checks don't pass after projection. *)
       let%bind obligations =
         create_obligations repo_root ~dirs:`All ~skip_full_repo_checks:()
       in
       let undefined_projections =
         match obligations.obligations_repo with
         | `Fake -> projections
         | `Actual obligations_repo ->
           List.filter projections ~f:(fun projection ->
             not (Map.mem obligations_repo.build_projections projection))
       in
       if not (List.is_empty undefined_projections)
       then
         raise_s
           [%sexp "undefined projections"
                , (undefined_projections : Build_projection_name.t list)
           ];
       obligations.by_path
       |> Hashtbl.to_alist
       |> List.filter_map ~f:(fun (path_in_repo, review_attributes) ->
         if List.exists projections ~f:(fun projection ->
           Set.mem review_attributes.build_projections projection)
         then Some path_in_repo
         else None)
       |> List.map ~f:Path_in_repo.to_string
       |> List.sort ~cmp:String.alphabetic_compare
       |> List.iter ~f:print_endline;
       return ()
    )
;;

let report =
  Command.async'
    ~summary:"output attributes of all files in the current subtree"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and resolve_aliases =
       no_arg_flag "resolve-aliases"
         ~doc:"query server for aliases and resolve them in computed report"
     and stable =
       no_arg_flag "-stable"
         ~doc:"output the values using stable sexp types to repart them later"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let repo_root = ok_exn Repo_root.program_started_in in
       let below = Repo_root.relativize_exn repo_root Abspath.program_started_in in
       let%bind obligations =
         create_obligations ~resolve_aliases repo_root ~dirs:(`Below below)
       in
       let sexp =
         if stable
         then
           obligations
           |> Obligations.sorted_files_alist
           |> [%sexp_of: ( Path_in_repo.t * Review_attributes.Stable.V2.t ) list]
         else obligations |> [%sexp_of: Obligations.t]
       in
       print_endline (Sexp.to_string_hum sexp);
       return ()
    )
;;

let show =
  Command.async'
    ~summary:"show obligations for a single file"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and path_in_repo = path_in_repo
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     and display_in_table =
       no_arg_flag "-display-in-table"
         ~doc:" display output as a table rather than a sexp"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let display_in_table =
         display_in_table
         || Client_config.(get () |> Cmd.Obligations_show.display_in_table)
       in
       let repo_root = ok_exn Repo_root.program_started_in in
       let dir =
         Option.value (Path_in_repo.parent path_in_repo) ~default:Path_in_repo.root
       in
       let%bind obligations =
         create_obligations repo_root ~dirs:(`Only_this dir)
       in
       let review_attributes =
         match Hashtbl.find obligations.by_path path_in_repo with
         | Some review_attributes -> review_attributes
         | None ->
           raise_s [%sexp "no such file in obligations", (path_in_repo : Path_in_repo.t)]
       in
       let to_print =
         if display_in_table
         then
           (review_attributes
            |> Review_attributes.attribute_table
            |> Ascii_table.to_string ~display_ascii ~max_output_columns)
         else
           (review_attributes
            |> [%sexp_of: Review_attributes.t]
            |> Sexp.to_string_hum)
       in
       printf "%s\n" to_print;
       return ()
    )
;;

let update_low_review_files =
  Command.async'
    ~summary:"update .fe/low-review-in-* files"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       let repo_root = ok_exn Repo_root.program_started_in in
       let%bind obligations =
         create_obligations repo_root ~dirs:`All ~skip_full_repo_checks:()
       in
       Deferred.List.iter ~how:(`Max_concurrent_jobs 30)
         (Map.to_alist (Obligations.low_review_files obligations))
         ~f:(fun (build_projection_name, low_review) ->
           Writer.save
             (Abspath.to_string
                (Repo_root.append repo_root
                   (Path_in_repo.low_review_file build_projection_name)))
             ~contents:
               (concat
                  [ "\
;; This file was generated by:
;;
;;   fe obligations update-low-review-files
;;
;; If you get an error about [missing low-review files] or [files are not low review],
;; then you can run the above command to generate valid low-review-* files in this repo.
;; But in the case of [missing low-review files], you should probably adjust .fe.sexp
;; files so that the missing files have the desired scrutiny and enough reviewers.
"
                  ; low_review
                    |> Set.to_list
                    |> List.map ~f:(fun path_in_repo ->
                      concat [ Path_in_repo.to_string path_in_repo; "\n" ])
                    |> String.concat
                  ]))
    )
;;

let command =
  Command.group ~summary: "process Iron obligations files"
    [ "allow-review-for"        , allow_review_for
    ; "check"                   , check
    ; "list-groups"             , list_groups
    ; "list-projections"        , list_projections
    ; "list-users"              , list_users
    ; "list-users-in-groups"    , list_users_in_groups
    ; "projection"              , projection
    ; "report"                  , report
    ; "show"                    , show
    ; "update-low-review-files" , update_low_review_files
    ]
;;
