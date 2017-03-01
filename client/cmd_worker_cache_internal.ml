open! Core
open! Async
open! Import

let clear =
  Command.async'
    ~summary:"clear worker cache on server"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and which_features =
       which_features ~allow_empty_selection:true ~default_to_current_bookmark:false ()
     and revs =
       flag "-rev" (listed rev_arg_type)
         ~doc:"REV revision(s) to be cleared in the cache"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind which_features = force which_features in
       let clear_features = function
         | Which_features.Features [] -> None
         | ( All_features | Features (_::_) ) as which_features
           -> Some (With_worker_cache.Action.Clear_features which_features)
       in
       let clear_revisions = function
         | [] -> None
         | (_::_) as revs -> Some (With_worker_cache.Action.Clear_revs revs)
       in
       let%bind revs =
         Deferred.List.map ~how:`Parallel revs ~f:(fun rev ->
           Raw_rev.resolve_exn rev ~in_:Repo_root.program_started_in)
       in
       match
         List.filter_opt [ clear_features  which_features
                         ; clear_revisions revs
                         ]
       with
       | [] ->
         failwith "it is required to precise what feature(s) or revision(s) to clear"
       | (_::_) as actions ->
         Deferred.List.iter actions ~f:With_worker_cache.rpc_to_server_exn
    )
;;

let set_max_size =
  Command.async'
    ~summary:"set max number of items allowed in the worker cache on server"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and max_size = anon ("SIZE" %: int)
     in
     fun () ->
       if max_size <= 0 then raise_s [%sexp "invalid max size", (max_size : int)];
       With_worker_cache.rpc_to_server_exn (Set_max_size max_size)
    )
;;

let set_max_items_per_rpc =
  Command.async'
    ~summary:"set max number of items allowed to be sent at once between hydra and Iron"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and max_items_per_rpc = anon ("SIZE" %: int)
     in
     fun () ->
       if max_items_per_rpc <= 0
       then raise_s [%sexp "invalid max items per rpc", (max_items_per_rpc : int)];
       With_worker_cache.rpc_to_server_exn (Set_max_items_per_rpc max_items_per_rpc)
    )
;;

let set_status =
  Command.async'
    ~summary:"change the activation status of the worker cache"
    ~readme:(fun () ->
      concat [ "\
For robustness and testing purposes the worker cache can be partially or totally disabled.
Accepted values for the STATUS are:

" ; Worker_cache.Status.doc_for_readme () ; "
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and status = enum_anon "STATUS" (module Worker_cache.Status)
     in
     fun () ->
       With_worker_cache.rpc_to_server_exn (Set_status status)
    )
;;

let show =
  let values_switch = "-values-at-rev" in
  let obligations_switch = "-obligations-at-rev" in
  let revs_switch = "-revs" in
  Command.async' ~summary:"output the worker cache"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and what_to_dump =
       let%map revs =
         no_arg_flag "-revs"
           ~doc:" dump the revs for which there are cached values"
       and values =
         flag "-values-at-rev" (optional rev_arg_type)
           ~doc:"REV dump cached values for a given revision"
       and obligations =
         flag "-obligations-at-rev" (optional rev_arg_type)
           ~doc:"REV dump cached obligations report for a given revision"
       in
       Or_error.try_with (fun () ->
         match revs, values, obligations with
         | false, None    , None     -> `Stats
         | false, Some rev, None     -> `Values_at_rev rev
         | false, None    , Some rev -> `Obligations_at_rev rev
         | true , None    , None     -> `Revs
         |  _   , _       , _        ->
           failwithf "The flags [%s], [%s] and [%s] are mutually exclusive"
             revs_switch values_switch obligations_switch ()
       )
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let resolve rev = Raw_rev.resolve_exn rev ~in_:Repo_root.program_started_in in
       let%bind what_to_dump =
         match ok_exn what_to_dump with
         | `Values_at_rev rev ->
           let%map rev = resolve rev in
           `Values_at_rev rev
         | `Obligations_at_rev rev ->
           let%map rev = resolve rev in
           `Obligations_at_rev rev
         | (`Stats | `Revs ) as what_to_dump -> return what_to_dump
       in
       Cmd_dump.dump (Worker_cache what_to_dump)
    )
;;

let analyze_obligations =
  Command.async'
    ~summary:"load and analyze multiple obligations report files"
    ~readme:(fun () ->
      concat [ "\
The command expects to be given obligations files obtained by call to the command:

  $ fe obligations report -stable

from the same repo, built at various revisions.  The files are then going to be parsed,
and transformed into the worker-cache representation on the server.  Then several stats
are given.
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     and obligations_files = anon (sequence ("obligations-report.sexp, [..]" %: file))
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let deferred_fold_map list ~init ~f =
         let%map (_, felts) =
           Deferred.List.fold ~init:(init, []) list ~f:(fun (acc, felts) elt ->
             let%map (acc, felt) = f acc elt in
             acc, (felt :: felts))
         in
         List.rev felts
       in
       let module Table_line = struct
         type t =
           { filename                   : string
           ; tree                       : File_tree_consing.Obligations.t
           ; duration_to_build_tree     : Time.Span.t
           ; number_of_dirs             : int
           ; number_of_files            : int
           ; cumulative_number_of_dirs  : int
           ; cumulative_number_of_files : int
           ; hash_consing_stats         : (string * Hash_consing.Set_stats.t) list
           ; gc_stat                    : Gc.stat
           }
         [@@deriving fields]

         module Acc = struct
           type t =
             { cumulative_number_of_dirs  : int
             ; cumulative_number_of_files : int
             }

           let init =
             { cumulative_number_of_dirs  = 0
             ; cumulative_number_of_files = 0
             }
         end
       end in
       let number_of_analyzed_files = List.length obligations_files in
       Print.printf "Will analyze obligations from %d report files\n"
         number_of_analyzed_files;
       let index = ref 0 in
       let hash_consing = Hash_consing.the_one_and_only () in
       let%bind rows =
         deferred_fold_map obligations_files ~init:Table_line.Acc.init
           ~f:(fun { cumulative_number_of_dirs; cumulative_number_of_files } filename ->
             incr index;
             let%map files =
               Reader.load_sexp_exn filename
                 [%of_sexp: ( Path_in_repo.t * Review_attributes.Stable.V2.t ) list]
             in
             let time_build_tree_start = Time.now () in
             let tree = File_tree_consing.Obligations.of_alist files in
             let duration_to_build_tree =
               Time.diff (Time.now ()) time_build_tree_start
             in
             let number_of_files = List.length files in
             let number_of_dirs =
               let dirs = Path_in_repo.Hash_set.create () in
               Hash_set.add dirs Path_in_repo.root;
               List.iter files ~f:(fun (path_in_repo, _) ->
                 let rec aux path_in_repo =
                   match Path_in_repo.parent path_in_repo with
                   | Some path_in_repo ->
                     Hash_set.add dirs path_in_repo;
                     aux path_in_repo
                   | None -> ()
                 in
                 aux path_in_repo);
               Hash_set.length dirs
             in
             let cumulative_number_of_dirs  =
               cumulative_number_of_dirs  + number_of_dirs
             in
             let cumulative_number_of_files =
               cumulative_number_of_files + number_of_files
             in
             Gc.compact ();
             Print.printf "Analyzing %s ... ( %3d / %d )\n"
               filename !index number_of_analyzed_files;
             let hash_consing_stats = Hash_consing.detailed_stats hash_consing in
             let gc_stat = Gc.stat () in
             { Table_line.Acc.
               cumulative_number_of_dirs
             ; cumulative_number_of_files
             }
           , { Table_line.
               filename
             ; tree
             ; duration_to_build_tree
             ; number_of_dirs
             ; number_of_files
             ; cumulative_number_of_dirs
             ; cumulative_number_of_files
             ; hash_consing_stats
             ; gc_stat
             })
       in
       let table =
         let hash_consing extract module_name =
           let module_name =
             Hash_consing.Module_name.module_name_without_libname module_name
           in
           Ascii_table.Column.(
             int ~header:module_name
               (cell (fun (line : Table_line.t) ->
                  match List.Assoc.find ~equal:String.equal
                          line.hash_consing_stats module_name
                  with
                  | None -> 0
                  | Some stats -> extract stats)))
         in
         let number_of_shared_entries =
           hash_consing Hash_consing.Set_stats.number_of_entries
         in
         let gc ~header extract =
           Ascii_table.Column.(
             int ~header (cell (fun line -> extract line.Table_line.gc_stat)))
         in
         Ascii_table.create
           ~columns:Ascii_table.Column.(
             [ string ~header:"name"    (cell Table_line.filename)
             ; int    ~header:"# dirs"  (cell Table_line.number_of_dirs)
             ; int    ~header:"# files" (cell Table_line.number_of_files)
             ; string ~header:"process" (cell (fun t ->
                 Table_line.duration_to_build_tree t
                 |> Time.Span.to_span
                 |> Core.Time.Span.to_string_hum))
             ; int    ~header:"dirs (total)"
                 (cell Table_line.cumulative_number_of_dirs)
             ; int    ~header:"files (total)"
                 (cell Table_line.cumulative_number_of_files)
             ; number_of_shared_entries File_tree_consing.Obligations.module_name
             ; number_of_shared_entries Review_attributes.module_name
             ; gc ~header:"live words" Gc.Stat.live_words
             ])
           ~rows
       in
       print_string
         (Ascii_table.to_string table ~display_ascii ~max_output_columns);
       print_string (sprintf "\nFinal Hash_consing state:\n%s\n"
                       (Hash_consing.stats hash_consing |> Sexp.to_string_hum));
       print_string (sprintf "\nFinal Gc state:\n%s\n"
                       (Gc.stat () |> [%sexp_of: Gc.Stat.t] |> Sexp.to_string_hum));
       return ()
    )
;;

let command =
  Command.group
    ~summary:"deal with the attributes cached by the hydra worker"
    ~readme:(fun () ->
      concat [ "\
Operations side effecting the cache require admin privileges.
"])
    [ "clear"                 , clear
    ; "analyze-obligations"   , analyze_obligations
    ; "set-max-size"          , set_max_size
    ; "set-max-items-per-rpc" , set_max_items_per_rpc
    ; "set-status"            , set_status
    ; "show"                  , show
    ]
;;
