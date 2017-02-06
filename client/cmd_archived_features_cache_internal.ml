open! Core
open! Async
open! Import

let clear =
  let all_switch = "-all" in
  let id_switch  = "-id" in
  Command.async'
    ~summary:"clear archived features cache on server"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and action =
       let%map all =
         no_arg_flag all_switch ~doc:"clear all the values cached"
       and id =
         flag id_switch (optional (Arg_type.create Feature_id.of_string))
           ~doc:"clear the value cached with that id"
       in
       Or_error.try_with (fun () ->
         match all, id with
         | true , None    -> `All
         | false, Some id -> `Feature_id id
         | false, None    -> failwithf "specify either [%s] or [%s]"
                               all_switch id_switch ()
         | true , Some _  -> failwithf "the flags [%s] and [%s] are mutually exclusive"
                               all_switch id_switch ()
       )
     in
     fun () ->
       let open! Deferred.Let_syntax in
       With_archived_features_cache.rpc_to_server_exn (Clear (ok_exn action))
    )
;;

let set_max_size =
  Command.async'
    ~summary:"set max number of items allowed in the archived features cache on server"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and max_size = anon ("SIZE" %: int)
     in
     fun () ->
       let open! Deferred.Let_syntax in
       if max_size <= 0 then raise_s [%sexp "invalid max size", (max_size : int)];
       With_archived_features_cache.rpc_to_server_exn (Set_max_size max_size)
    )
;;

let show =
  let feature_paths_switch = "-feature-paths" in
  let feature_id_switch = "-id" in
  Command.async' ~summary:"show the items in the archived features cache"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and with_feature_paths =
       no_arg_flag feature_paths_switch
         ~doc:"output the ids with the feature-paths instead of the stats"
     and feature_id =
       flag feature_id_switch (optional (Arg_type.create Feature_id.of_string))
         ~doc:"output the value cached with that id"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let what_to_dump =
         match with_feature_paths, feature_id with
         | false, None    -> `Stats
         | false, Some id -> `Value id
         | true,  None    -> `Ids_and_feature_paths
         | true,  Some _  ->
           failwithf "flags [%s] and [%s] are incompatible"
             feature_paths_switch feature_id_switch ()
       in
       Cmd_dump.dump (Archived_features_cache what_to_dump)
    )
;;

let command =
  Command.group
    ~summary:"deal with the archived features cached on the server"
    ~readme:(fun () ->
      concat [ "\
Operations side affecting the cache require admin privileges." ])
    [ "clear"        , clear
    ; "set-max-size" , set_max_size
    ; "show"         , show
    ]
;;
