open! Core
open! Async
open! Import

let show =
  let user_switch = "-user" in
  let values_switch = "-values" in
  Command.async'
    ~summary:"show push events"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and what_to_show =
       let%map values = no_arg_flag values_switch ~doc:"show all values"
       and feature_id = feature_id_option
       and user_name =
         flag user_switch (optional user_name) ~doc:"USER show push events of USER"
       in
       Or_error.try_with (fun () ->
         match values, feature_id, user_name with
         | false, None, None -> `Stats
         | false, Some feature_id, None ->
           `Feature_id feature_id
         | false, None, Some user_name -> `User_name user_name
         | true, None, None -> `Values
         | _  , _  , _      ->
           failwithf "The flags [%s], [%s] and [%s] are mutually exclusive"
             Switch.feature_id user_switch values_switch ())
     in
     fun () ->
       Cmd_dump.dump (Push_events (ok_exn what_to_show)))
;;

let clear =
  Command.async'
    ~summary:"clear push events"
    (let open Command.Let_syntax in
     let all_switch = "-all" in
     let%map_open () = return ()
     and what_to_clear =
       let%map revs =
         flag "-rev" (listed rev_arg_type)
           ~doc:"REV revision(s) to be cleared from state"
       and feature_ids = feature_id_list
       and users =
         flag "-user" (listed user_name)
           ~doc:"USER clear events attached to the supplied username"
       and all =
         no_arg_flag all_switch ~doc:" clear the entire state"
       in
       Or_error.try_with (fun () ->
         match revs, feature_ids, users, all with
         | []    , []    , []     , true  -> `All
         | []    , (_::_), []     , false -> `Features feature_ids
         | (_::_), []    , []     , false -> `Revs     revs
         | []    , []    , (_::_) , false -> `Users    users
         | _, _, _, _ ->
           failwithf "One should explicitly specify what to clear or supply %s"
             all_switch ()
       )
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind what_to_clear =
         match ok_exn what_to_clear with
         | `All ->
           return Push_events.Change.Action.Clear_all
         | `Features feature_ids ->
           return (Push_events.Change.Action.Clear_features feature_ids)
         | `Users user_names ->
           return (Push_events.Change.Action.Clear_users user_names)
         | `Revs revs ->
           let%map revs =
             Deferred.List.map revs ~how:(`Max_concurrent_jobs 4)
               ~f:(fun rev -> Raw_rev.resolve_exn rev ~in_:Repo_root.program_started_in)
           in
           Push_events.Change.Action.Clear_revs revs
       in
       Push_events.Change.rpc_to_server_exn what_to_clear)
;;

let set_max_size_per_feature =
  Command.async'
    ~summary:"set max number of push events per feature stored in state"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and max_size = anon ("SIZE" %: int)
     in
     fun () ->
       if max_size <= 0 then raise_s [%sexp "invalid max size", (max_size : int)];
       Push_events.Change.rpc_to_server_exn (Set_max_size_per_feature max_size))
;;

let command =
  Command.group
    ~summary:"push events stored on the server"
    ~readme:(fun () ->
      concat [ "\
Operations side effecting the push events state require admin privileges.
"])
    [ "clear"                    , clear
    ; "set-max-size-per-feature" , set_max_size_per_feature
    ; "show"                     , show
    ]
;;

