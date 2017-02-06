open! Core
open! Async
open! Import

let dump action =
  let%map sexp = Dump.rpc_to_server_exn action in
  printf "%s\n" (Sexp.to_string_hum sexp)
;;

let bookmarks_without_feature =
  Command.async' ~summary:"output bookmarks that don't have a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and remote_repo_path = remote_repo_path
     and user             = for_or_all_default_me
     in
     fun () ->
       dump (Bookmarks_without_feature (remote_repo_path, user))
    )
;;

let feature =
  Command.async' ~summary:"output a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     in
     fun () ->
       dump (Feature (ok_exn feature_path))
    )
;;

let hash_consing_cache =
  let values_switch        = "-values"        in
  let module_values_switch = "-module-values" in
  let hash_data_switch     = "-hash-data"     in
  Command.async' ~summary:"output stats or values of the hash-consing cache"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and what_to_dump =
       let%map values =
         no_arg_flag values_switch
           ~doc:"output the whole values instead of the stats"
       and module_values =
         flag module_values_switch (optional string)
           ~doc:"module_name output the whole values for a particular module"
       and hash_data =
         flag hash_data_switch (optional string)
           ~doc:"module_name values and hash data for a particular module"
       in
       Or_error.try_with (fun () ->
         match values, module_values, hash_data with
         | false, None, None             -> `Stats
         | true , None, None             -> `Values
         | false, Some module_name, None -> `Module_values    module_name
         | false, None, Some module_name -> `Module_hash_data module_name
         | _ ->
           failwithf "The flags [%s], [%s], and [%s] are mutually exclusive"
             values_switch module_values_switch hash_data_switch ()
       )
     in
     fun () ->
       dump (Hash_consing_cache (ok_exn what_to_dump))
    )
;;

let review_analysis =
  Command.async' ~summary:"output the review analysis of a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     in
     fun () ->
       dump (Review_analysis (ok_exn feature_path))
    )
;;

let review_lines =
  Command.async' ~summary:"output details of review lines"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and for_or_all   = for_or_all_default_me
     in
     fun () ->
       dump (Review_lines (ok_exn feature_path, for_or_all))
    )
;;

let review_manager =
  Command.async' ~summary:"output a review manager"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and for_or_all   = for_or_all_default_me
     in
     fun () ->
       dump (Review_manager (ok_exn feature_path, for_or_all))
    )
;;

let state =
  Command.async' ~summary:"output the entire server state"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       dump State
    )
;;

let user_info =
  Command.group ~summary:"output user info"
    (List.map ~f:(fun (name, which, description) ->
       name,
       Command.async' ~summary:(sprintf "output %s" description)
         (let open Command.Let_syntax in
          let%map_open () = return () in
          fun () -> dump (User_info which)
         ))
       (let aliases = Dump.Which_user_info.Aliases in
        [ "aliases"       , aliases       , "map from alias to user name"
        ; "existing-users", Existing_users, "existing user names"
        ; "typos"         , Typos         , "map from typo to user name"
        ; "valid-users"   , Valid_users   , "valid user names"
        ]))
;;

let command =
  Command.group ~summary:"output part of server internal state as a sexp"
    [ "bookmarks-without-feature", bookmarks_without_feature
    ; "feature"                  , feature
    ; "hash-consing-cache"       , hash_consing_cache
    ; "review-analysis"          , review_analysis
    ; "review-lines"             , review_lines
    ; "review-manager"           , review_manager
    ; "state"                    , state
    ; "user-info"                , user_info
    ]
;;
