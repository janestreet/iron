open! Core
open! Async
open! Import

let browse_disk_quota =
  Command.basic' ~summary:"catalog browse the Iron disk quota"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let prog = "catalog" in
       never_returns
         (Unix.exec () ~prog
            ~argv:[ prog
                  ; "browse"
                  ; Fe_config.catalog_browse_path
                  ])
    )
;;

let build_info =
  Command.async' ~summary:"output Iron server executable's build info"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       Iron_client.Cmd_dump.dump Build_info
    )
;;

let last_hydra_error =
  Command.basic' ~summary:"show the last error hydra had talking to Iron server"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let prog = "bash" in
       never_returns
         (Unix.exec () ~prog
            ~argv:[ prog
                  ; "-c"
                  ; "hydra internal peek | sexp select last_error_talking_to_fe | grep -v '()'"
                  ])
    )
;;

let user_sets =
  let module User_set = Iron_protocol.User_set in
  List.map User_set.all ~f:(fun user_set ->
    Enum.to_string_hum (module User_set) user_set
  , Iron_client.Cmd_user_set.make_command user_set)
;;

let users =
  Command.group ~summary: "commands about the various sets of user names"
    ([ "define-typos"                   , Iron_client.Cmd_define_typos.command
     ; "get-invalid"                    , Iron_client.Cmd_get_invalid_users.command
     ; "refresh-existing-users"         , Iron_client.Cmd_refresh_existing_users.command
     ; "remove-aliases"                 , Iron_client.Cmd_remove_aliases.command
     ; "remove-typos"                   , Iron_client.Cmd_remove_typos.command
     ; "repartition-crs"                , Iron_client.Cmd_repartition_crs.command
     ; "update-valid-users-and-aliases" ,
       Iron_client.Cmd_update_valid_users_and_aliases.command
     ]
     @ user_sets)
;;

let version =
  Command.async' ~summary:"output Iron server executable's hg version info"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       Iron_client.Cmd_dump.dump Version
    )
;;

let command =
  Command.group ~summary:"control Iron server, deploy exe's, see disk usage, etc."
    ~readme:(fun () -> "\
Also see:

  http://docs/systems/iron/admin.html
")
    [ "browse-disk-quota"       , browse_disk_quota
    ; "deploy"                  , Deploy.deploy
    ; "deploy-check-obligations", Deploy.deploy_check_obligations
    ; "last-hydra-error"        , last_hydra_error
    ; "proxy-server"            , Iron_server.Cmd_server.Proxy.command
    ; "server"                  ,
      Command.group ~summary:"commands to poke/prod/control the server"
        ( Iron_server.Cmd_server.commands
          @ [ "build-info"  , build_info
            ; "down-message", Iron_client.Cmd_server_down_message.command
            ; "gc-compact"  , Iron_client.Cmd_gc_compact.command
            ; "rpc-stats"   , Iron_client.Cmd_rpc_stats.command
            ; "metrics"     , Iron_client.Cmd_metrics.command
            ; "serializer"  , Iron_client.Cmd_serializer.command
            ; "stat"        , Iron_client.Cmd_stat.command
            ; "uptime"      , Iron_client.Cmd_uptime.command
            ; "version"     , version
            ] )
    ; "users", users
    ]
;;
