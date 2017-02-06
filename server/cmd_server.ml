open! Core
open! Async
open! Import

let async_rpc_server t (async_rpc_port : Async_rpc_port.t) =
  let where_to_listen =
    match async_rpc_port with
    | Static port -> Tcp.on_port port
    | Dynamic _ -> Tcp.on_port_chosen_by_os
  in
  let%bind server =
    Rpc.Connection.serve
      ~implementations:State.rpc_implementations
      ~initial_connection_state:(fun _ _ -> t)
      ~on_handshake_error:(`Call (fun e ->
        Log.Global.info !"handshake error: %{sexp:exn}" e))
      ~where_to_listen
      ()
  in
  let%bind () =
    Async_rpc_port.write_if_dynamic async_rpc_port ~port:(Tcp.Server.listening_on server)
  in
  let%map () = Tcp.Server.close_finished server in
  Log.Global.error "Async rpc server stopped";
;;

let load_state ~root_directory ~server_config ~only_checking_invariants =
  (* initializing the file system is required even when just checking the invariants.
     If not, features adding new directories in export fails to check invariants. *)
  let serializer = Serializer.create ~root_directory () in
  let%bind () = State.initialize_and_sync_file_system serializer in
  let serializer =
    if only_checking_invariants
    then Serializer.do_nothing
    else serializer
  in
  let%map create_state =
    Deserializer.load State.deserializer ~root_directory ~serializer
  in
  create_state ~server_config
;;

let load_state_and_start_async_rpc_server ~basedir =
  let basedir = Abspath.of_string basedir in
  let%bind server_config = Iron_config.load_exn ~basedir in
  (match server_config.host with
   | "localhost" -> ()
   | expected_host ->
     let actual_host = Unix.gethostname () in
     [%test_result: string] actual_host ~expect:expected_host);
  let root_directory = Abspath.append basedir (Relpath.of_string "export") in
  let%bind state =
    load_state ~root_directory ~server_config ~only_checking_invariants:false
  in
  don't_wait_for (async_rpc_server state server_config.async_rpc_port);
  Deferred.never ()
;;

let load_state_and_check_invariants =
  Command.async'
    ~summary:"check a backup in a directory"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and root_directory = anon ("ROOT_DIRECTORY" %: file)
     in
     fun () ->
       let open Deferred.Let_syntax in
       let root_directory = Abspath.of_string root_directory in
       let server_config = Iron_config.for_checking_invariants in
       let%bind state =
         load_state ~root_directory ~server_config ~only_checking_invariants:true
       in
       State.invariant state;
       Deferred.unit
    )
;;

let home = Sys.getenv "HOME" |> Option.value_exn ~here:[%here]

let commands =
  App_harness.commands
    ~appname:"fe"
    ~appdir_for_doc:"$HOME"
    ~appdir:home
    ~log_format:(if am_functional_testing then `Sexp_hum else `Sexp)
    ~start_spec:Command.Spec.empty
    ~start_main:load_state_and_start_async_rpc_server
;;

module Proxy = struct

  let commands =
    App_harness.commands
      ~appname:"fe-proxy"
      ~appdir_for_doc:"$HOME/proxy"
      ~appdir:(home ^/ "proxy")
      ~log_format:(if am_functional_testing then `Sexp_hum else `Sexp)
      ~start_spec:Command.Spec.(
        empty
        +> flag "-where-to-connect" (optional (sexp_conv [%of_sexp: Host_and_port.t]))
             ~doc:"HOST_AND_PORT to connect to the real server.  default is prod"
        +> flag "-where-to-listen" (optional int)
             ~doc:"PORT specify a port where to listen.  default is same forward port"
      )
      ~start_main:(fun where_to_connect where_to_listen ~basedir:_ ->
        let%bind real_server =
          match where_to_connect with
          | Some host_and_port -> return host_and_port
          | None ->
            Iron_config.Restricted_for_rpcs.load_as_per_IRON_CONFIG
              ~may_connect_to_proxy:false ()
        in
        let where_to_listen =
          Option.value where_to_listen
            ~default:(Host_and_port.port real_server)
        in
        let%bind server =
          Rpc_proxy_server.simple_server
            ~where_to_listen:(Tcp.on_port where_to_listen)
            ~real_server
        in
        Tcp.Server.close_finished server)
  ;;

  let command =
    Command.group ~summary:"a proxy server that forwards RPCs to Iron server"
      commands
  ;;
end
