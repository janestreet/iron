open! Core
open! Async
open! Import

let supported_by_client =
  Command.async' ~summary:"output a table of RPCs supported by this executable"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     in
     fun () ->
       print_endline
         (Ascii_table.to_string
            (Rpc_description.to_ascii_table (force Iron_versioned_rpc.rpc_descriptions))
            ~display_ascii
            ~max_output_columns);
       return ()
    )
;;

let supported_by_server =
  Command.async' ~summary:"output a table of RPCs supported by Iron server"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map descriptions = Supported_rpcs.rpc_to_server_exn () in
       print_endline
         (Ascii_table.to_string
            (Rpc_description.to_ascii_table descriptions)
            ~display_ascii
            ~max_output_columns)
    )
;;

let command =
  Command.group ~summary:"dealing with RPCs"
    [ "call"               , force Iron_versioned_rpc.command
    ; "supported-by-client", supported_by_client
    ; "supported-by-server", supported_by_server
    ]
;;
