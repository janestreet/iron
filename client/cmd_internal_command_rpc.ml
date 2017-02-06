open! Core
open! Async
open! Import

let supported_by_iron_lib =
  Command.async' ~summary:"output a table of command RPCs supported by Iron public lib"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and display_ascii      = display_ascii
     and max_output_columns = max_output_columns
     and names_only =
       no_arg_flag "-names-only" ~doc:"output names only"
     in
     fun () ->
       let rpc_descriptions = force Iron_command_rpc.rpc_descriptions in
       (if names_only
        then (
          rpc_descriptions
          |> List.map ~f:Rpc_description.name
          |> String.Set.of_list
          |> Set.iter ~f:print_endline)
        else
          print_endline
            (Ascii_table.to_string
               (Rpc_description.to_ascii_table rpc_descriptions)
               ~display_ascii
               ~max_output_columns));
       return ()
    )
;;

let supported_by_command =
  Command.async' ~summary:"output a table of command RPCs implemented by the binary"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind rpc_descriptions = Fe.supported_command_rpcs () in
       print_endline
         (Ascii_table.to_string
            (Rpc_description.to_ascii_table (ok_exn rpc_descriptions))
            ~display_ascii
            ~max_output_columns);
       return ()
    )
;;

let referenced_by_fe_file =
  Command.async' ~summary:"output a table of command RPCs referenced by the [Fe] module \
                           in the Iron public lib"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       Set.iter (force Fe.command_rpc_names) ~f:print_endline;
       return ()
    )
;;

let call =
  Command_rpc.Command.create ~summary:"spawn local command RPC server"
    Implement_public_lib.rpc_implementations
;;

let command =
  Command.group ~summary:"RPC access for fe client"
    [ "call"                  , call
    ; "supported-by-command"  , supported_by_command
    ; "supported-by-iron-lib" , supported_by_iron_lib
    ; "referenced-by-fe-file" , referenced_by_fe_file
    ]
;;
