open! Core
open! Async
open! Import

let invalid_users_table invalid_users =
  let rows = Map.to_alist invalid_users in
  let columns =
    Ascii_table.Column.(
      [ string ~header:"invalid user"
          (cell (fun (name, _) -> User_name.to_string name))
      ; string ~header:"occurs as"
          (cell (fun (_, sets_of_origin) ->
             List.map sets_of_origin ~f:User_name_occurrence.to_string_hum
             |> List.sort ~cmp:String.compare
             |> String.concat ~sep:", "))
      ])
  in
  Ascii_table.create ~columns ~rows
;;

let command =
  Command.async'
    ~summary:"output invalid users"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and display_ascii = display_ascii
     and () = interactive
     and max_output_columns = max_output_columns
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map invalid_users = Get_invalid_users.rpc_to_server_exn () in
       let invalid_users_table = invalid_users_table invalid_users in
       if not (Ascii_table.is_empty invalid_users_table)
       then print_string
              (Ascii_table.to_string invalid_users_table ~display_ascii
                 ~max_output_columns)
    )
;;
