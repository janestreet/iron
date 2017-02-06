open Core
open Async
open Import

let set_message t =
  let target = Abspath.to_string Server_down_message.prod_path in
  Editor.with_temp_file (fun file ->
    (* It seems there is no good way to change the permissions on the sink file itself, so
       forcing the permissions of the tmp file that is being copied to be 644. *)
    let%bind () =
      Server_down_message.save_exn ~perm:0o644 t (Abspath.of_string file)
    in
    let%map output =
      Process.run ~prog:"sink"
        ~args:([ "deploy"
               ; "file"
               ; file
               ]
               @ List.map Iron_config.deploy_offices ~f:(fun office ->
                 sprintf "as-fe@sink.%s:%s" office target)
               @
               [ "-message"; "update"
               ; "-allow-plus-revisions"
               ; "-pruning-scheme"; "(keep 2)"
               ; "-verbose"
               ])
        ()
    in
    output
    |> ok_exn
    |> printf "%s\n")
;;

let rec edit_message old_message ~f =
  let%bind new_message = Editor.invoke_editor old_message in
  let new_message = ok_exn new_message in
  let rules = Pdiff4.User_config.get () |> Pdiff4.User_config.diff_config in
  print_endline (Patdiff_lib.Patdiff_core.patdiff ~rules ()
                   ~from_:{ name = "old"; text = old_message }
                   ~to_:  { name = "new"; text = new_message });
  let module Choice = Async_interactive.Choice in
  match%bind
    Async_interactive.ask_dispatch_with_help "Accept the changes?"
      [ Choice.create 'y' `Yes  "Accept the changes."
      ; Choice.create 'n' `No   "Decline the changes."
      ; Choice.create 'e' `Edit "Edit the changes"
      ]
  with
  | `Yes  -> f new_message
  | `Edit -> edit_message new_message ~f
  | `No   ->
    print_string "Aborted\n";
    return ()
;;

let edit_command =
  Command.async'
    ~summary:"edit the standard server-down message"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind old_message =
         let%map down_message =
           Server_down_message.load_exn Server_down_message.prod_path
         in
         Server_down_message.get_non_temporary_message down_message
       in
       edit_message old_message ~f:(fun new_message ->
         printf "Setting standard server-down message...\n";
         set_message (Server_down_message.create new_message))
    )
;;

let show_command =
  Command.async'
    ~summary:"show the server-down message"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map t = Server_down_message.load_exn Server_down_message.prod_path in
       printf "%s\n" (Server_down_message.message t)
    )
;;

let roll_start_command =
  Command.async'
    ~summary:"start a roll message"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and expires_in =
       flag "-expires-in" (required time_span)
         ~doc:"SPAN before reverting to the standard message"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind t = Server_down_message.load_exn Server_down_message.prod_path in
       let%bind old_message =
         match Server_down_message.get_temporary_message t with
         | Some msg -> return msg
         | None ->
           Reader.file_contents (Abspath.to_string Server_down_message.roll_etc_path)
       in
       edit_message old_message
         ~f:(fun message ->
           print_string "Setting temporary roll message...\n";
           set_message (Server_down_message.set_temporary_message t ~expires_in message))
    )
;;

let roll_end_command =
  Command.async'
    ~summary:"end a roll message"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind t = Server_down_message.load_exn Server_down_message.prod_path in
       match Server_down_message.get_temporary_message t with
       | None -> failwith "No temporary roll message to clear."
       | Some temporary_message ->
         print_endline "Current temporary message is set to:";
         print_endline "------------------------------------";
         print_endline temporary_message;
         print_endline "------------------------------------";
         let module Choice = Async_interactive.Choice in
         match%bind
           Async_interactive.ask_dispatch_with_help "Clear this temporary roll message ?"
             [ Choice.create 'y' `Yes  "Clears the message."
             ; Choice.create 'n' `No   "Abort, keep it."
             ]
         with
         | `No  ->
           print_string "Aborted\n";
           Deferred.unit
         | `Yes ->
           print_endline "Clearing temporary roll message...";
           set_message (Server_down_message.clear_temporary_message t)
    )
;;

let roll_command =
  Command.group ~summary:"roll message commands"
    [ "start", roll_start_command
    ; "end"  , roll_end_command
    ]
;;

let command =
  Command.group ~summary:"server-down message commands"
    [ "edit"  , edit_command
    ; "show"  , show_command
    ; "roll"  , roll_command
    ]
;;
