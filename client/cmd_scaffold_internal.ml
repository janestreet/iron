open! Core
open! Async
open! Import

let print_command =
  Command.async'
    ~summary:"parse and print Iron representation of a scaffold.sexp file"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and scaffold_file = anon ("path/to/scaffold.sexp" %: resolved_file_path_arg_type)
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map scaffold = Scaffold.load_scaffold_file_exn scaffold_file in
       print_endline (scaffold
                      |> [%sexp_of: Scaffold.t]
                      |> Sexp.to_string_hum)
    )
;;

let command =
  Command.group ~summary: "miscellaneous scaffold internal commands"
    [ "print", print_command
    ]
;;
