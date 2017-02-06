open! Core
open! Async
open! Import

let command ~moved_to:new_path =
  let msg =
    sprintf "This command has been moved to: [%s]"
      (String.concat ~sep:" " new_path)
  in
  Command.async'
    ~summary:"DEPRECATED"
    ~readme:(fun () -> msg)
    (let open Command.Let_syntax in
     let%map_open ( _ : string list) = anon (sequence ("IGNORED" %: string))
     in
     fun () ->
       failwith msg)
;;
