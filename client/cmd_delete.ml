open! Core
open! Import

(* Discoverability over doc indirection *)

let command =
  Command.basic'
    ~summary:"explain why we don't have (and don't want) [fe delete]"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and _ = anon (sequence ("_" %: string))
     in
     fun () ->
       failwith "\
Deleting a feature is not supported in Iron, on purpose.  This is a policy.
Use [fe archive] instead."
    )
;;
