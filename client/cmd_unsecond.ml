open! Core
open! Async
open! Import

let command =

  Command.async' ~summary:"undo a second"
    ~readme:(fun () ->
      concat [ "\
The recommended workflow is for the seconder to unsecond, should the need arise. However,
Iron allows the command to be run on behalf of the seconder, using [-for _]:

  $ fe unsecond -for USER [FEATURE]
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and for_ = for_
     in
     fun () ->
       let feature_path = ok_exn feature_path in
       Unsecond.rpc_to_server_exn { feature_path; for_ }
    )
;;
