open! Core
open! Import

let command =
  Command.basic'
    ~summary:"Filter color escape chars from stdin and output to stdout"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       Unix.exec () ~use_path:true
         ~prog:"sed"
         ~argv:[ "sed"
               ; "-e"; "s,\\x1B\\[[0-9;]*[a-zA-Z],,g"
               ]
       |> never_returns
    )
;;
