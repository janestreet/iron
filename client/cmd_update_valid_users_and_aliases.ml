open Core
open Async
open Import

module User_aliases = Update_valid_users_and_aliases.User_aliases

(* This type agrees with the output of the call to [jadmin corpdir query] below. *)
type corpdir_user =
  { username : User_name.t
  ; alias    : Alternate_name.t sexp_list
  }
[@@deriving of_sexp]

let command =
  Command.async'
    ~summary:"update Iron's mapping from aliases to user names, \
              and the set of valid user names."
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and read_from_stdin =
       no_arg_flag "-stdin" ~doc:"read corpdir cache input from stdin"
     and may_repartition_crs = may_repartition_crs
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind input =
         if read_from_stdin
         then Reader.file_contents "/dev/stdin"
         else (
           let%map process =
             Process.run
               ~prog:"jadmin"
               ~args:[ "corpdir"; "query"; "-sexp"; "-attr"; "username"; "-attr"; "alias"]
               ()
           in
           process |> ok_exn)
       in
       let corpdir_user_list =
         Sexp.list_of_string_conv_exn input [%of_sexp: corpdir_user list]
       in
       let valid_users_and_aliases =
         List.map corpdir_user_list ~f:(fun { username; alias } ->
           { User_aliases. user_name = username; aliases = alias })
       in
       Update_valid_users_and_aliases.rpc_to_server_exn
         { valid_users_and_aliases; may_repartition_crs }
    )
;;
