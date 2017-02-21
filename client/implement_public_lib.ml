open! Core
open! Async
open! Import

let rpc_implementations = ref []

let implement
      (type action) (type reaction)
      m
      f =
  let module M = (val m : Iron_command_rpc.S
                  with type action   = action
                   and type reaction = reaction)
  in
  let module T_conv = struct
    include M

    let implementation _invocation query =
      match%map try_with (fun () -> f query) with
      | Ok reaction -> reaction
      | Error exn ->
        raise_s
          [%sexp
            "iron public rpc error"
          , ((M.name, Monitor.extract_exn exn) : string * exn)
          ]
    ;;
  end in
  rpc_implementations :=
    (module T_conv : Command_rpc.Command.T_conv) :: !rpc_implementations
;;

let () = implement (module Fe.Archive)                 Cmd_archive.                main
let () = implement (module Fe.Compress)                Cmd_compress.               main
let () = implement (module Fe.Create)                  Cmd_create.                 main
let () = implement (module Fe.Feature_table_of_csv)    Cmd_feature_table_of_csv.   main
let () = implement (module Fe.List.Table)              Cmd_list.Table.             main
let () = implement (module Fe.Lock)                    Cmd_lock.                   main
let () = implement (module Fe.Obligations.List_groups) Cmd_obligations.List_groups.main
let () = implement (module Fe.Obligations.List_users)  Cmd_obligations.List_users. main
let () = implement (module Fe.Rebase)                  Cmd_rebase.                 main
let () = implement (module Fe.Release)                 Cmd_release.                main
let () = implement (module Fe.Rename)                  Cmd_rename.                 main
let () = implement (module Fe.Unarchive)               Cmd_unarchive.              main
let () = implement (module Fe.Unlock)                  Cmd_unlock.                 main
let () = implement (module Fe.Update)                  Cmd_update.                 main
let () = implement (module Fe.Wait_for_hydra)          Cmd_wait_for_hydra.         main

let () =
  implement (module Fe.Supported_command_rpcs) (fun () ->
    List.concat_map !rpc_implementations ~f:(fun (module T) ->
      let name = T.name in
      List.map (Set.to_list (T.versions ())) ~f:(fun version ->
        Iron_command_rpc.find_rpc_exn ~name ~version))
    |> return)
;;

let rpc_implementations : Command_rpc.Command.t list =
  List.map !rpc_implementations ~f:(fun t_conv -> `Plain_conv t_conv)
;;
