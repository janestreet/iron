open! Core
open! Async

module Rpc_for (M : sig
    module Action   : T
    module Reaction : T
  end) = struct
  open M
  type t =
    ?connection:Connection.t
    -> Action.t
    -> Reaction.t Or_error.t Deferred.t
end

module Pipe_rpc_for (M : sig
    module Action   : T
    module Reaction : T
  end) = struct
  open M
  type t =
    ?connection:Connection.t
    -> Action.t
    -> Reaction.t Or_error.t Pipe.Reader.t Or_error.t Deferred.t
end

let command_rpc_names   = ref []
let rpc_to_server_names = ref []

let command_rpc (type action) (type reaction)
      (module M : Iron_command_rpc.S
        with type action   = action
         and type reaction = reaction)
  =
  command_rpc_names := M.name :: !command_rpc_names;
  fun ?connection action ->
    let connection = Option.map connection ~f:Connection.command_rpc in
    M.command_rpc ?connection action
;;

let rpc_to_server (type action) (type reaction)
      (module M : Iron_protocol.Iron_versioned_rpc.S
        with type action   = action
         and type reaction = reaction)
  =
  rpc_to_server_names := M.name :: !rpc_to_server_names;
  fun ?connection action ->
    let connection = Option.map connection ~f:Connection.rpc_to_server in
    M.rpc_to_server ?connection action
;;

let pipe_rpc_to_server (type action) (type reaction)
      (module M : Iron_protocol.Iron_versioned_rpc.S_pipe_rpc
        with type action   = action
         and type reaction = reaction)
  =
  rpc_to_server_names := M.name :: !rpc_to_server_names;
  fun ?connection action ->
    let connection = Option.map connection ~f:Connection.rpc_to_server in
    M.rpc_to_server ?connection action
;;

let archive                      = command_rpc   (module Archive)
let change_feature               = rpc_to_server (module Iron_protocol.Change_feature)
let compress                     = command_rpc   (module Compress)
let create                       = command_rpc   (module Create)
let enable_review                = rpc_to_server (module Iron_protocol.Enable_review)
let fact_action                  = rpc_to_server (module Iron_protocol.Fact_action)
let fact_evidence                = rpc_to_server (module Iron_protocol.Fact_evidence)
let feature_exists               = rpc_to_server (module Iron_protocol.Feature_exists)
let feature_table_of_csv         = command_rpc   (module Feature_table_of_csv)
let get_brain                    = rpc_to_server (module Iron_protocol.Get_brain)
let get_feature                  = rpc_to_server (module Iron_protocol.Get_feature)
let get_feature_by_id            = rpc_to_server (module Iron_protocol.Get_feature.By_id)
let get_feature_maybe_archived   = rpc_to_server (module Iron_protocol.Get_feature.Maybe_archived)
let list_feature_names           = rpc_to_server (module Iron_protocol.List_feature_names)
let list_features                = rpc_to_server (module Iron_protocol.List_features)
let list_obligations_groups      = command_rpc   (module Obligations.List_groups)
let list_obligations_users       = command_rpc   (module Obligations.List_users)
let list_table                   = command_rpc   (module Fe_list.Table)
let lock                         = command_rpc   (module Lock)
let notify_on_descendant_updates = pipe_rpc_to_server (module Iron_protocol.Notify_on_descendant_updates)
let notify_on_feature_updates    = pipe_rpc_to_server (module Iron_protocol.Notify_on_feature_updates)
let ping                         = rpc_to_server (module Iron_protocol.Ping)
let rebase                       = command_rpc   (module Rebase)
let release                      = command_rpc   (module Release)
let rename                       = command_rpc   (module Rename)
let revision_is_fully_reviewed   = rpc_to_server (module Iron_protocol.Revision_is_fully_reviewed)
let second                       = rpc_to_server (module Iron_protocol.Second)
let supported_command_rpcs       = command_rpc   (module Supported_command_rpcs)
let unarchive                    = command_rpc   (module Unarchive)
let unlock                       = command_rpc   (module Unlock)
let update                       = command_rpc   (module Update)
let wait_for_hydra               = command_rpc   (module Wait_for_hydra)

let command_rpc_names   = lazy (String.Set.of_list !command_rpc_names)
let rpc_to_server_names = lazy (String.Set.of_list !rpc_to_server_names)

open! Iron_common.Std
open! Iron_hg.Std

let show_supported_iron_rpcs_command =
  Command.async' ~summary:"output a table of RPCs supported by Iron public lib"
    (let open Iron_param.Let_syntax in
     let%map_open () = return ()
     and as_sexp            = flag "-sexp" no_arg ~doc:" output in sexp format"
     and display_ascii      = display_ascii
     and max_output_columns = max_output_columns
     in
     fun () ->
       let open Deferred.Let_syntax in
       let rpcs names rpcs =
         let names = force names in
         List.filter (force rpcs) ~f:(fun rpc ->
           Set.mem names (Rpc_description.name rpc))
         |> List.sort ~cmp:Rpc_description.Compare_by_name_and_version.compare
       in
       let print_as_table rpcs =
         print_endline
           (Ascii_table.to_string
              (Rpc_description.to_ascii_table rpcs)
              ~display_ascii
              ~max_output_columns)
       in
       let rpcs_to_server =
         rpcs rpc_to_server_names Iron_protocol.Iron_versioned_rpc.rpc_descriptions
       in
       let command_rpcs =
         rpcs command_rpc_names Iron_command_rpc.rpc_descriptions
       in
       if as_sexp
       then (
         [%sexp { rpcs_to_server : Rpc_description.t list
                ; command_rpcs   : Rpc_description.t list
                }]
         |> Sexp.to_string
         |> print_endline)
       else (
         print_endline "RPCs to server:";
         print_as_table rpcs_to_server;
         print_endline "Command RPCs:";
         print_as_table command_rpcs);
       return ())
;;

let show_supported_iron_rpcs =
  "show-supported-iron-rpcs", show_supported_iron_rpcs_command
;;
