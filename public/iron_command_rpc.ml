open! Core
open! Async.Std
open! Import

include Iron_command_rpc_intf

let verbose = Verbose.command_rpc

module Command_rpc_info = struct
  type t =
    { name             : string
    ; versions         : unit -> Int.Set.t
    }
end

let rpcs_ref = ref []

let rpcs = lazy !rpcs_ref

let rpc_descriptions =
  lazy (
    List.concat_map (force rpcs) ~f:(fun { Command_rpc_info. name; versions } ->
      List.map (Set.to_list (versions ())) ~f:(fun version ->
        { Rpc.Description. name; version })))
;;

module Make
    (Name     : Name)
    (Version  : Version)
    (Action   : Action)
    (Reaction : Reaction)
= struct

  let ensure_rpcs_not_forced ~while_calling =
    if Lazy.is_val rpcs
    then failwithf "cannot call [%s] after [rpcs] has been forced" while_calling ()
  ;;

  let () = ensure_rpcs_not_forced ~while_calling:"Iron_command_rpc.Make"

  module Model = struct
    include Name
    type action   = Action.t    [@@deriving bin_io, sexp]
    type reaction = Reaction.t  [@@deriving bin_io, sexp_of]
    type query    = Action.t    [@@deriving bin_io, sexp]
    type response = Reaction.t  [@@deriving bin_io, sexp_of]
  end

  include Model

  module Both_convert =
    Versioned_rpc.Both_convert.Plain.Make (struct
      include Name
      module Caller = Model
      module Callee = Model
    end)

  let map_reaction_in_client = Set_once.create ()

  module Register_map_reaction_in_client
      (Map_reaction_in_client : Map_reaction_in_client
       with type action   := Action.t
        and type reaction := Reaction.t)
  = struct
    let () =
      match
        Set_once.set map_reaction_in_client Map_reaction_in_client.of_command_reaction
      with
      | Ok () -> ()
      | Error str ->
        failwiths "map_reaction_in_client cannot be registered multiple time"
          (Name.name, str) [%sexp_of: string * string];
    ;;
  end

  let command_rpc ?connection:(how_to_connect = `Default) action =
    print_elapsed [%here];
    let query = action in
    if verbose
    then Verbose.message "command rpc query" (name, query) [%sexp_of: string * query];
    let%map response_or_error =
      let { Connection.Command_rpc.Process. prog; args } =
        Connection.Command_rpc.process how_to_connect
      in
      Command_rpc.Connection.with_close ~prog ~args (fun connection ->
        Versioned_rpc.Connection_with_menu.create connection
        >>=? fun connection ->
        Both_convert.dispatch_multi connection query)
    in
    print_elapsed [%here];
    if verbose
    then Verbose.message "command rpc response" (name, response_or_error)
           [%sexp_of: string * response Or_error.t];
    match Set_once.get map_reaction_in_client with
    | None -> response_or_error
    | Some of_command_reaction ->
      Or_error.map response_or_error ~f:(fun reaction ->
        let reaction = of_command_reaction action reaction in
        if verbose
        then Verbose.message "command rpc response -- map in client" (name, Ok reaction)
               [%sexp_of: string * response Or_error.t];
        reaction)
  ;;

  let () =
    let versions = Both_convert.versions in
    rpcs_ref := { Command_rpc_info. name; versions } :: !rpcs_ref;
  ;;

  include Both_convert.Register (struct
      include Model
      include Version
      let callee_model_of_query    = Fn.id
      let response_of_callee_model = Fn.id
      let caller_model_of_response = Fn.id
      let query_of_caller_model    = Fn.id
    end)

  module Register_old_rpc
      (Version : sig val version : int end)
      (Action   : Old_action   with type model := Action.t)
      (Reaction : Old_reaction with type model := Reaction.t)
  = struct
    let () =
      ensure_rpcs_not_forced ~while_calling:"Iron_command_rpc.Make.Register_old_rpc"
    ;;

    include
      Both_convert.Register (struct
        include Version
        type query = Action.t [@@deriving bin_io]
        type response = Reaction.t [@@deriving bin_io]
        let callee_model_of_query    = Action.to_model
        let response_of_callee_model = Reaction.of_model
        let unsupported () =
          failwiths "it is not supported to use a new client with an old Iron server"
            Name.name [%sexp_of: string]
        ;;
        let query_of_caller_model    _ = unsupported ()
        let caller_model_of_response _ = unsupported ()
      end)
  end

  module Register_old_rpc_converting_both_ways
      (Version : sig val version : int end)
      (Action   : Old_action_converting_both_ways   with type model := Action.t)
      (Reaction : Old_reaction_converting_both_ways with type model := Reaction.t)
  = struct
    let () =
      ensure_rpcs_not_forced
        ~while_calling:"Iron_command_rpc.Make.Register_old_rpc_converting_both_ways"
    ;;

    include
      Both_convert.Register (struct
        include Version
        type query = Action.t [@@deriving bin_io]
        type response = Reaction.t [@@deriving bin_io]
        let callee_model_of_query    = Action.to_model
        let query_of_caller_model    = Action.of_model
        let response_of_callee_model = Reaction.of_model
        let caller_model_of_response = Reaction.to_model
      end)
  end

  include Both_convert
  type caller_query    = action
  type caller_response = reaction
  type callee_query    = action
  type callee_response = reaction
end
