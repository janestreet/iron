open! Core
open! Async
open! Import
open! Iron_common.Std

include Iron_command_rpc_intf

let verbose = Verbose.command_rpc

let rpc_summary      = Rpc_summary.create ()
let rpc_descriptions = Rpc_summary.rpc_descriptions rpc_summary
let find_rpc_exn     = Rpc_summary.find_rpc_exn     rpc_summary

module Make
    (Name     : Name)
    (Version  : Version)
    (Action   : Action)
    (Reaction : Reaction)
= struct

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
        Set_once.set map_reaction_in_client [%here]
          Map_reaction_in_client.of_command_reaction
      with
      | Ok () -> ()
      | Error error ->
        raise_s [%sexp "map_reaction_in_client cannot be registered multiple time"
                     , (Name.name : string), (error : Error.t)];
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

  include Both_convert.Register (struct
      include Model
      include Version
      let callee_model_of_query    = Fn.id
      let response_of_callee_model = Fn.id
      let caller_model_of_response = Fn.id
      let query_of_caller_model    = Fn.id
    end)

  let () =
    Rpc_summary.register
      rpc_summary
      ~name
      ~version:Version.version
      ~query:[%bin_shape: Model.query]
      ~response:[%bin_shape: Model.response]
      ~metadata:()
  ;;

  module Register_old_rpc
      (Version : sig val version : int end)
      (Action   : Old_action   with type model := Action.t)
      (Reaction : Old_reaction with type model := Reaction.t)
  = struct

    module Old_rpc = struct
      include Version
      type query = Action.t [@@deriving bin_io]
      type response = Reaction.t [@@deriving bin_io]
      let callee_model_of_query    = Action.to_model
      let response_of_callee_model = Reaction.of_model
      let unsupported () =
        raise_s [%sexp "it is not supported to use a new client with an old Iron server"
                     , (Name.name : string)]
      ;;
      let query_of_caller_model    _ = unsupported ()
      let caller_model_of_response _ = unsupported ()
    end

    include Both_convert.Register (Old_rpc)

    let () =
      Rpc_summary.register_old_version
        rpc_summary
        ~name
        ~version:Version.version
        ~query:[%bin_shape: Old_rpc.query]
        ~response:[%bin_shape: Old_rpc.response]
    ;;
  end

  module Register_old_rpc_converting_both_ways
      (Version : sig val version : int end)
      (Action   : Old_action_converting_both_ways   with type model := Action.t)
      (Reaction : Old_reaction_converting_both_ways with type model := Reaction.t)
  = struct

    module Old_rpc = struct
      include Version
      type query = Action.t [@@deriving bin_io]
      type response = Reaction.t [@@deriving bin_io]
      let callee_model_of_query    = Action.to_model
      let query_of_caller_model    = Action.of_model
      let response_of_callee_model = Reaction.of_model
      let caller_model_of_response = Reaction.to_model
    end

    include Both_convert.Register (Old_rpc)

    let () =
      Rpc_summary.register_old_version
        rpc_summary
        ~name
        ~version:Version.version
        ~query:[%bin_shape: Old_rpc.query]
        ~response:[%bin_shape: Old_rpc.response]
    ;;
  end

  include Both_convert
  type caller_query    = action
  type caller_response = reaction
  type callee_query    = action
  type callee_response = reaction
end
