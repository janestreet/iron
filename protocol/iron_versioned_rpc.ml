open Core
open Async
open Import

include Iron_versioned_rpc_intf

let verbose = Verbose.rpc

let server_down_message () =
  Monitor.try_with_or_error (fun () ->
    match%map Clock.with_timeout (Time.Span.of_sec 3.)
                (Server_down_message.load_exn Server_down_message.prod_path)
    with
    | `Timeout -> failwith "timeout reading server down message"
    | `Result info -> Server_down_message.message info)
;;

module To_default_host_and_port = struct
  type t = { may_connect_to_proxy : bool }
  [@@deriving compare, sexp]
end

module Connect_to = struct
  module T = struct
    type t =
      | Host_and_port of Host_and_port.t
      | Default_host_and_port of To_default_host_and_port.t
    [@@deriving compare, sexp]

    let hash : t -> int = Hashtbl.hash
  end
  include T
  include Hashable.Make (T)
end

let make_fresh_connection (connect_to : Connect_to.t) =
  print_elapsed [%here];
  let%bind host_and_port =
    match connect_to with
    | Host_and_port host_and_port -> return host_and_port
    | Default_host_and_port { To_default_host_and_port. may_connect_to_proxy } ->
      Iron_config.Restricted_for_rpcs.load_as_per_IRON_CONFIG
        ~may_connect_to_proxy ()
  in
  let (host, port) = Host_and_port.tuple host_and_port in
  match%bind
    match%bind Rpc.Connection.client ~host ~port () with
    | Error exn ->
      return (Or_error.error "could not connect to Iron server"
                (Monitor.extract_exn exn) [%sexp_of: exn])
    | Ok connection ->
      match%bind
        Monitor.try_with_join_or_error ~extract_exn:true
          (fun () -> Versioned_rpc.Connection_with_menu.create connection)
      with
      | Ok v ->
        print_elapsed [%here];
        return (Ok v)
      | Error err ->
        let%map () = Rpc.Connection.close connection in
        Or_error.error
          "connected to Iron server, but could not create versioned-RPC connection"
          err [%sexp_of: Error.t]
  with
  | Ok _ as connection -> return connection
  | Error error ->
    (* When the main server is down, the proxy server is likely still up. So connection
       will succeed. However it will refused to serve the menu rpc, so we check if there
       a server down message in both cases. *)
    match%map server_down_message () with
    | Ok message -> Or_error.error_string (message ^ "\n\n" ^ Error.to_string_hum error)
    | Error _ -> Error error
;;

let get_cached_durable_connection =
  Memo.general ~hashable:Connect_to.hashable (fun connect_to ->
    let module Durable = Async_extended.Std.Durable in
    Durable.create
      ~to_create:(fun () -> make_fresh_connection connect_to)
      ~is_broken:(fun connection ->
        Rpc.Connection.is_closed
          (Versioned_rpc.Connection_with_menu.connection connection))
      ())
;;

let rpc_to_server_gen_exn
      ~may_connect_to_proxy ~how_to_connect ~dispatch_callback ~response_callback =
  ok_exn (Rpc_to_server_prevention.is_rpc_to_server_enabled ());
  print_elapsed [%here];
  let how_to_connect =
    match how_to_connect with
    | `Default -> Connect_to.Default_host_and_port { may_connect_to_proxy }
    | `Explicit host_and_port -> Connect_to.Host_and_port host_and_port
  in
  let response_or_error =
    let durable_connection = get_cached_durable_connection how_to_connect in
    print_elapsed [%here];
    let module Durable = Async_extended.Std.Durable in
    Durable.with_ durable_connection ~f:(fun connection ->
      let%map res = dispatch_callback connection in
      Or_error.tag res ~tag:"RPC to Iron server failed")
  in
  let%map response_or_error = response_or_error in
  print_elapsed [%here];
  response_callback (ok_exn response_or_error)
;;

let rpc_summary      = Rpc_summary.create ()
let rpc_descriptions = Rpc_summary.rpc_descriptions rpc_summary
let find_rpc_exn     = Rpc_summary.find_rpc_exn rpc_summary

let command =
  lazy (
    Command.async'
      ~summary:"call the given RPC using stdin for the action and stdout for the reaction"
      (let open Command.Let_syntax in
       let%map_open () = return ()
       and rpc = anon ("RPC" %: Rpc_summary.arg_type rpc_summary) in
       fun () ->
         rpc ()))

module Make
    (Name     : Name)
    (Version  : Version)
    (Action   : Action)
    (Reaction : Reaction)
= struct

  module Model = struct
    include Name
    type action   = Action.t                                             [@@deriving bin_io, sexp]
    type reaction = Reaction.t                                           [@@deriving bin_io, sexp_of]
    type query    = Action.t Query.Stable.V1.t                           [@@deriving bin_io, sexp]
    type response = Reaction.t Or_error.Stable.V1.t Response.Stable.V1.t [@@deriving bin_io, sexp_of]
  end

  include Model

  module Both_convert =
    Versioned_rpc.Both_convert.Plain.Make (struct
      include Name
      module Caller = Model
      module Callee = Model
    end)

  let implement_deferred_rpc f = Both_convert.implement_multi f

  let map_reaction_in_client = Set_once.create ()

  module Register_map_reaction_in_client
      (Map_reaction_in_client : Map_reaction_in_client
       with type action   := Action.t
        and type reaction := Reaction.t)
  = struct
    let () =
      match
        Set_once.set map_reaction_in_client [%here]
          Map_reaction_in_client.of_server_reaction
      with
      | Ok () -> ()
      | Error error ->
        raise_s [%sexp "map_reaction_in_client cannot be registered multiple time"
                     , (Name.name : string), (error : Error.t)];
    ;;
  end

  let rpc_to_server_exn ?connection:(how_to_connect = `Default) action =
    let query = Query.create action in
    if verbose
    then Verbose.message "rpc query" (name, query) [%sexp_of: string * query];
    rpc_to_server_gen_exn
      ~may_connect_to_proxy:true
      ~how_to_connect
      ~dispatch_callback:(fun connection ->
        Both_convert.dispatch_multi connection query)
      ~response_callback:(fun response ->
        if verbose
        then Verbose.message "rpc response" (name, response)
               [%sexp_of: string * response];
        let reaction = Response.reaction response |> ok_exn in
        match Set_once.get map_reaction_in_client with
        | None -> reaction
        | Some of_reaction -> of_reaction action reaction)
  ;;

  let rpc_to_server ?connection action =
    Monitor.try_with_or_error ~extract_exn:true (fun () ->
      rpc_to_server_exn ?connection action)
  ;;

  let command_line_rpc () =
    let%bind contents = Reader.file_contents "/dev/stdin" in
    let action = Sexp.of_string_conv_exn contents [%of_sexp: action] in
    match%bind rpc_to_server action with
    | Error error ->
      eprintf "%s\n" (error |> [%sexp_of: Error.t] |> Sexp.to_string_hum);
      Shutdown.exit 1
    | Ok reaction ->
      printf "%s\n" (reaction |> [%sexp_of: reaction] |> Sexp.to_string_hum);
      Deferred.unit
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
      ~metadata:command_line_rpc
  ;;

  module Register_old_rpc_converting_both_ways
      (Version : sig val version : int end)
      (Action   : Old_action_converting_both_ways   with type model := Action.t)
      (Reaction : Old_reaction_converting_both_ways with type model := Reaction.t)
  = struct

    module Old_rpc = struct
      include Version
      type query = Action.t Query.Stable.V1.t [@@deriving bin_io]
      type response =
        Reaction.t Or_error.Stable.V1.t Response.Stable.V1.t
      [@@deriving bin_io]
      let callee_model_of_query query =
        Query.Stable.V1.map query ~f:Action.to_model
      ;;
      let response_of_callee_model response =
        Response.Stable.V1.map response ~f:(Or_error.Stable.V1.map ~f:Reaction.of_model)
      ;;
      let query_of_caller_model query =
        Query.Stable.V1.map query ~f:Action.of_model
      ;;
      let caller_model_of_response response =
        Response.Stable.V1.map response ~f:(Or_error.Stable.V1.map ~f:Reaction.to_model)
      ;;
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

  module Register_old_rpc
      (Version : sig val version : int end)
      (Action   : Old_action   with type model := Action.t)
      (Reaction : Old_reaction with type model := Reaction.t)
  = struct

    module Old_rpc = struct
      include Version
      type query = Action.t Query.Stable.V1.t [@@deriving bin_io]
      type response =
        Reaction.t Or_error.Stable.V1.t Response.Stable.V1.t
      [@@deriving bin_io]
      let callee_model_of_query query =
        Query.Stable.V1.map query ~f:Action.to_model
      ;;
      let response_of_callee_model response =
        Response.Stable.V1.map response ~f:(Or_error.Stable.V1.map ~f:Reaction.of_model)
      ;;
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
end

module Make_pipe_rpc
    (Name     : Name)
    (Version  : Version)
    (Action   : Action)
    (Reaction : Reaction)
= struct

  module Model = struct
    include Name
    type action   = Action.t                      [@@deriving bin_io, sexp]
    type reaction = Reaction.t                    [@@deriving bin_io, sexp_of]
    type query    = Action.t Query.Stable.V1.t    [@@deriving bin_io, sexp]
    type response = reaction Or_error.Stable.V2.t [@@deriving bin_io, sexp_of]
    type error    = Error.Stable.V2.t             [@@deriving bin_io, sexp]
  end

  include Model

  module Both_convert =
    Versioned_rpc.Both_convert.Pipe_rpc.Make (struct
      include Name
      module Caller = Model
      module Callee = Model
    end)

  let implement_deferred_rpc f = Both_convert.implement_multi f

  let map_reaction_in_client = Set_once.create ()

  module Register_map_reaction_in_client
      (Map_reaction_in_client : Map_reaction_in_client
       with type action   := Action.t
        and type reaction := Reaction.t)
  = struct
    let () =
      match
        Set_once.set map_reaction_in_client [%here]
          Map_reaction_in_client.of_server_reaction
      with
      | Ok () -> ()
      | Error error ->
        raise_s [%sexp "map_reaction_in_client cannot be registered multiple time"
                     , (Name.name : string), (error : Error.t)];
    ;;
  end

  let rpc_to_server_exn ?connection:(how_to_connect = `Default) action =
    let query = Query.create action in
    if verbose
    then Verbose.message "rpc query" (name, query) [%sexp_of: string * query];
    (* Pipe RPCs don't use the proxy.  The proxy reduces the cost of setting up a
       connection but pipe rpcs are long running. *)
    rpc_to_server_gen_exn
      ~may_connect_to_proxy:false
      ~how_to_connect
      ~dispatch_callback:(fun connection ->
        Both_convert.dispatch_multi connection query)
      ~response_callback:(fun result ->
        let (pipe, id) = ok_exn result in
        let map_reaction =
          match Set_once.get map_reaction_in_client with
          | None -> (fun reaction -> Ok reaction)
          | Some of_server_reaction ->
            (fun reaction -> Or_error.try_with (fun () ->
               of_server_reaction action reaction))
        in
        let reader, writer = Pipe.create () in
        don't_wait_for (
          let%bind () =
            Pipe.transfer pipe writer ~f:(fun elt ->
              let result = Or_error.bind (Or_error.join elt) ~f:map_reaction in
              if is_error result then Pipe.close_read pipe;
              result)
          in
          let%map close_reason = Rpc.Pipe_rpc.close_reason id in
          (match close_reason with
           | Closed_remotely | Closed_locally -> ()
           | Error error ->
             Pipe.write_without_pushback_if_open writer (Error error)
          );
          Pipe.close writer);
        reader)
  ;;

  let rpc_to_server ?connection action =
    Monitor.try_with_or_error ~extract_exn:true (fun () ->
      rpc_to_server_exn ?connection action)
  ;;

  let command_line_rpc () =
    let%bind contents = Reader.file_contents "/dev/stdin" in
    let action = Sexp.of_string_conv_exn contents [%of_sexp: action] in
    match%bind
      Monitor.try_with_or_error ~extract_exn:true (fun () ->
        let%bind pipe = rpc_to_server action in
        Pipe.iter_without_pushback (ok_exn pipe) ~f:(fun reaction ->
          printf !"%{sexp#hum:reaction}\n" (ok_exn reaction)))
    with
    | Ok () -> return ()
    | Error error ->
      eprintf !"%{sexp#hum:Error.t}\n" error;
      Shutdown.exit 1
  ;;

  include Both_convert.Register (struct
      include Model
      include Version
      let query_of_caller_model    = Fn.id
      let callee_model_of_query    = Fn.id
      let response_of_callee_model = Fn.id
      let caller_model_of_response = Fn.id
      let error_of_callee_model    = Fn.id
      let caller_model_of_error    = Fn.id
      let client_pushes_back       = false
    end)

  let () =
    Rpc_summary.register
      rpc_summary
      ~name
      ~version:Version.version
      ~query:[%bin_shape: Model.query]
      ~response:[%bin_shape: Model.response]
      ~metadata:command_line_rpc
  ;;

  module Register_old_rpc
      (Version : sig val version : int end)
      (Action   : Old_action   with type model := Action.t)
      (Reaction : Old_reaction with type model := Reaction.t)
  = struct

    module Old_rpc = struct
      include Version
      type query    = Action.t Query.Stable.V1.t      [@@deriving bin_io]
      type response = Reaction.t Or_error.Stable.V2.t [@@deriving bin_io]
      type error    = Error.Stable.V2.t               [@@deriving bin_io]
      let callee_model_of_query query =
        Query.Stable.V1.map query ~f:Action.to_model
      ;;
      let response_of_callee_model = function
        | Error _ as error -> error
        | Ok model -> Or_error.try_with (fun () -> Reaction.of_model model)
      ;;
      let error_of_callee_model = Fn.id
      let unsupported () =
        raise_s [%sexp "it is not supported to use a new client with an old Iron server"
                     , (Name.name : string)]
      ;;
      let query_of_caller_model    _ = unsupported ()
      let caller_model_of_response _ = unsupported ()
      let caller_model_of_error    _ = unsupported ()
      let client_pushes_back         = false
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
end
