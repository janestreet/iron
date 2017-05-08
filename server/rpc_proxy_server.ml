open Core
open Async

let log ~level sexp =
  Log.Global.sexp ~level sexp
;;

module Description = struct
  module T = struct
    include Rpc.Description
    let t_of_sexp _ = failwith "t_of_sexp unimplemented"
    let hash (t : t) = Hashtbl.hash t
  end
  include T
  include Hashable.Make (T)
end

let cache_rpcs_of { Rpc.Description. name; version = _ } =
  String.equal name Versioned_rpc.Menu.rpc_name
;;

type t =
  { cache : Bigstring.t Or_error.t Description.Table.t }
[@@deriving sexp_of]

let create () =
  { cache = Description.Table.create () }
;;

let lost_connection_error =
  Error.of_string "In RPC Proxy: connection with real server was closed"
;;

let handle_rpc t connection ~rpc_tag ~version responder buf ~pos ~len =
  let module Responder = Rpc.Rpc.Expert.Responder in
  let response_by_description = t.cache in
  let description = { Description. name = rpc_tag; version } in
  let should_cache =
    cache_rpcs_of description
    && not (Hashtbl.mem response_by_description description)
  in
  let cache data =
    (* By the time the boolean [should_cache] is used in the handlers, the table may have
       been mutated.  This may happen in case multiple queries happen in parallel on
       startup.  In case of a closed connection, the state will be reset so the value to
       choose here should not matter that much.  Presumably if the values come from the
       same connection, their values are likely to be consistent. *)
    Hashtbl.update response_by_description description ~f:(fun previous ->
      match previous, data with
      | Some (Ok _ as previous), Error _ -> previous
      | _, Ok _ | (None | Some Error _), Error _ ->
        log ~level:`Info
          [%sexp
            "caching RPC response",
            { description : Description.t
            ; data        : _ Or_error.t
            }
          ];
        data)
  in
  let handle_response buf ~pos ~len =
    log ~level:`Debug
      [%sexp
        "proxying RPC response",
        { rpc_tag : string
        ; version : int
        }
      ];
    Responder.write_bigstring responder buf ~pos ~len;
    if should_cache then cache (Ok (Bigstring.sub buf ~pos ~len));
  in
  let handle_error error =
    if should_cache then cache (Error error);
    Responder.write_error responder error;
  in
  match Hashtbl.find response_by_description description with
  | Some result ->
    (match result with
     | Ok buf      -> handle_response buf ~pos:0 ~len:(Bigstring.length buf)
     | Error error -> handle_error error)
  | None ->
    log ~level:`Debug
      [%sexp
        "proxying RPC query",
        { rpc_tag : string
        ; version : int
        }
      ];
    (* The incoming RPC had a query id, which is in [responder].  This call to
       [dispatch] creates a new, unrelated, query id for the connection to the
       real server.  [handle_response] will output the incoming query id. *)
    let status =
      let handle_response buf ~pos ~len = handle_response buf ~pos ~len; Deferred.unit in
      Rpc.Rpc.Expert.dispatch connection ~rpc_tag ~version buf ~pos ~len
        ~handle_response ~handle_error
    in
    (match status with
     | `Ok -> ()
     | `Connection_closed -> handle_error lost_connection_error)
;;

let simple_server ~where_to_listen ~real_server =
  let module Durable = Async_extended.Std.Durable in
  let durable_connection =
    Durable.create
      ~to_create:(fun () ->
        log ~level:`Info
          [%sexp "connecting to RPC server...", (real_server : Host_and_port.t) ];
        Rpc.Connection.client ()
          ~host:(Host_and_port.host real_server)
          ~port:(Host_and_port.port real_server)
        >>| function
        | Ok connection ->
          log ~level:`Info
            [%sexp "connected to RPC server...", (real_server : Host_and_port.t) ];
          Ok (create (), connection)
        | Error exn ->
          let error = Error.of_exn (Monitor.extract_exn exn) in
          log ~level:`Error
            [%sexp
              "failed to connect to RPC server...",
              { server = (real_server : Host_and_port.t)
              ; error : Error.t
              }
            ];
          Error error)
      ~is_broken:(fun (_, connection) ->
        let is_broken = Rpc.Connection.is_closed connection in
        if is_broken
        then
          log ~level:`Error
            [%sexp "connection to RPC server was lost", (real_server : Host_and_port.t) ];
        is_broken)
      ()
  in
  let handle_rpc () ~rpc_tag ~version responder buf ~pos ~len =
    Durable.with_ durable_connection ~f:(fun (proxy, connection) ->
      return (Or_error.try_with (fun () ->
        handle_rpc proxy connection ~rpc_tag ~version responder buf ~pos ~len)))
    >>| function
    | Ok () -> ()
    | Error error -> Rpc.Rpc.Expert.Responder.write_error responder error
  in
  let implementations =
    Rpc.Implementations.Expert.create_exn
      ~implementations:[]
      ~on_unknown_rpc:(`Expert handle_rpc)
  in
  Rpc.Connection.serve ()
    ~implementations
    ~initial_connection_state:(fun _ _ -> ())
    ~where_to_listen
;;
