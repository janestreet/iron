(** An Async-RPC proxy server that forwards RPCs requests to another RPC server and then
    forwards the response back.  The proxy server does not inspect most RPC requests.
    However it does treat [Versioned_rpc.Menu] specially -- it will only call the real
    server once with that, and then always return the same response.  This saves a round
    trip to the real server for each incoming RPC connection.
*)

open! Core
open! Async

val simple_server
  :  where_to_listen : Tcp.Where_to_listen.inet
  -> real_server     : Host_and_port.t
  -> (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t
