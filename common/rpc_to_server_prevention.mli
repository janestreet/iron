open! Core
open! Import

module Reason_for_disabling_rpc_to_server : sig
  type t = Use_of_fake_obligations
  [@@deriving sexp_of]
end

val disable_rpc_to_server : Reason_for_disabling_rpc_to_server.t -> unit

val is_rpc_to_server_enabled : unit -> unit Or_error.t
