open! Core
open! Import

module Command_rpc : sig
  module Process : sig
    type t =
      { prog : string
      ; args : string list
      }
    [@@deriving sexp_of]
  end

  type t =
    [ `Default
    | `Explicit of Process.t
    ]
  [@@deriving sexp_of]

  val process      : t -> Process.t
  val fe_exec_args : string list
end

module Rpc_to_server : sig

  type t =
    [ `Default
    | `Explicit of Host_and_port.t
    ]
  [@@deriving sexp_of]
end

type t =
  { command_rpc   : Command_rpc.t
  ; rpc_to_server : Rpc_to_server.t
  }
[@@deriving fields, sexp_of]
