open! Core
open! Async
open! Import

module Command_rpc = struct

  let fe_exec_args = [ "internal" ; "command-rpc" ; "call" ]

  module Process = struct
    type t =
      { prog : string
      ; args : string list
      }
    [@@deriving sexp_of]

    let default : t =
      let prog =
        if am_functional_testing
        then Sys.executable_name
        else "/j/office/app/fe/prod/bin/fe"
      in
      { prog
      ; args = fe_exec_args
      }
    ;;
  end

  type t =
    [ `Default
    | `Explicit of Process.t
    ]
  [@@deriving sexp_of]

  let process = function
    | `Default          -> Process.default
    | `Explicit process -> process
  ;;
end

module Rpc_to_server = struct

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
