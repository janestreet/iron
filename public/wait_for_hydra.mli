open! Core
open! Import

(** The aim of the RPC is to wait until the feature is no longer pending.  It raises on
    timeout. *)

module Action : sig
  type t =
    { feature_path      : Iron.Feature_path.t
    ; rev_zero          : Iron.Rev.t option
    ; timeout           : Iron.Time.Span.t
    ; whether_to_update : [ `No_update
                          | `Update
                          ]
    }
  [@@deriving sexp_of]
end

module Reaction : sig
  type t =
    { tip              : Iron.Rev.t
    ; remote_repo_path : Iron.Remote_repo_path.t
    }
  [@@deriving sexp_of]
end

include Iron_command_rpc.S
  with type action = Action.t
  with type reaction = Reaction.t
