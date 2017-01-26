open! Core
open! Import

module Kind : sig
  type t = Gc_stat | Gc_quick_stat | Process_times
  [@@deriving sexp_of]

  include Enum.S with type t := t
end

module Action : sig
  type t =
    { kind : Kind.t
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t = Sexp.t
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
