open! Core
open! Async
open! Import

module Action : sig
  type t =
    { files : string Diamond.t
    ; lines_required_to_separate_ddiff_hunks : int
    }
  [@@deriving sexp]
end

module Reaction : sig
  type t =
    { num_lines_in_diff4 : int
    }
  [@@deriving sexp]
end

val compute : Action.t -> Reaction.t Deferred.t
val internal_group : string
