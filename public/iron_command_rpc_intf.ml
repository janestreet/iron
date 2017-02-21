open! Core
open! Async
open! Import

module Versioned_rpc = Async.Versioned_rpc

module type Name = sig
  val name : string
end

module type Version = sig
  val version : int
end

module type S = sig
  type action   [@@deriving sexp]
  type reaction [@@deriving sexp_of]
  type query    = action   [@@deriving sexp]
  type response = reaction [@@deriving sexp_of]

  include Name

  include Versioned_rpc.Both_convert.Plain.S
    with type caller_query    = action
    with type caller_response = reaction
    with type callee_query    = action
    with type callee_response = reaction

  val command_rpc
    :  ?connection:Connection.Command_rpc.t
    -> action
    -> reaction Or_error.t Deferred.t
end

module type Action = sig
  type t [@@deriving bin_io, sexp]

  val to_model : t -> t
end

module type Reaction = sig
  type t [@@deriving bin_io, sexp_of]

  val of_model : t -> t
end

module type Map_reaction_in_client = sig
  type action
  type reaction

  (** Additional computation performed on the client side for performance reasons after
      receiving the reaction from the command. *)
  val of_command_reaction : action -> reaction -> reaction
end

module type Old_action = sig
  type model
  type t [@@deriving bin_io]

  val to_model : t -> model
end

module type Old_reaction = sig
  type model
  type t [@@deriving bin_io]

  val of_model : model -> t
end

module type Old_action_converting_both_ways = sig
  include Old_action
  val of_model : model -> t
end

module type Old_reaction_converting_both_ways = sig
  include Old_reaction
  val to_model : t -> model
end

module type Iron_command_rpc = sig
  module type S = S
  module Make
      (Name     : Name)
      (Version  : Version)
      (Action   : Action)
      (Reaction : Reaction)
    : sig
      include S
        with type action = Action.t
        with type reaction = Reaction.t
      module Register_old_rpc
          (Version  : Version)
          (Action   : Old_action   with type model := Action.t)
          (Reaction : Old_reaction with type model := Reaction.t)
        : sig end
      module Register_old_rpc_converting_both_ways
          (Version  : Version)
          (Action   : Old_action_converting_both_ways   with type model := Action.t)
          (Reaction : Old_reaction_converting_both_ways with type model := Reaction.t)
        : sig end
      module Register_map_reaction_in_client
          (Map_reaction_in_client : Map_reaction_in_client
           with type action   := Action.t
            and type reaction := Reaction.t)
        : sig end
    end

  val rpc_descriptions : Iron.Rpc_description.t list Lazy.t
  val find_rpc_exn : name : string -> version : int -> Iron.Rpc_description.t
end
