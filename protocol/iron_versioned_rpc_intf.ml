open! Core
open! Async
open Import

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
  type query    = action   Query.Stable.V1.t                         [@@deriving sexp]
  type response = reaction Or_error.Stable.V1.t Response.Stable.V1.t [@@deriving sexp_of]

  include Name

  val implement_deferred_rpc
    : ('state -> version:int -> query -> response Deferred.t)
    -> 'state Rpc.Implementation.t list

  val rpc_to_server
    :  ?connection:[ `Default
                   | `Explicit of Host_and_port.t
                   ]
    -> action
    -> reaction Or_error.t Deferred.t

  val rpc_to_server_exn
    :  ?connection:[ `Default
                   | `Explicit of Host_and_port.t
                   ]
    -> action
    -> reaction Deferred.t
end

module type S_pipe_rpc = sig
  type action   [@@deriving sexp]
  type reaction [@@deriving sexp_of]
  type query    = action Query.Stable.V1.t [@@deriving sexp]

  include Name

  (** The pipe returned by the server may contain an [Error] value, in which case it will
      be the last value handed to the client and will close the pipe.  This is to furnish
      a way to pass in some termination value in case of an Error, as opposed to just
      closing the pipe. *)
  val implement_deferred_rpc
    :  ('state
        -> version : int
        -> query
        -> reaction Or_error.t Pipe.Reader.t Or_error.t Deferred.t)
    -> 'state Rpc.Implementation.t list

  val rpc_to_server
    :  ?connection:[ `Default
                   | `Explicit of Host_and_port.t
                   ]
    -> action
    -> reaction Or_error.t Pipe.Reader.t Or_error.t Deferred.t

  val rpc_to_server_exn
    :  ?connection:[ `Default
                   | `Explicit of Host_and_port.t
                   ]
    -> action
    -> reaction Or_error.t Pipe.Reader.t Deferred.t
end

module type Action = sig
  type t [@@deriving bin_io, sexp]

  (** [to_model] should be the identity function.  It is present for consistency between
      [Action] and [Old_action]. *)
  val to_model : t -> t
end

module type Reaction = sig
  type t [@@deriving bin_io, sexp_of]

  (** [of_model] should be the identity function.  It is present for consistency between
      [Action] and [Old_action]. *)
  val of_model : t -> t
end

module type Map_reaction_in_client = sig
  type action
  type reaction

  (** Additional computation performed on the client side for performance reasons after
      receiving the reaction from the server. *)
  val of_server_reaction : action -> reaction -> reaction
end

module type Old_action = sig
  type model
  type t [@@deriving bin_io]

  (** [to_model] runs in the server, and converts the old action used by the client into
      the action used by the server. *)
  val to_model : t -> model
end

module type Old_reaction = sig
  type model
  type t [@@deriving bin_io]

  (** [of_model] runs in the server, and converts the reaction used by the server to the
      old reaction used by the client. *)
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

(** We require [with sexp] rather [with sexp_of] so that we can send raw queries to the
    server via the command line tool [fe internal rpc]. *)
module type Iron_versioned_rpc = sig

  module type S = S
  module type S_pipe_rpc = S_pipe_rpc

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

  module Make_pipe_rpc
      (Name     : Name)
      (Version  : Version)
      (Action   : Action)
      (Reaction : Reaction)
    : sig
      include S_pipe_rpc
        with type action = Action.t
        with type reaction = Reaction.t
      module Register_old_rpc
          (Version  : Version)
          (Action   : Old_action   with type model := Action.t)
          (Reaction : Old_reaction with type model := Reaction.t)
        : sig end
      module Register_map_reaction_in_client
          (Map_reaction_in_client : Map_reaction_in_client
           with type action   := Action.t
            and type reaction := Reaction.t)
        : sig end
    end

  val rpc_descriptions : Rpc_description.t list Lazy.t
  val find_rpc_exn : name : string -> version : int -> Rpc_description.t
  val command : Command.t Lazy.t
end
