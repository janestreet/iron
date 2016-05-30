open! Core.Std
open! Import

module Get : sig
  module Action : Unit

  module Reaction : sig
    type t = User_name.Set.t
    [@@deriving sexp_of]
  end

  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end

module Change_user : sig
  module Action : sig
    type t =
      { user_name : User_name.t
      ; change    : [ `Add | `Remove ]
      }
    [@@deriving fields, sexp_of]
  end

  module Reaction : Unit

  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end
