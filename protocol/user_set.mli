open! Core
open! Import

type t =
  [ `Admins
  | `Feeding_metrics
  | `Using_locked_sessions
  ]
[@@deriving enumerate, sexp_of]

module Get : sig
  module Action : sig
    type nonrec t = t
    [@@deriving sexp_of]
  end

  module Reaction : sig
    type t = User_name.Set.t
    [@@deriving sexp_of]
  end

  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end

module Change : sig
  module Action : sig
    type user_set
    type t =
      { user_set   : user_set
      ; user_names : User_name.Set.t
      ; change     : [ `Add | `Remove ]
      ; idempotent : bool
      }
    [@@deriving fields, sexp_of]
  end with type user_set := t

  module Reaction : Unit

  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end
