open! Core.Std
open! Import

module Action : sig
  type t =
    | Set_max_subscriptions_per_user of int
    | Set_max_subscriptions_global   of int
    | Drop_all_by_user               of [ `User of User_name.t
                                        | `All_users
                                        ]
  [@@deriving sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action = Action.t
  with type reaction = Reaction.t
