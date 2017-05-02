open! Core
open! Import

module Action : sig
  type t =
    { for_               : User_name.t
    ; machine_name       : Machine_name.t
    ; unclean_workspaces : Unclean_workspace.t list
    ; clean_workspaces   : [ `Complement_of_those_listed_as_unclean
                           | `At_least_these of Feature_path.t list
                           ]
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
