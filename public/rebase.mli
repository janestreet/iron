open! Core
open! Import

module Validation_command : sig
  type t =
    { prog : string
    ; args : string list
    }
  [@@deriving sexp_of]
end

module Action : sig
  type t =
    { feature_path                : Iron.Feature_path.t
    ; allow_non_cr_clean_new_base : bool
    ; for_                        : Iron.User_name.t
    ; new_base                    : Iron.Raw_rev.t option
    ; abort_on_merge_conflicts    : bool
    ; post_merge_validation_hook  : Validation_command.t option
    }
  [@@deriving sexp_of]
end

module Reaction : Unit

include Iron_command_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
