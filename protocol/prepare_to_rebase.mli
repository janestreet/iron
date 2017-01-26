open! Core
open! Import

module Action : sig
  type t =
    { feature_path                : Feature_path.t
    ; rev_zero                    : Rev.t
    ; allow_non_cr_clean_new_base : bool
    ; for_                        : User_name.t
    ; new_base                    : Rev.t option
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t =
    { old_tip          : Rev.t
    ; old_base         : Rev.t
    ; new_base         : Rev.t
    ; remote_repo_path : Remote_repo_path.t
    ; feature_id       : Feature_id.t
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
