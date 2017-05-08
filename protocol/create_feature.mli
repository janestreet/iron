open! Core
open! Import

module Action : sig
  type t =
    { feature_path                : Feature_path.t
    ; owners                      : User_name.t list
    ; is_permanent                : bool
    ; description                 : string
    ; base                        : Rev.t option
    ; tip                         : Rev.t option
    ; add_whole_feature_reviewers : User_name.Set.t
    ; reviewing                   : [ `Whole_feature_reviewers
                                    | `First_owner
                                    ]
    ; rev_zero                    : Rev.t
    ; remote_repo_path            : Remote_repo_path.t option
    ; allow_non_cr_clean_base     : bool
    ; properties                  : Properties.t option
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t =
    { feature_id       : Feature_id.t
    ; remote_repo_path : Remote_repo_path.t
    ; tip              : Rev.t
    }
  [@@deriving fields, sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
