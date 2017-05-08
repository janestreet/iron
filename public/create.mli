open! Core
open! Import

module Action : sig
  type t =
    { feature_path                : Iron.Feature_path.t
    ; base                        : Iron.Raw_rev.t option
    ; tip                         : Iron.Raw_rev.t option
    ; description                 : string option
    ; owners                      : Iron.User_name.t list
    ; is_permanent                : bool
    ; remote_repo_path            : Iron.Remote_repo_path.t option
    ; no_bookmark                 : bool
    ; add_whole_feature_reviewers : Iron.User_name.Set.t option
    ; reviewing                   : [ `Whole_feature_reviewers
                                    | `First_owner
                                    ]
    ; allow_non_cr_clean_base     : bool
    ; properties                  : Iron.Properties.t option
    }
  [@@deriving sexp_of]
end

module Reaction : Unit

include Iron_command_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
