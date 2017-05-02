open! Core
open! Import

module Action : sig
  type t =
    { feature_path         : Feature_path.t
    ; rev_zero             : Rev.t
    ; for_                 : User_name.t
    ; reason_for_archiving : string
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t =
    { remote_repo_path : Remote_repo_path.t
    ; send_email_to    : Email_address.Set.t
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
