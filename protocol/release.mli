open! Core
open! Import

module Action : sig
  type t =
    { feature_path : Feature_path.t
    ; rev_zero     : Rev.t
    ; tagged_tip   : Rev.t option
    ; for_         : User_name.t
    }
  [@@deriving fields, sexp_of]
end

module Reasons_for_not_archiving : sig
  type t

  val create
    : [ `Feature_is_permanent
      | `Feature_has_children
      ] list
    -> t

  val to_string_hum : t -> string
end

module Disposition : sig
  type t =
    [ `Released_and_archived
    | `Released_and_cleared of Reasons_for_not_archiving.t
    | `Not_released__push_to_hydra
    ]
end

module Reaction : sig
  type t =
    { disposition           : Disposition.t
    (** [send_release_email_to] is the list of users to send release mail to -- this
        includes the feature's owners and its parent's owners, as well as their
        [send_release_email_to]s. *)
    ; send_release_email_to : Email_address.Set.t
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
