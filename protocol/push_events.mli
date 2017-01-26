open! Core
open! Import

module What_to_dump : sig
  type t =
    [ `Feature_id of Feature_id.t
    | `Stats
    | `User_name  of User_name.t
    | `Values
    ]
  [@@deriving sexp_of]

  val require_admin_privileges : t -> bool
end

module Add : sig
  module Action : sig
    type t =
      { feature_id : Feature_id.t
      ; tip        : Rev.t
      }
    [@@deriving sexp_of]
  end

  module Reaction : Unit

  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end

module Change : sig
  module Action : sig
    type t =
      | Clear_all
      | Clear_features           of Feature_id.t list
      | Clear_revs               of Rev.t list
      | Clear_users              of User_name.t list
      | Set_max_size_per_feature of int
    [@@deriving sexp_of]
  end

  module Reaction : Unit

  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end
