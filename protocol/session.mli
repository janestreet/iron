open! Core
open! Import

module Forget : sig
  module Action : sig
    type t =
      { feature_path      : Feature_path.t
      ; for_              : User_name.t
      ; review_session_id : Session_id.t
      ; what_to_forget    : [ `All
                            | `Files of Path_in_repo.t list
                            ]
      }
    [@@deriving fields, sexp_of]
  end
  module Reaction : Unit
  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end

module Commit : sig
  module Action : sig
    type t =
      { feature_path      : Feature_path.t
      ; for_              : User_name.t
      ; review_session_id : Session_id.t
      }
    [@@deriving fields, sexp_of]
  end

  module Reaction : Unit

  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end

module Set_lock : sig
  module Action : sig
    type t =
      { feature_path     : Feature_path.t
      ; for_             : User_name.t
      ; which_session    : Which_session.t
      ; set_is_locked_to : bool
      }
    [@@deriving fields, sexp_of]
  end

  module Reaction : Unit

  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end
