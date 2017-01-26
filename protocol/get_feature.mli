open! Core
open! Import

module By_id : sig

  module Action : sig
    type t =
      { feature_id       : Feature_id.t
      ; even_if_archived : bool
      }
    [@@deriving sexp_of]
  end

  module Reaction : sig
    type t = Feature.t
    [@@deriving sexp_of]
  end

  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end

module Maybe_archived : sig

  module Action : sig
    type t =
      { what_feature : Maybe_archived_feature_spec.t
      ; what_diff    : What_diff.t
      }
    [@@deriving sexp_of]
  end

  module Reaction : sig
    type t = Feature.t
    [@@deriving sexp_of]
  end

  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end

module Action : sig
  type t =
    { feature_path : Feature_path.t
    (** Pass [rev_zero] if in a repo, and Iron server will check that [feature_path] is in
        the same family.  If you don't pass [rev_zero], the server won't do the check. *)
    ; rev_zero     : Rev.t option
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t = Feature.t
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
