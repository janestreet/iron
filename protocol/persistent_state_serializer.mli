open! Core
open! Import

module Pause : sig
  module Action : sig
    type t =
      { with_timeout : Time.Span.t
      }
    [@@deriving fields, sexp_of]
  end
  module Reaction : Unit
  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end

module Prior_changes_synced_to_file_system : sig
  module Action : Unit
  module Reaction : Unit
  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end

module Resume : sig
  module Action : Unit
  module Reaction : Unit
  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end

module Status : sig
  module Action : Unit
  module Reaction : sig
    type t = Sexp.t
    [@@deriving sexp_of]
  end
  include Iron_versioned_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end
