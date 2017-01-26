open! Core
open! Import

module Which_obligations : sig
  type t =
    { repo_root          : Iron.Abspath.t
    ; file_tree_of       : [ `Working_copy | `Rev of Iron.Raw_rev.t ]
    ; aliases_resolution : [ `None
                           | `Using_latest_aliases_from_iron_server
                           ]
    }
  [@@deriving fields, sexp_of]
end

module List_users : sig
  module Action : sig
    type t = Which_obligations.t
    [@@deriving sexp_of]
  end
  module Reaction : sig
    type t = Iron.Unresolved_name.Set.t
    [@@deriving sexp_of]
  end

  include Iron_command_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end

module List_groups : sig
  module Action : sig
    type t = Which_obligations.t
    [@@deriving sexp_of]
  end
  module Reaction : sig
    type t = Iron.Unresolved_name.Set.t Iron.Group_name.Map.t
    [@@deriving sexp_of]
  end

  include Iron_command_rpc.S
    with type action   = Action.t
    with type reaction = Reaction.t
end
