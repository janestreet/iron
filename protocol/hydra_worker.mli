open! Core
open! Import

module Action : sig
  (** [tip] is given so that if there are cached information about that tip on the
      server it will be included it in the reaction *)
  type t =
    { feature_path : Feature_path.t
    ; rev_zero     : Rev.t
    ; tip          : Rev.t option
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  (** The user names in [need_diff4s_starting_from] aren't necessary, but are useful
      for debugging. *)
  type t =
    { base                             : Rev.t
    ; feature_id                       : Feature_id.t
    ; need_diff4s_starting_from        : (Review_edge.t * User_name.Set.t) list
    ; aliases                          : User_name_by_alternate_name.t
    ; lines_required_to_separate_ddiff_hunks : int
    ; worker_cache                     : Worker_cache.From_server_to_worker.t
    }
  [@@deriving sexp_of]

  module Concise : sig type nonrec t = t [@@deriving sexp_of] end
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
