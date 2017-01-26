open! Core
open! Import

type t [@@deriving sexp_of]

include Comparator.S       with type t := t
include Comparable.S_plain with type t := t
                            and type comparator_witness := comparator_witness
include Stringable.S       with type t := t
include Hashable.S_plain   with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t
    include Stable_without_comparator
      with type t := t
    include Comparable.Stable.V1.S
      with type comparable := t
      with type comparator_witness = comparator_witness
  end
end

(** This represents the span in sec it takes the system [Hydra / Iron] to realize that a
    new revision has been pushed, causing the feature to require an update bookmark.
    Typically this is the time it takes to [fe show] to start showing the feature as
    pending just following a push. *)
val hydra_synchronize_state_latency : t

(** This represents the span in sec it takes for a feature state to be updated to a new
    revision [rev] that has been pushed, from the time [rev] was pushed. *)
val hydra_update_bookmark_latency   : t
