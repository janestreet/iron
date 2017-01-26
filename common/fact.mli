(** Facts are propositions about scopes that can be asserted or retracted by authorized
    asserters. *)

open! Core
open! Import

(** A scope is a set of key-value pairs describing the domain over
    which a particular fact is declared to be true.  E.g:
    {v
      ((rev a73c238d8ed) (projection live) (pts_type regular))
    v}
*)
module Scope : sig
  module Key : Identifiable.S
  type t = string Key.Map.t [@@deriving sexp]

  include Comparable with type t := t
  include Hashable   with type t := t
  include Stringable with type t := t
end

(** A spec provides constraints on a particular type of fact.  E.g:

    {[

      ((scope_keys (rev))
         (authorization_rules (
            ((asserters (as-hydra)))
          ))
         (description "Automated regression tests have passed.")
      )
    ]}

    Someone can add a fact for a spec if _any_ authorization rule authorizes him.  This
    type is private so that values cannot be constructed in a way that bypasses the
    validation performed by [t_of_sexp].
*)

module Spec : sig
  module Id : Identifiable.S

  module Authorization_rule : sig
    type t =
      { asserters          : User_name.Set.t
      (** Optionally, an authorization rule may limit the values in scopes that an
          asserter can act over, e.g. [user1] may only be able to assert facts on [(repo
          live)] while [user2] can assert facts on [(repo live)] and [(repo jane)]. *)
      ; scope_restrictions : String.Set.t Scope.Key.Map.t sexp_option
      }
    [@@deriving fields, sexp_of]
  end

  type t = private
    { scope_keys          : Scope.Key.Set.t
    ; authorization_rules : Authorization_rule.t list
    ; description         : string
    }
  [@@deriving fields, sexp]
end

(** Evidence that a fact is true in the form of a record of its assertion by a qualified
    asserter. *)
module Evidence : sig
  type t = private
    { asserter       : User_name.t
    ; assertion_time : Iron_time.t
    ; comment        : string
    }
  [@@deriving sexp_of]
end

module Action : sig
  type t =
    Spec.Id.t
    * [ `add_spec of Spec.t
      | `remove_spec
      | `add_fact of Scope.t * User_name.t * [`comment of string] * Iron_time.t
      | `remove_fact of Scope.t
      ]
  [@@deriving sexp_of]

  module Persist : Persistent.S with type t = t
end

(** Internal data structure for storing facts. *)
module Db : sig
  type t [@@deriving sexp_of]

  include Invariant.S with type t := t

  val create : unit -> t

  (** All actions are atomic: they either work, or fail without making any change. *)
  val handle_action : t -> Action.t -> unit Or_error.t

  (** Readonly queries *)
  val evidence       : t -> Spec.Id.t -> Scope.t -> Evidence.t Or_error.t
  val list_evidences : t -> Spec.Id.t -> (Scope.t * Evidence.t) list Or_error.t
  val spec           : t -> Spec.Id.t -> Spec.t Or_error.t
  val list_specs     : t -> (Spec.Id.t * Spec.t) list
end

module Stable : sig
  module Scope : sig
    module Key : sig
      module V1 : Stable_without_comparator with type t = Scope.Key.t
    end
    module V1 : Stable_without_comparator with type t = Scope.t
  end

  module Spec : sig
    module Id : sig
      module V1 : Stable_without_comparator with type t = Spec.Id.t
    end
    module V1 : Stable_without_comparator with type t = Spec.t
  end

  module Evidence : sig
    module V1 : Stable_without_comparator with type t = Evidence.t
  end

  module Action : sig
    module V2 : Stable_without_comparator with type t = Action.t
    module V1 : sig
      include Stable_without_comparator
      val to_model : t -> V2.t
    end
  end
end

