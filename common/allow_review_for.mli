(** A specification of which users can modify review of other users.  These are defined in
    obligations-repo.sexp.  They are computed in the Iron-hydra worker, passed to the
    server, and stored in features, review managers, and review sessions.  They are
    ultimately tested in the client, before allowing a reviewer to review a diff for
    another user.  They are also double checked in the server, before allowing one user to
    adjust another user's brain or review session. *)

open! Core
open! Import

module Users : sig
  type t [@@deriving sexp_of]

  val empty     : t
  val all_users : t

  val users : User_name.Set.t -> t

  val add   : t -> User_name.t -> t
  val mem   : t -> User_name.t -> bool
  val union : t -> t -> t
  val union_list : t list -> t
end

type t [@@deriving sexp_of]

val none : t
val all  : t

val also_allow
  :  t
  -> reviewed_for : Users.t
  -> reviewed_by  : Users.t
  -> t

val may_be_reviewed_by : t -> reviewed_for:User_name.t -> Users.t

val check
  :  t
  -> reviewed_for       : User_name.t
  -> reviewed_by        : User_name.t
  -> unit Or_error.t

module Stable : sig
  module V1 : sig
    include Stable_without_comparator with type t = t
    val hash : t -> int
  end

  module Users : sig
    module V1 : sig
      include Stable_without_comparator with type t = Users.t
      val hash : t -> int
    end
  end
end
