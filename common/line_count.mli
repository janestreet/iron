open! Core
open! Import


module Review : sig
  type t =
    { must_review       : int
    ; follow            : int
    ; may_review        : int
    ; ownership_changes : int
    }
  [@@deriving fields, sexp_of]

  include Invariant.S with type t := t

  val zero  : t
  val (+)   : t -> t -> t
  val total : t -> int

  val add_count : t -> Review_kind.t -> int -> t

  val to_review_column_shown :
    t -> have_potentially_blocking_review_session_in_progress : bool -> Review_or_commit.t
end

module Catch_up : sig
  type t =
    { create_catch_up_for_me   : int
    ; follower                 : int
    ; unfinished_review        : int
    ; reviewed_by_someone_else : int
    }
  [@@deriving fields, sexp_of]

  include Invariant.S with type t := t

  val zero  : t
  val (+)   : t -> t -> t
  val total : t -> int

  val add_count : t -> Catch_up_kind.t -> int -> t
end

type t =
  { review    : Review.t
  ; catch_up  : Catch_up.t
  ; completed : int
  ; have_potentially_blocking_review_session_in_progress : bool
  }
[@@deriving fields, sexp_of]

include Invariant.S with type t := t

val zero : t
val (+)  : t -> t -> t

val is_zero : t -> bool

val is_shown : t -> show_completed_review:bool -> bool

val sort_decreasing_review : (User_name.t * t) list -> (User_name.t * t) list

val catch_up_only : Catch_up.t -> t

(** If the user has an uncommitted session and 0 [must_review] lines in their review todo
    they need to be notified that something that needs to be done -- Iron encourages the
    user to commit their session. *)
val _must_review_or_commit : t -> Review_or_commit.t
val to_review_column_shown : t -> Review_or_commit.t

module Cached_in_feature : sig
  type line_count

  type t =
    { review    : Review.t To_goal_via_session.t
    ; completed : int
    ; have_potentially_blocking_review_session_in_progress : bool
    }
  [@@deriving compare, fields, sexp_of]

  include Invariant.S with type t := t

  module By_user : sig
    type nonrec t = t User_name.Map.t [@@deriving sexp_of]
    include Invariant.S with type t := t
    include Equal.S     with type t := t
  end
end with type line_count := t

module Stable : sig
  module Review : sig
    module Model : T with type t = Review.t
    module V1 : Stable_without_comparator with type t = Model.t
  end
  module Catch_up : sig
    module Model : T with type t = Catch_up.t
    module V1 : Stable_without_comparator with type t = Model.t
  end
  module Model : T with type t = t
  module V5 : Stable_without_comparator with type t = Model.t
  module V4 : sig
    type t =
      { review    : Review.V1.t
      ; catch_up  : Catch_up.V1.t
      ; completed : int
      ; have_uncommitted_and_potentially_blocking_session : bool
      }
    include Stable_without_comparator with type t := t
    val of_v5 : V5.t -> t
  end
  module V3 : sig
    type t =
      { review    : Review_or_commit.Stable.V1.t
      ; follow    : int
      ; catch_up  : int
      ; completed : int
      }
    include Stable_without_comparator with type t := t
    val of_v4 : V4.t -> t
  end
  module V2 : sig
    type t =
      { review    : int
      ; catch_up  : int
      ; completed : int
      }
    include Stable_without_comparator with type t := t
    val of_v3 : V3.t -> t
  end
end
