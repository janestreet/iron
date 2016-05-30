(** Scrutiny records, defined in [.fe/obligations-global.sexp].

    {v
      SCRUTINY-DEFN ::= (define_scrutiny NAME (SCRUTINY-CLAUSE ...))
      SCRUTINY-CLAUSE ::= (min_file_reviewers NAT)
                        | (max_file_reviewers NAT)
                        | (level NAT) ; An integer in the range [0,100]
                        | (read_by_whole_feature_reviewers BOOL)
                        | (description STRING)
                        | (color SYMBOL) ; E.g., red, green, yellow
    v}

    [Syntax.t] is for the forms as they are written; [t] is for the name-resolved value.

    Constraints checked on the number of reviewers:

 * no satisfying set of users is smaller than [min_file_reviewers]

 * complain about situations where we're always going to have too many reviewers.  It's
    OK if some satisfying set of reviewers is too large.  We just want there to be some
    satisfying set that isn't.
*)

open! Core.Std
open! Import

module Syntax : sig
  type t =
    { min_file_reviewers              : int
    ; max_file_reviewers              : int
    ; read_by_whole_feature_reviewers : bool
    ; level                           : Scrutiny_level.Syntax.t
    ; description                     : string
    ; color                           : string sexp_option
    }
  [@@deriving compare, sexp]

  val for_testing : t
end

type t =
  { name  : Scrutiny_name.t
  ; value : Syntax.t
  }
[@@deriving compare, fields, sexp_of]

val for_testing : t

val create    : Scrutiny_name.t -> Syntax.t -> t

module Stable : sig
  module V1 : sig
    include Stable_without_comparator with type t = t
    val hash : t -> int
  end
end
