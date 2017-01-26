open! Core
open! Import

module Syntax : sig
  type t [@@deriving sexp]
end

type t = private
  { name                            : Scrutiny_name.t
  ; level                           : Scrutiny_level.t
  ; min_file_reviewers              : int
  ; max_file_reviewers              : int
  ; read_by_whole_feature_reviewers : bool
  ; obligations_read_by             : Review_obligation.t
  ; description                     : string
  ; color                           : string option
  }
[@@deriving compare, fields, sexp_of]

val for_testing : t

val eval
  :  Scrutiny_name.t
  -> Syntax.t
  -> Error_context.t
  -> aliases       : User_name_by_alternate_name.t
  -> allowed_users : Unresolved_name.Set.t
  -> known_groups  : Groups.t
  -> t

module Stable : sig
  module V2 : sig
    include Stable_without_comparator with type t = t
    val hash : t -> int
  end
end
