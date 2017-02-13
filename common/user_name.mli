(** A user name (e.g. [jdoe]).  It cannot be an alias, because aliases are resolved
    before such a value is created.  However, we do not know whether a user name is a
    valid user name yet.

    A user name must match this regex: ["^[a-zA-Z0-9_][a-zA-Z0-9._-]*$"].

    Because there is no constraint on the content of a user names that appears in the text
    of a CR, if such a username doesn't match the regex, then we treat the CR as malformed
    and assign it to the feature owner. *)

open! Core
open! Import

type t

include Validated_string.Unstable with type t := t

val unix_login : t

(** [missing_file_owner] is used for unassigned CR-soons and CR-somedays, which are
    supposed to be assigned to a file's owner, but cannot be when the file owner is
    missing due to invalid obligations. *)
val missing_file_owner : t

(** This is a fake user name used to create a [Reviewer.t] when a query requests to build
    a feature diff as it would be seen by a whole feature reviewer. *)
val synthetic_whole_feature_reviewer : t

(** A user name must be a valid file name, because the server uses user names as file
    names in its persistent store. *)
val to_file_name : t -> File_name.t

val to_unresolved_name : t -> Unresolved_name.t

module Or_all : sig
  type user_name
  type t =
    [ `All_users
    | `User of user_name
    ]
  [@@deriving sexp_of]

  include Stringable.S with type t := t

  val arg_type
    : complete_user_name:(Univ_map.t -> part:string -> string list)
    -> t Command.Arg_type.t

  val arg_doc : string
end with type user_name := t

module Or_all_or_all_but : sig
  type user_name
  type t =
    [ `All_users_but of Set.t
    | Or_all.t
    ]
  [@@deriving sexp_of]

  val mem : t -> user_name -> bool
end with type user_name := t

module Stable : sig
  include Validated_string.Stable
    with type model := t
    with type comparator_witness := comparator_witness

  module Or_all : sig
    module V1 : Stable_without_comparator with type t = Or_all.t
  end
  module Or_all_or_all_but : sig
    module V1 : Stable_without_comparator with type t = Or_all_or_all_but.t
  end
end
