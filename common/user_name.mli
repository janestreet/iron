(** A user name (e.g. [jdoe]).  It cannot be an alias, because aliases are resolved
    before such a value is created.  However, we do not know whether a user name is a
    valid user name yet.

    A user name must match this regex: ["^[a-zA-Z0-9_][a-zA-Z0-9._-]*$"].

    Because there is no constraint on the content of a user names that appears in the text
    of a CR, if such a username doesn't match the regex, then we treat the CR as malformed
    and assign it to the feature owner.
*)

open! Core.Std
open! Import

include Validated_string.S

val unix_login : t

(** [missing_file_owner] is used for unassigned CR-soons and CR-somedays, which are
    supposed to be assigned to a file's owner, but cannot be when the file owner is
    missing due to invalid obligations. *)
val missing_file_owner : t

(** this is a fake user name used to create a [Reviewer.t] when a query requests to build
    a feature diff as it would be seen by a whole feature reviewer. *)
val synthetic_whole_feature_reviewer : t

(** A user name must be a valid file name, because the server uses user names as file
    names in its persistent store. *)
val to_file_name : t -> File_name.t

val to_unresolved_name : t -> Unresolved_name.t
