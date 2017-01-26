(** A user alias or a typo.  Alternate name are only used as part of a mapping from
    alternate name to user names.  See also [unresolved_name.mli]. *)

open! Core
open! Import

include Validated_string.S

val to_unresolved_name : t -> Unresolved_name.t
