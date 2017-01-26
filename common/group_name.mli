(** A name for a group of users, e.g. "tech".

    A group name must match this regex: ["^[a-zA-Z0-9_][a-zA-Z0-9._-]*$"].
*)

open! Core
open! Import

include Validated_string.S
