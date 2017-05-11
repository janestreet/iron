(** A glob pattern for a *simple file name*, not for a full path.  Thus (1) slashes & nul
    chars not permitted; (2) must be legal glob pattern. *)

open! Core
open! Import

type t

(** [of_string] checks: valid glob, no nul chars & no / chars. *)
include Identifiable with type t := t

val star : t

val matches : t -> File_name.t -> bool
