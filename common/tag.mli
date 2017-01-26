open! Core
open! Import

(** Tags are identifiers in obligations files which group related projects.

    If you want to model mercurial tags, see [Iron_hg.Tag] instead. *)

include Validated_string.S
