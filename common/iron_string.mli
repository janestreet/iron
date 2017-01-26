open! Core

include module type of String

val alphabetic_compare : t -> t -> int

val try_chop_suffix : t -> suffix:t -> t
