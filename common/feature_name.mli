open! Core
open! Import

include Validated_string.S

val to_file_name : t -> File_name.t

val unanchored_regex_pattern : string

val regex : Regex.t
