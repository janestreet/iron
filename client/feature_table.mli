open! Core
open! Import

val create
  :  'a list
  -> ?preserve_input_ordering:unit
  -> ('a -> Feature_path.t)
  -> (feature:string -> 'a option -> 'b)
  -> 'b list
