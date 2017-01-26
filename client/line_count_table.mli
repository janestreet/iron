open! Core
open! Import

val create
  :  show_completed_review:bool
  -> (User_name.t * Line_count.t) list
  -> Ascii_table.t
