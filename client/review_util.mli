open! Core
open! Async
open! Import

val show_lines
  :  ?msg:string
  -> string list
  -> unit Deferred.t

module Choice : sig
  type 'a t = 'a Async_interactive.Choice.t
  val show_again     : string -> [> `Show_again   ] t
  val reviewed       : string -> [> `Reviewed     ] t
  val not_reviewed   : string -> [> `Not_reviewed ] t
  val previous       : string -> [> `Previous     ] t
  val commit_session : [> `Commit_session ] t
  val quit           : [> `Quit] t
  module Mode : sig
    val hunk_by_hunk   : [> `Hunk_by_hunk   ] t
    val file_by_file   : [> `File_by_file   ] t
    val global_diff    : [> `Global_diff    ] t
    val selected_files : [> `Selected_files ] t
  end
end
