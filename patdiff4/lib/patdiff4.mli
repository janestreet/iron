open! Core
open! Import

module View_ids_computed : sig
  (** Optimizing the computation done if the result is of no interest.

      [Compute_every_view_available] : no optimization. This is the default mode.

      [Compute_only_by_diff4_class of Diff_algo_id.Set.t Diff4_class.Map.t] if there is a
      set present in the map, only compute the algo of that set. If the set is not present
      in the map, compute everything

      [Compute_only_in_that_order of Diff_algo_id.t list]
      compute only strictly the ids of that list if they are available *)
  type t =
    | Compute_every_view_available
    | Compute_only_by_diff4_class of Diff_algo_id.Set.t Diff4_class.Map.t
    | Compute_only_in_that_order of Diff_algo_id.t list
end

(** Note about the rev_names argument and contents.
    For optimization purposes, the implementation assumes the following property:

    P: Forall a, b in the input Diamonds,
    ( rev_name(a) = rev_name(b) ) => ( contents(a) = contents(b) )

    If [P] does not hold, the behavior of that module is unspecified. *)


val hunks
  :  ?verbose:bool
  -> ?view_ids_computed:View_ids_computed.t
  -> rev_names:string Diamond.t
  -> file_names:string Diamond.t
  -> header_file_name:string
  -> context:int
  -> lines_required_to_separate_ddiff_hunks:int
  -> scrutiny:File_scrutiny.t option
  -> contents:string Diamond.t
  -> unit
  -> Hunk.t list

(** This performs all the steps of a full diff, returning a colorized string list with the
    diff output. *)
val diff
  :  ?verbose:bool
  -> view_ids_shown:Hunk.View_ids_shown.t
  -> rev_names:string Diamond.t
  -> file_names:string Diamond.t
  -> header_file_name:string
  -> context:int
  -> lines_required_to_separate_ddiff_hunks:int
  -> contents:string Diamond.t
  -> unit
  -> string list

val num_lines_to_review
  :  lines_required_to_separate_ddiff_hunks:int
  -> contents:string Diamond.t
  -> int
