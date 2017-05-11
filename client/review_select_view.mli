open! Core
open! Async

(** Returns the equivalent of [List.filter_map configuration ~f:find_in_available], where
    [find_in_available] returns the first pair in [available] with the given first
    coordinate (with equality determined by [to_string]), with the following exception: if
    this list is empty, [available] is returned instead. *)
val select
  :  to_string: ('a -> string) (** should be injective *)
  -> available: ('a * 'b) list
  -> configuration: 'a list
  -> ('a * 'b) list

(** Given two sets [available] and [configuration], where [configuration] should be a
    subset of [available], interactively queries the user to allow them to change to a
    different subset of [available].  (If [configuration] isn't a subset of [available],
    its intersection with [available] is taken.) *)
val toggle
  :  menu_name:string
  -> to_string: ('a -> string) (** should be injective *)
  -> display_prefix_in_list: ('a -> string) option
  -> available: 'a list
  -> configuration: 'a list
  -> [ `New_configuration of 'a list
     | `File_by_file
     | `Global_diff
     | `Quit
     ] Deferred.t

(** [navigate] returns an optional element of [available].  Which element is returned
    depends on whether [`Pred_view] or [`Succ_view] is passed.

 * If [`Succ_view] is passed, the element in [available] after the maximum element of
    [configuration] is returned.

 * If [`Pred_view] is passed, the element in [available] before the minimum element of
    [configuration] is returned.

    Comparisons are done via the [to_string] function.

    It is used when the user enter the circular navigation between views while pressing:
    '>' or '<'.
    These keys may be pressed at a moment when several views are enabled, so it uses the
    heuristic described above to determine where to start in the circle.
    After entering the circle, navigate is called with [configuration:[current_view]]
*)
val navigate
  :  to_string: ('a -> string) (** should be injective *)
  -> available: 'a list
  -> configuration: 'a list
  -> [ `Pred_view | `Succ_view ]
  -> 'a option
