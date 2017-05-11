(** Keeps track of all catch up sessions for a user for a feature path.

    Catch up sessions are created when one user reviews for another, [fe review -for], for
    example, when one user reviews for another who is on vacation. *)

open! Core
open! Import

type t [@@deriving sexp_of]

include Container.S0 with type t := t and type elt := Catch_up_session.t
include Invariant.S  with type t := t

val create : unit -> t

val is_empty : t -> bool

val find
  :  t
  -> Session_id.t
  -> Catch_up_session.t option

val add
  :  t
  -> Catch_up_session.t
  -> unit

(** Dereferencing a session will just remove the link to it from the catch-up manager, but
    this will not delete the session.  It is up to the caller to independently either
    delete the session or link it back to another manager.  If not the session directory
    will just be stale on disk and most probably reloaded on restart. *)
val remove_link
  :  t
  -> Session_id.t
  -> unit

val remove_all_links_for_feature_id : t -> Feature_id.t -> Catch_up_session.t list
val find_all_for_feature_id         : t -> Feature_id.t -> Catch_up_session.t list

val find_all_for_feature_path : t -> Feature_path.t -> Catch_up_session.t list

val to_protocol
  :  t
  -> Catch_up_session.t
  -> is_archived:Is_archived.t
  -> lines_required_to_separate_ddiff_hunks:int
  -> Iron_protocol.Get_catch_up_session.Catch_up_session.t

val get_next_session
  :  t
  -> [ `Catch_up_session of Catch_up_session.t
     | `Up_to_date
     ]

val line_count_remaining_to_catch_up : t -> Line_count.Catch_up.t
