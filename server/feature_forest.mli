open! Core
open! Import

type 'a t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

val create : unit -> 'a t

val find : 'a t -> Feature_path.t -> 'a Or_error.t
val mem  : 'a t -> Feature_path.t -> bool
val iteri       : 'a t -> f:(Feature_path.t -> 'a -> unit) -> unit
val iteri_roots : 'a t -> f:(Feature_name.t -> 'a -> unit) -> unit
val iter_children : 'a t -> Feature_path.t -> f:('a -> unit) -> unit
val iter_descendants : 'a t -> Feature_path.t -> f:('a -> unit) -> unit

val complete : _ t -> prefix:string -> [ `Of_partial_name | `Of_full_name ] -> string list

val find_root : 'a t -> Feature_name.t -> 'a Or_error.t
val root_of   : 'a t -> Feature_path.t -> 'a Or_error.t

(** [check_add] returns [Ok] if [path] can be added to [t], i.e. if [path] is a leaf. *)
val check_add : 'a t -> Feature_path.t -> unit Or_error.t

(** [add_exn t path a] raises if [not (is_ok (check_add t path))] *)
val add_exn : 'a t -> Feature_path.t -> 'a -> unit

val add_ancestors : 'a t -> Feature_path.t -> f:(unit -> 'a) -> unit

(** [change_exn t path] fails if [not (mem t path)]. *)
val change_exn : 'a t -> Feature_path.t -> ('a -> 'a) -> unit

val roots    : 'a t -> (Feature_name.t * 'a) list

val has_children_exn : _ t -> Feature_path.t -> bool

val remove_exn       : _ t -> Feature_path.t -> unit

val list
  :  'a t
  -> descendants_of:Which_ancestor.t
  -> depth:int
  -> (Feature_path.t * 'a) list Or_error.t

(** [strict_descendants t feature_path ~f] returns [Error] if [not (mem t
    feature_path)]. *)
val strict_descendants
  :  'a t
  -> Feature_path.t
  -> 'a list Or_error.t
