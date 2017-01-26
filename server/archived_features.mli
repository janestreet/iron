(** All features that have ever been archived on the server, indexed by feature path and
    date.

    Here is the file-system layout of a persistent [t].

    {v
      queries
      $YYYY/
        $MM/
          $DD/
            $FEATURE_ID/
    v}
*)

open! Core
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val deserializer
  : dynamic_upgrade_state:Dynamic_upgrade.State.t
  -> t Deserializer.t

(** [feature_dir archived_feature] returns the directory where [archived_feature] is
    serialized, which is of the form [$YYYY/$MM/$DD/$FEATURE_ID]. *)
val feature_dir : Archived_feature.t -> Relpath.t

val add                : t -> _ Query.t -> Archived_feature.t -> unit
val remove             : t -> _ Query.t -> Archived_feature.t -> unit
val set_max_cache_size : t -> _ Query.t -> max_size:int -> unit

val find_by_id   : t -> Feature_id.t   -> Archived_feature.t Or_error.t
val find_by_path : t -> Feature_path.t -> Archived_feature.t list

module Cache : sig
  type data = Iron_protocol.Feature.t

  val find         : t -> Feature_id.t -> data option
  val add          : t -> data -> unit
  val remove       : t -> Feature_id.t -> unit
  val clear        : t -> unit

  module What_to_dump : sig
    type t =
      [ `Stats
      | `Ids_and_feature_paths
      | `Value of Feature_id.t
      ]
  end

  val dump : t -> What_to_dump.t -> Sexp.t
end

val mem_feature_path : t -> Feature_path.t -> bool

val iteri : t -> f:(Feature_path.t -> Archived_feature.t list -> unit) -> unit

val list_features
  :  t
  -> descendants_of:Which_ancestor.t
  -> depth:int
  -> Iron_protocol.List_features.Reaction.one list Or_error.t

val list_feature_names
  :  t
  -> descendants_of:Which_ancestor.t
  -> depth:int
  -> Feature_path.t list Or_error.t

val complete : t -> prefix:string -> [ `Of_partial_name | `Of_full_name ] -> string list
