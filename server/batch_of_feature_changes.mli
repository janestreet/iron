open! Core
open! Import

(** A value of type [t] holds a collection of changes that one wants to make on a feature,
    to be applied at once.  The value of batching the changes as opposed to applying them
    one by one resides in the fact that [change_feature] needs to run potentially costly
    operations to refresh the rest of the state after the changes happened. *)
type t

val empty : t

val to_feature_updates : Feature.t -> t -> Iron_protocol.Change_feature.Update.t list

val add_whole_feature_reviewers : t -> whole_feature_reviewers:User_name.Set.t -> t
val add_properties              : t -> properties:Properties.t -> t

val add_inherited_from_parent
  : t
  -> parent_inheritable_attributes:Feature_inheritable_attributes.t
  -> t
