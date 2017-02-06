(** The state at the heart of the Iron server.

    The state comprises:

    - a [Feature.t] for every feature.
    - a [Review_manager.t] for every feature and user involved in that feature.
    - a set of pairs of revisions [(r1, r2)] such that all obligations have been met from
    [r1] to [r2].

    Persistence is achieved via a number of files rooted at some user-specified directory,
    one file for each piece of state above.  All files are updated by appending sexps.
    For features, the files are arranged in a directory tree where path parts correspond
    to feature names in feature paths.  In the tree, there is a file for each feature that
    contains the sequence of [Action.t Query.t]s applied to that feature.  For each
    feature there is also a file for each user with the review manager for that user and
    feature, containing the sequence of [Review_manager.(Action.t Query.t)]s applied to
    that review manager.

    The set of reviewed pairs of revisions is a single file at the root of the directory,
    and pairs are appended when they are added to the set.
*)

open! Core
open! Async
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val initialize_and_sync_file_system : Serializer.t -> unit Deferred.t
val deserializer : (server_config:Iron_config.t -> t) Deserializer.t

val rpc_implementations : t Rpc.Implementations.t
