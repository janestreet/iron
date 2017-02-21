open! Core
open! Import

(** Summarizes available rpcs and their versions.  Tracks custom metadata for each rpc. *)

type 'a t

val create : unit -> 'a t

val register
  :  'a t
  -> name     : string
  -> version  : int
  -> query    : Bin_prot.Shape.t
  -> response : Bin_prot.Shape.t
  -> metadata : 'a
  -> unit

(** It is an error to [register_old_version ~name] before [register ~name]. *)
val register_old_version
  :  'a t
  -> name     : string
  -> version  : int
  -> query    : Bin_prot.Shape.t
  -> response : Bin_prot.Shape.t
  -> unit

(** It is an error to register after any of the following functions are called. *)

(** Takes the rpc name on the command line and gives that rpc's metadata. *)
val arg_type : 'a t -> 'a Command.Arg_type.t

val find_rpc_exn : 'a t -> name : string -> version : int -> Rpc_description.t

val rpc_descriptions : 'a t ->  Rpc_description.t list Lazy.t
