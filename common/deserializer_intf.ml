(** For deserializing a subtree of the filesystem.

    A [Deserializer.t] is a specification of how to deserialize a value from a subtree of
    the filesystem.  The specification says what files to read and what types of values to
    read (represented using [Persistent] sexps).  A [Deserializer.t] is a functional
    value -- to actually perform deserialization, one calls [load].
*)

open! Core
open! Async
open! Import

module type S = sig
  type 'a t [@@deriving sexp_of]

  include Applicative            with type 'a t := 'a t
  include Monad.S_without_syntax with type 'a t := 'a t

  val load
    :  'a t
    -> root_directory : Abspath.t
    -> serializer     : Serializer.t
    -> 'a Deferred.t

  val in_subdir : Relpath.t -> 'a t -> 'a t

  (** [one] returns [default] if [in_file] doesn't exist. *)
  val one : ?default:'a -> 'a Persistent.Reader.t -> in_file:Relpath.t -> 'a t
  val one_opt : 'a Persistent.Reader.t -> in_file:Relpath.t -> 'a option t

  (** [sequence_of] returns the empty list if [in_file] doesn't exist. *)
  val sequence_of : 'a Persistent.Reader.t -> in_file:Relpath.t -> 'a list t

  val upgrade_one
    :  from_:('a Persistent.Reader.t * Relpath.t)
    -> to_:  ('b Persistent.Reader.t * Relpath.t)
    -> ('a -> 'b)
    -> 'b t

  val all_subdirs : 'a t -> 'a File_name.Map.t t

  (** [with_serializer f] applies [f] to a serializer that records in the same directory
      where the deserializer returned by [f] is ultimately run.  This is useful because a
      deserializer usually needs access to a serializer, so that it can record any
      subsequent changes to the value after it is deserialized. *)
  val with_serializer : (Serializer.t -> 'a t) -> 'a t

end

module type Deserializer = sig
  include S


  module Let_syntax : sig
    val return : 'a -> 'a t
    module Let_syntax : sig
      val return : 'a -> 'a t
      val map    : 'a t -> f:('a -> 'b) -> 'b t
      val bind   : 'a t -> f:('a -> 'b t) -> 'b t
      val both   : 'a t -> 'b t -> ('a * 'b) t
      module Open_on_rhs : S with type 'a t := 'a t
    end
  end
end
