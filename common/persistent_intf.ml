open! Core
open! Import

module type Model = sig
  type t [@@deriving sexp]
end

module type Version = sig
  val version : int
end

module type Context = sig
  type t [@@deriving sexp_of]
end

module type S = sig
  type t
  module Persist : sig
    type nonrec t = t [@@deriving sexp]
  end
end

module type S_writer = sig
  type t
  module Persist : sig
    type nonrec t = t [@@deriving sexp_of]
  end
end

module type S_reader = sig
  type t
  module Persist : sig
    type nonrec t = t [@@deriving of_sexp]
  end
end

module type Persistent = sig

  module type S = S

  module Reader : sig
    type 'a t = (module S_reader with type t = 'a)

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  module Writer : sig
    type 'a t = (module S_writer with type t = 'a)

    val prepend : 'a t -> f:('b -> 'a) -> 'b t
  end

  module Make
      (Version : Version)
      (Model   : Model)
    : sig
      include S with type t = Model.t
      module Register_read_old_version
          (Version : Version)
          (Conv    : sig
             type t [@@deriving of_sexp]
             val to_model : t -> Model.t
           end)
        : sig end
    end

  (** [Register_read_write_old_version] should be applied at most once for a given type or
      this would raise.  During serialization, [should_write_this_version] is called and
      if this returns [true], the old version is serialized instead of the model.

      For any version, at most one of [Register_read_write_old_version] and
      [Register_read_old_version] may be used. *)
  module Make_with_context
      (Context : Context)
      (Version : Version)
      (Model   : Model)
    : sig
      module Writer : S_writer with type t = Context.t * Model.t
      module Reader : S_reader with type t = Context.t -> Model.t
      module Register_read_old_version
          (Version : Version)
          (Conv    : sig
             type t [@@deriving of_sexp]
             val to_model : Context.t -> t -> Model.t
           end)
        : sig end
      module Register_read_write_old_version
          (Version                : Version)
          (Conv                   : sig
             type t [@@deriving sexp]
             val of_model : Context.t -> Model.t -> t
             val to_model : Context.t -> t -> Model.t
             val should_write_this_version : Context.t -> bool
           end)
        : sig end
    end
end
