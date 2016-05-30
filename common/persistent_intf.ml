open! Core.Std
open! Import

module type Of_sexp = sig
  type model
  type t [@@deriving of_sexp]
  val to_model : t -> model
end

module type To_sexp = sig
  type model
  type t [@@deriving sexp_of]
  val of_model : model -> t
end

module type Of_sexp_with_context = sig
  type context
  type model
  type t [@@deriving of_sexp]
  val to_model : context -> t -> model
end

module type To_sexp_with_context = sig
  type context
  type model
  type t [@@deriving sexp_of]
  val of_model : context -> model -> t
end

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
      module Register_read_old_persist
          (Version : Version)
          (Of_sexp : Of_sexp with type model := Model.t)
        : sig end
      (** [Register_write_old_persist] should be applied at most once for a given type,
          and after registering the corresponding read_persist for that version.  It
          raises otherwise. *)
      module Register_write_old_persist
          (Version : Version)
          (To_sexp : To_sexp with type model := Model.t)
        : sig end
    end

  module Make_with_context
      (Context : Context)
      (Version : Version)
      (Model   : Model)
    : sig
      module Writer : S_writer with type t = Context.t * Model.t
      module Reader : S_reader with type t = Context.t -> Model.t
      module Register_read_old_persist
          (Version : Version)
          (Of_sexp : Of_sexp_with_context
           with type model := Model.t
            and type context := Context.t)
        : sig end
      (** [Register_write_old_persist] should be applied at most once for a given type,
          and after registering the corresponding read_persist for that version.  It
          raises otherwise. *)
      module Register_write_old_persist
          (Version : Version)
          (To_sexp : To_sexp_with_context
           with type model := Model.t
            and type context := Context.t)
        : sig end
    end
end
