open! Core
open! Async

module type Config = sig
  module Statement : sig
    type t [@@deriving of_sexp]
  end

  type t

  (** give a list of files that are always loaded.  Example
      [/j/office/app/fe/prod/etc/ferc] *)
  val always_loaded_if_present : Abspath.t list

  (** [home_basename] is the name of the config file, which will be in the users home
      directory. *)
  val home_basename : string

  (** A config is defined by a sequence of statements.  [create] creates a default config,
      while [update] mutates the config to reflect the effect of a statement. *)
  val create : unit -> t
  val update : t -> Statement.t -> unit
end

module type S = sig

  type t

  (** This module holds a lazy value that loads the config from [home_basename] in the
      users home directory, if it exists; otherwise it uses [X.create].  Both [get] and
      [errors] force this lazy value.  [get] returns the config, while [errors] returns
      the errors that happened during the loading of the config.

      The reason we ignore the errors during the loading is because we want to roll out
      new functionality or deprecate existing config constructors easily without having to
      fear that we are going to break fe for a bunch of users, especially because these
      files are very unstable for now.

      We offer a programmatic API to access errors (function [errors] below), as well as
      the two following commands for users to check and refresh their config from time to
      time asynchronously after a roll has happened:

      $ fe tools validate-ferc
      $ patdiff4 validate-config
  *)
  val get    : unit -> t
  val errors : unit -> Error.t list

  val validate_config : Command.t
end

module type Make_intf = sig

  module type Config = Config
  module type S = S

  module Make (X : Config) : S with type t := X.t

  module Utils : sig
    type t = Sexp.t list -> unit
    val empty : t

    type flag

    val (+>) : t -> flag -> t

    val no_arg : string -> (unit -> unit) -> flag
    val flag   : string -> (Sexp.t -> 'a) -> ('a -> unit) -> flag
  end
end
