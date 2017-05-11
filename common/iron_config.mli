open! Core
open! Async
open! Import

(** After [use_prod_IRON_CONFIG ()], a subsequent use of [as_per_IRON_CONFIG] or
    [load_as_per_IRON_CONFIG] will load the prod config.  [use_prod_IRON_CONFIG] raises if
    they have already been used. *)
val use_prod_IRON_CONFIG : unit -> unit

module Rpc_proxy_config : sig
  type t
end

module Restricted_for_rpcs : sig

  (** [load_as_per_IRON_CONFIG] isn't [lazy] like [as_per_IRON_CONFIG] below, so that one
      can call it multiple times over the life of a program, and get different results if
      the Iron config has changed.  E.g. hydra uses this so that it can pick up
      Iron-config changes without having to restart. *)
  val load_as_per_IRON_CONFIG
    :  may_connect_to_proxy:bool
    -> unit
    -> Host_and_port.t Deferred.t
end

(** [hydra_user] determines how [fe release] works with continuous release.  For ordinary
    users, [fe release] does [Hg.push], but hydra, it does a direct release. *)
type t = private
  { host                       : string
  ; async_rpc_port             : Async_rpc_port.t
  ; rpc_proxy_config           : Rpc_proxy_config.t
  ; hgrc                       : Abspath.t
  ; hydra_user                 : User_name.t
  ; serializer_pause_timeout   : Time.Span.t
  }
[@@deriving fields, sexp_of]

(** [for_checking_invariants] is a bogus config that will cause Iron server to immediately
    fail if its fields are used.  It is used for [fe admin
    check-invariants-of-last-backup], as a way of being conservative so that Iron server
    can't actually behave like a prod server. *)
val for_checking_invariants : t

include Invariant.S with type t := t

val load_exn : basedir:Abspath.t -> t Deferred.t

(** The config value to use, as determined by the [IRON_CONFIG] environment variable. *)
val as_per_IRON_CONFIG : t Deferred.t Lazy.t

val prod_basedir : Abspath.t
val prod_etc     : Abspath.t
val prod_var     : Abspath.t

val deploy_offices : string list

val serializer_pause_timeout_default : Time.Span.t
