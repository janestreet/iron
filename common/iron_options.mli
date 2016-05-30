open! Core.Std

type t [@@deriving sexp_of]

val t : t

val display_ascii_always                 : bool
val find_features_by_partial_name        : bool
val load_user_configs                    : bool
val silence_uninteresting_hg_warnings    : bool
val workspaces_are_enabled__forced_value : bool option

module Verbose : sig
  val build_order  : bool
  val deserializer : bool
  val command      : bool (* Verbose top-level command processing. *)
  val command_rpc  : bool
  val cr_comment   : bool
  val hg           : bool (* Noisy hg invocations. *)
  val knowledge    : bool
  val patdiff4     : bool
  val rpc          : bool
  val serializer   : bool
  val worker       : bool
  val workspaces   : bool
end
