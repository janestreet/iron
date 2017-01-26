open! Core
open! Import

val load_state_and_check_invariants : Command.t

val commands : (string * Command.t) list

module Proxy : sig
  val command : Command.t
end
