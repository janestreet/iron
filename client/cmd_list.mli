open! Core
open! Import

module Table : sig
  val main : Implement_command_rpc (Fe.List.Table).t
end

val command : Command.t
