open! Core
open! Async

include (Iron_common.Std : module type of struct include Iron_common.Std end
         with module Command := Iron_common.Std.Command)

module Command = struct
  include (Iron_common.Std.Command :
             module type of struct include Iron_common.Std.Command end
           with module Param      := Iron_common.Std.Command.Param
            and module Let_syntax := Iron_common.Std.Command.Let_syntax)
  module Param      = Iron.Std.Iron.Param
  module Let_syntax = Param.Let_syntax
end

include Int.Replace_polymorphic_compare

let (>>>) = `Deprecated_in_iron__Use_let_syntax_instead
let exit  = `Deprecated_in_iron__Use_shutdown_dot_exit_instead
let failwiths = `Deprecated_in_iron__Use_raise_s
