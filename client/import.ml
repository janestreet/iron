open! Core.Std
open! Async.Std

include (Iron_common.Std : module type of struct include Iron_common.Std end
         with module Command := Iron_common.Std.Command)
include Iron_obligations.Std
include Iron_hg.Std
include Iron_protocol

module Command_rpc = Async_extended.Std.Command_rpc
module Iron_command_rpc = Iron.Iron_command_rpc

module Command = struct
  include (Iron_common.Std.Command :
             module type of struct include Iron_common.Std.Command end
               with module Param      := Iron_common.Std.Command.Param
                and module Let_syntax := Iron_common.Std.Command.Let_syntax)
  module Param      = Iron.Std.Iron.Param
  module Let_syntax = Param.Let_syntax
end

include (struct
  open Iron.Std
  module Client_config = Iron.Client_config
  module Fe = Fe
end)

module Iron = struct let _ = `Use_Fe_instead end

let die message a sexp_of_a =
  eprintf "%s\n" (Sexp.to_string_hum
                    (Error.create message a sexp_of_a |> [%sexp_of: Error.t]));
  shutdown 1;
  Deferred.never ();
;;

let ensure_can_access_remote_repo ~for_root_of =
  let%bind { remote_repo_path; _ } =
    Get_feature_revs.rpc_to_server_exn
      { feature_path = Feature_path.root_path for_root_of
      ; rev_zero     = None
      }
  in
  Hg.ensure_can_access_remote_repo remote_repo_path
;;

let feature_reviewers feature_path ~sort =
  let%map feature =
    Get_feature.rpc_to_server_exn { feature_path; rev_zero = None }
  in
  Feature.reviewers_exn feature ~sort
;;

let () = print_elapsed [%here]

module Implement_command_rpc (M : sig
    module Action   : T
    module Reaction : T
  end) = struct
  open M
  type t = Action.t -> Reaction.t Deferred.t
end

let (>>>) = `Deprecated_in_iron__Use_let_syntax_instead
let (>>=) = `Deprecated_in_iron__Use_let_syntax_instead
let (>>|) = `Deprecated_in_iron__Use_let_syntax_instead
