open! Core
open! Import

module Reason_for_disabling_rpc_to_server = struct
  type t = Use_of_fake_obligations
  [@@deriving compare, sexp_of]

  let equal (t1 : t) t2 = 0 = compare t1 t2

  let to_error = function
    | Use_of_fake_obligations ->
      Error.of_string "program makes use of fake obligations"
  ;;
end

let reasons_for_disabling_rpc_to_server = ref []

let disable_rpc_to_server reason =
  if not (List.exists !reasons_for_disabling_rpc_to_server
            ~f:(Reason_for_disabling_rpc_to_server.equal reason))
  then reasons_for_disabling_rpc_to_server :=
      reason :: !reasons_for_disabling_rpc_to_server
;;

let is_rpc_to_server_enabled () =
  match !reasons_for_disabling_rpc_to_server with
  | [] -> Ok ()
  | _ :: _ as reasons ->
    Error
      (Error.of_list (List.map reasons ~f:Reason_for_disabling_rpc_to_server.to_error))
;;
