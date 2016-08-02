open Core.Std
open Async.Std
open Import

module Temporary_message = struct
  type t =
    { expires_at : Iron_time.Stable.V1_round_trippable.t
    ; message    : string
    }
  [@@deriving fields, sexp]

  let create ~expires_in message =
    { message = String.rstrip message
    ; expires_at = Time.add (Time.now ()) expires_in
    }
  ;;
end

type t =
  { message           : string
  ; temporary_message : Temporary_message.t option
  }

let persist =
  Future_proof_serializer.mapN
    Future_proof_serializer.(
      field       "message"           (module String) ~default:""
      & field_opt "temporary_message" (module Temporary_message)
      & nil)
    ~load:(fun message temporary_message  -> { message ; temporary_message })
    ~save:(fun f_message f_temporary_message { message ; temporary_message } ->
      f_message message;
      f_temporary_message temporary_message)
;;

let create message =
  { message = String.rstrip message
  ; temporary_message = None
  }
;;

let get_non_temporary_message t = t.message
let get_temporary_message t = Option.map t.temporary_message ~f:Temporary_message.message

let message t =
  let default = String.rstrip t.message in
  match t.temporary_message with
  | None -> default
  | Some t ->
    let now = Time.now () in
    if Time.(>=) now t.expires_at
    then default
    else (
      let span = Time.diff t.expires_at now in
      sprintf "%s\n\nRestoration of the service expected in %s or before"
        (String.rstrip t.message) (Time.Span.to_short_string span))
;;

let set_temporary_message t ~expires_in message =
  let temporary_message = Temporary_message.create ~expires_in message in
  { t with temporary_message = Some temporary_message }
;;

let clear_temporary_message t = { t with temporary_message = None }
;;

let save_exn t ~perm file =
  Future_proof_serializer.to_sexp persist t
  |> Writer.save_sexp ~perm (Abspath.to_string file)
;;

let load_exn file =
  Reader.load_sexp_exn (Abspath.to_string file) (Future_proof_serializer.of_sexp persist)
;;

let prod_path =
  Abspath.extend Iron_config.prod_var (File_name.of_string "server-down.txt")
;;

let roll_etc_path =
  Abspath.extend Iron_config.prod_etc (File_name.of_string "server-down-roll.txt")
;;
