open Core
open Async
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
  ; temporary_message : Temporary_message.t sexp_option
  }
[@@deriving sexp]

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
  Writer.save_sexp ~perm (Abspath.to_string file) (sexp_of_t t)
;;

let load_exn file =
  Reader.load_sexp_exn (Abspath.to_string file)
    (Sexp.of_sexp_allow_extra_fields t_of_sexp)
;;

let prod_path =
  Abspath.extend Iron_config.prod_var (File_name.of_string "server-down.txt")
;;

let roll_etc_path =
  Abspath.extend Iron_config.prod_etc (File_name.of_string "server-down-roll.txt")
;;
