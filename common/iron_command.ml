open Core.Std
open Async.Std
open! Import

include Command

let to_string_hum str =
  if String.length str = 0
  then "<no error message>"
  else
    String.concat [ Char.to_string (Char.uppercase str.[0])
                  ; String.drop_prefix str 1
                  ; if Char.equal str.[String.length str - 1] '.'
                    then ""
                    else "."
                  ]
;;

let in_async param =
  let open Let_syntax in
  [%map
    let main = param in
    fun () ->
      let open Deferred.Let_syntax in
      let monitor = Monitor.create () in
      Stream.iter (Monitor.detach_and_get_error_stream monitor) ~f:(fun exn ->
        shutdown 1;
        Core.Std.eprintf "%s\n%!"
          (match Monitor.extract_exn exn with
           | Failure str -> to_string_hum str
           | exn ->
             match exn |> [%sexp_of: exn] with
             | Sexp.Atom str -> to_string_hum str
             | sexp -> Sexp.to_string_hum sexp));
      don't_wait_for begin
        let%bind () = within' ~monitor main in
        (* We close stdout and stderr to make sure they are flushed before we call
           [shutdown]. *)
        let%map () =
          Deferred.List.iter ~how:`Parallel [ stdout; stderr ] ~f:(fun writer ->
            Writer.close writer ~force_close:(Deferred.never ()))
        in
        shutdown 0
      end;
      (never_returns (Scheduler.go ()) : unit)
  ]
;;

let async' ~summary ?readme param =
  Command.basic' ~summary ?readme (in_async param)
;;

let async = `Use_async'_instead
let async_basic = `Async_basic_is_deprecated
let async_or_error = `Redefine_async_or_error_in_iron_command_if_needed
