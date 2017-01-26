open Core
open! Import

include Iron_options.Verbose

module Message = struct
  type 'a t =
    { elapsed_since_program_start : Time.Span.t
    ; elapsed_since_last_message  : Time.Span.t option
    ; message                     : string * 'a
    }
  [@@deriving sexp_of]
end

let last_message_at = ref None

let message string a sexp_of_a =
  let now = Time.now () in
  let elapsed_since_last_message =
    match !last_message_at with
    | None -> None
    | Some at -> Some (Time.diff now at)
  in
  let elapsed_since_program_start = Time.diff now program_started_at in
  last_message_at := Some now;
  Core.eprintf "%s\n%!"
    ({ Message.
       elapsed_since_program_start
     ; elapsed_since_last_message
     ; message = (string, a)
     }
     |> [%sexp_of: a Message.t]
     |> Sexp.to_string_hum)
;;

