open Core
open Async
open Import


let open_file file =
  match%bind Abspath.file_exists_exn file with
  | false -> return ()
  | true ->
    let elisp =
      sprintf "\
(let ((frame-to-use
          (if (and (boundp 'Jane.Cr.dedicated-review-frame)
                (frame-live-p Jane.Cr.dedicated-review-frame))
              Jane.Cr.dedicated-review-frame
              (selected-frame))))
   (set-window-buffer
      (frame-selected-window frame-to-use)
      (find-file-noselect \"%s\")))"
        (Abspath.to_string file)
    in
    match%map Process.run ~prog:"emacsclient" ~args:[ "-e"; elisp ] () with
    | Ok (_ : string) -> ()
    | Error e -> raise_s [%sexp "problem with emacs", (e : Error.t)]
;;
