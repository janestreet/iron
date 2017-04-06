open! Core
open! Async

let with_temp_file ?file f =
  let base = match file with
    | Some file -> Filename.basename file
    | None -> "tmp"
  in
  let prefix,suffix = match String.rsplit2 ~on:'.' base with
    | Some (base, ext) -> base, ext
    | None -> base, ".tmp"
  in
  let file = Filename.temp_file prefix suffix in
  Monitor.protect
    ~finally:(fun () -> Unix.unlink file)
    (fun () -> f file)
;;

let invoke_editor ?(tmpfile = "tmp") text =
  let editor =
    Option.value ~default:"emacs" (Core_extended.Std.Sys_utils.get_editor ())
  in
  with_temp_file ~file:tmpfile (fun file ->
    let%bind () = Writer.save file ~contents:text in
    match%bind Unix.system (sprintf "%s %s" editor file) with
    | Ok () ->
      let%map contents = Reader.file_contents file in
      Ok contents
    | stat  ->
      error "Error editing text" stat [%sexp_of: Unix.Exit_or_signal.t]
      |> return
  )
;;
