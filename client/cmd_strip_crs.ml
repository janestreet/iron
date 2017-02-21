open! Core
open! Async
open! Import

let strip_crs_exn format ~replace_with ~extra_cr_comment_headers file =
  let%map contents = Reader.file_contents (Abspath.to_string file) in
  let raws =
    Cr_comment.Raw.With_file_positions.extract
      format
      ~extra_cr_comment_headers
      ~path:(Relpath.of_string "no-one-cares")
      ~file_contents:contents
  in
  let offsets : Cr_comment.Raw.With_file_positions.t -> _ =
    if Option.is_some replace_with
    then (
      fun { content_start; cr; _ } ->
        content_start, content_start + String.length (Cr_comment.Raw.content cr))
    else (
      fun { start_index = comment_start; end_index; _ } ->
        let comment_stop = end_index + 1 in
        let rec extend_left = function
          | 0 -> `Bol 0
          | x when Char.equal contents.[x-1] '\n' -> `Bol x
          | x when not (Char.is_whitespace contents.[x-1]) -> `Start x
          | x -> extend_left (x-1)
        in
        let rec extend_right = function
          | x when x = String.length contents -> `Eof x
          | x when Char.equal contents.[x] '\n' -> `Eol (x+1)
          | x when not (Char.is_whitespace contents.[x]) -> `Stop x
          | x -> extend_right (x+1)
        in
        match extend_left comment_start, extend_right comment_stop with
        | `Bol start,   (`Eof stop | `Eol stop) -> start, stop
        | `Bol _,        `Stop stop -> comment_start, stop
        | `Start start,  `Eof stop  -> start, stop
        | `Start start,  `Eol stop  -> start, stop - 1
        | `Start _,      `Stop _    -> comment_start, comment_stop)
  in
  let chunks =
    let stop = String.length contents in
    List.fold_right raws ~init:([],stop) ~f:(fun raw (acc,stop) ->
      let comment_start, pos = offsets raw in
      let len = stop - pos in
      String.sub contents ~pos ~len :: acc, comment_start)
    |> (fun (acc, stop) -> String.sub contents ~pos:0 ~len:stop :: acc)
  in
  String.concat ?sep:replace_with chunks
;;

let command =
  let default_max_concurrent_jobs = 8 in
  let max_concurrent_jobs_switch = "-max-concurrent-jobs" in
  let in_place_switch = "-in-place" in
  Command.async' ~summary:"strip out CRs from one or more files"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and replace_with =
       flag "-replace-with" (optional string)
         ~doc:"TEXT replace CR comments with this string"
     and in_place =
       flag in_place_switch no_arg ~doc:" update files in place"
     and max_concurrent_jobs =
       flag max_concurrent_jobs_switch (optional int)
         ~doc:(sprintf "NUM allow NUM jobs at once when updating files in \
                        place.  default is %d"
                 default_max_concurrent_jobs)
     and extra_cr_comment_headers =
       flag "-extra-cr-comment-header" (listed string)
         ~doc:"STR additional header to recognize as a CR"
     and files =
       map (anon (non_empty_sequence_as_pair
                    ("FILE" %: resolved_file_path_arg_type)))
         ~f:(fun (x, l) -> x :: l)
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let max_concurrent_jobs =
         match max_concurrent_jobs with
         | Some value ->
           if not in_place
           then failwithf "Switch [%s] may be passed only when [%s] is supplied."
                  max_concurrent_jobs_switch in_place_switch ();
           value
         | None -> default_max_concurrent_jobs
       in
       let how =
         if in_place then
           `Max_concurrent_jobs max_concurrent_jobs
         else
           `Sequential
       in
       Deferred.List.iter files ~how ~f:(fun file ->
         let%bind contents =
           strip_crs_exn Cr_comment_format.latest ~replace_with ~extra_cr_comment_headers
             file
         in
         if in_place then
           Writer.save (Abspath.to_string file) ~contents
         else (
           print_string contents;
           Writer.flushed (force stdout))))
;;
