open Core
open Async
open Import

let show_lines ?msg lines =
  let file = Filename.temp_file "patdiff4" ".review" in
  let%bind () =
    Writer.with_file file ~f:(fun writer ->
      List.iter lines ~f:(fun line ->
        Writer.write writer line;
        Writer.newline writer;
      );
      Deferred.unit
    )
  in
  let%bind () =
    Async_interactive.show_file
      ?pager:Client_config.(get () |> pager_for_review)
      ?msg ~file ()
  in
  Deferred.ignore (try_with (fun () -> Sys.remove file))
;;

module Choice = struct
  module Choice = Async_interactive.Choice
  type 'a t = 'a Choice.t

  let show_again label =
    Choice.create 'a' `Show_again (sprintf "Show the current %s again" label)
  ;;

  let commit_session  =
    Choice.create 'c' `Commit_session
      "Commit the current session and review the remainder to the current tip"
  ;;

  let reviewed label =
    Choice.create 'y' `Reviewed (sprintf "Mark the current %s as reviewed" label)
  ;;

  let not_reviewed label =
    Choice.create 'n' `Not_reviewed
      (sprintf "Skip the current %s for now. Go to the next one" label)
  ;;

  let previous label =
    Choice.create 'p' `Previous
      (sprintf "Skip the current %s for now. Go back to the previous one" label)
  ;;

  let quit = Choice.create 'q' `Quit "Quit"

  module Mode = struct
    let hunk_by_hunk   = Choice.create 'h' `Hunk_by_hunk   "Enter hunk-by-hunk mode"
    let file_by_file   = Choice.create 'f' `File_by_file   "Enter file-by-file mode"
    let global_diff    = Choice.create 'g' `Global_diff    "See all the remaining hunks at once"
    let selected_files = Choice.create 's' `Selected_files "Select a subset of the files"
  end
end
