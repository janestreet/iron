open Core.Std
open Async.Std
open Import

let dialog ~feature_path ~display_ascii ~max_output_columns
      { Prepare_to_second.Reaction. whole_feature_review_remaining; cr_summary } =
  let%bind s =
    Reader.file_contents
      (Abspath.to_string
         (Abspath.extend Iron_config.prod_etc (File_name.of_string "seconding.txt")))
  in
  let%bind () = Interactive.printf "%s\n" s in
  let%bind () =
    match Cr_comment.Summary.to_ascii_table cr_summary with
    | None -> return ()
    | Some table ->
      Interactive.printf "\nOutstanding CRs:\n%s\n"
        (Ascii_table.to_string table ~display_ascii ~max_output_columns)
  in
  let%bind () =
    if List.is_empty whole_feature_review_remaining
    then return ()
    else Interactive.printf "\nWhole-feature review remaining:\n%s\n"
           (Ascii_table.to_string
              (Line_count_table.create ~show_completed_review:true
                 whole_feature_review_remaining)
              ~display_ascii ~max_output_columns)
  in
  let%bind b =
    Interactive.ask_yn ~default:false
      (sprintf "Do you want to second %s ?"
         (Feature_path.to_string feature_path))
  in
  if b
  then return ()
  else failwith "did not second"
;;

let command =
  Command.async'
    ~summary:"state that a feature is ready to be reviewed by everyone"
    ~readme:(fun () ->
      concat [ "\
The recommended workflow is to find a non-owner whole-feature reviewer to act as the
seconder.  If you need to second even though you are an owner, use:

  $ fe second "; Switch.even_though_owner; " [FEATURE]

For a permanent feature acting as an umbrella feature for development in child
features, one can second it once and for all using:

  $ fe second "; Switch.even_though_empty; " [FEATURE]
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and () = interactive
     and display_ascii = display_ascii
     and even_though_empty = even_though_empty
     and even_though_owner = even_though_owner
     and max_output_columns = max_output_columns
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind () =
         if not !Interactive.interactive
         then return ()
         else begin
           let%bind reaction =
             Prepare_to_second.rpc_to_server_exn
               { feature_path; even_though_empty; even_though_owner }
           in
           dialog ~feature_path ~display_ascii ~max_output_columns reaction
         end
       in
       Second.rpc_to_server_exn
         { feature_path; even_though_empty; even_though_owner  }
    )
;;
