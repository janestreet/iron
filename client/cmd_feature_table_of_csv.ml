open Core
open Async
open Import

module Delimited = Async_extended.Std.Delimited

let array_safe_get array i =
  if Array.length array > i then Some array.(i) else None
;;

let get_alignment rows i =
  let is_numeric s =
    match Int.of_string s with
    | exception _ -> false
    | _ -> true
  in
  if List.for_all rows ~f:(fun (row : _ Fe.Feature_table_of_csv.Row.t) ->
    match array_safe_get row.other_columns i with
    | None -> true
    | Some contents -> String.is_empty contents || is_numeric contents)
  then Ascii_table.Align.Right
  else Left
;;

let get_columns (headers : _ Fe.Feature_table_of_csv.Row.t) data =
  let feature_path_column =
    Ascii_table.Column.string ~header:headers.feature_path ~align:Left
      (Ascii_table.Column.cell fst)
  in
  let other_columns =
    List.mapi (Array.to_list headers.other_columns) ~f:(fun i header ->
      Ascii_table.Column.string ~header ~align:(get_alignment data i)
        (Ascii_table.Column.cell
           (fun (_, data) ->
              Option.value ~default:""
                (Option.bind data ~f:(fun data ->
                   array_safe_get (Fe.Feature_table_of_csv.Row.other_columns data) i)))))
  in
  feature_path_column :: other_columns
;;

let main' { Fe.Feature_table_of_csv.Action.
            headers
          ; data
          ; preserve_input_ordering
          ; display_ascii
          ; max_output_columns
          } =
  let columns = get_columns headers data in
  let rows =
    Feature_table.create
      ?preserve_input_ordering:(Option.some_if preserve_input_ordering ())
      data
      Fe.Feature_table_of_csv.Row.feature_path
      (fun ~feature data -> feature, data)
  in
  Ascii_table.to_string (Ascii_table.create ~rows ~columns)
    ~display_ascii ~max_output_columns
;;

let main action = return (main' action)
;;

let command =
  Command.async' ~summary:"print tabular data indexed by feature name"
    ~readme:(fun () -> "\
This commands reads comma separated values from stdin and outputs the data in the form of
an Iron feature table on stdout.

The first line of input should be a header.  The first column should be feature names.

Rows will be re-ordered by increasing feature name, but duplicates will be preserved.")
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and sep =
       flag "-sep" (optional char)
         ~doc:"CHAR delimits adjacent columns.  Default is ','"
     and max_output_columns = max_output_columns
     and display_ascii = display_ascii
     in
     fun () ->
       let open! Deferred.Let_syntax in
       match%map
         Pipe.to_list (Delimited.Csv.of_reader ?sep ~header:`Yes (Lazy.force Reader.stdin))
       with
       | [] -> () (* Do not output any table if input is empty. *)
       | (car :: _) as rows ->
         let headers =
           Delimited.Row.headers car
           |> Hashtbl.to_alist
           |> List.map ~f:Tuple.T2.swap
           |> Int.Map.of_alist_exn
           |> Map.data
           |> function
           | [] ->
             failwith "invalid headers: expected at least one header for feature path"
           | feature_path :: other_columns ->
             { Fe.Feature_table_of_csv.Row.
               feature_path
             ; other_columns = Array.of_list other_columns
             }
         in
         let data =
           List.mapi rows ~f:(fun i row ->
             let feature_path =
               try Delimited.Row.nth_conv_exn row 0 [%here] Feature_path.of_string with
               | _ ->
                 raise_s
                   [%sexp
                     (sprintf "invalid row(%d): first column must be valid \
                               feature path"
                        (succ i) : string)
                   , (Delimited.Row.to_list row : string list)
                   ]
             in
             let other_columns =
               let all_columns = Delimited.Row.to_array row in
               let length = Array.length all_columns in
               if length <= 1
               then [||]
               else Array.sub all_columns ~pos:1 ~len:(pred length)
             in
             { Fe.Feature_table_of_csv.Row. feature_path; other_columns })
         in
         main'
           { headers
           ; data
           ; preserve_input_ordering = false
           ; display_ascii
           ; max_output_columns
           }
         |> printf "%s"
    )
;;
