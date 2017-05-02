open! Core
open! Async
open! Import

let table { Fe.List.Table.Action.
            features
          ; preserve_input_ordering
          ; display_ascii
          ; max_output_columns
          } =
  let empty_column = [], "" in
  let module Line = struct
    type column = Ascii_table.Attr.t list * string

    type t =
      { feature              : column
      ; feature_id           : column
      ; archived_at          : column
      ; reason_for_archiving : column
      ; lines                : column
      ; next_steps           : column
      }
    [@@deriving fields]

    let empty =
      { feature              = empty_column
      ; feature_id           = empty_column
      ; archived_at          = empty_column
      ; reason_for_archiving = empty_column
      ; lines                = empty_column
      ; next_steps           = empty_column
      }
    ;;
  end in
  let rows =
    Feature_table.create (features : List_features.Reaction.t)
      ?preserve_input_ordering:(if preserve_input_ordering then Some () else None)
      (fun r -> r.feature_path)
      (fun ~feature reaction_option ->
         match reaction_option with
         | None -> { Line.empty with feature = ([], feature) }
         | Some { next_steps
                ; num_lines
                ; review_is_enabled
                ; feature_id
                ; status
                ; _
                } ->
           let feature_id =
             match status with
             | Existing   -> empty_column
             | Archived _ -> [], Feature_id.to_string feature_id
           in
           let archived_at =
             match status with
             | Existing   -> empty_column
             | Archived t -> ([ `Yellow ], Time.to_string t.archived_at)
           in
           let reason_for_archiving =
             match status with
             | Existing   -> empty_column
             | Archived t -> ([], t.reason_for_archiving)
           in
           let lines =
             match status with
             | Archived _ -> empty_column
             | Existing ->
               match num_lines with
               | Pending_since _ -> ([ `Yellow ], "pending")
               | Known (Error _) -> ([ `Red    ], "error")
               | Known (Ok int)  -> ([         ], Int.to_string_hum int)
           in
           let (attr, _) as next_steps =
             match status with
             | Archived _ -> empty_column
             | Existing -> Next_step.to_attrs_and_string next_steps ~review_is_enabled
           in
           { Line.
             feature    = (attr, feature)
           ; feature_id
           ; archived_at
           ; reason_for_archiving
           ; lines
           ; next_steps
           })
  in
  let columns =
    Ascii_table.Column.(
      [ string ~header:"feature"     (attr_cell Line.feature)
      ; string ~header:"feature id"  (attr_cell Line.feature_id)
          ~min_width:Feature_id.length_of_string_repr
      ; string ~header:"archived at" (attr_cell Line.archived_at)
      ; string ~header:"reason for archiving" (attr_cell Line.reason_for_archiving)
          ~truncate_after_n_char:40
      ; string ~header:"lines"       (attr_cell Line.lines) ~align:Right
      ; string ~header:"next step"   (attr_cell Line.next_steps)
      ])
  in
  Ascii_table.to_string
    (Ascii_table.create ~columns ~rows)
    ~display_ascii
    ~max_output_columns
;;

module Table = struct
  let main action =
    return (table action)
  ;;
end

let command =
  Command.async'
    ~summary:"list descendants of a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and descendants_of =
       let%map feature_path_option = feature_path_option in
       Or_error.map feature_path_option ~f:(function
         | None -> Which_ancestor.Any_root
         | Some feature -> Feature feature)
     and use_archived =
       no_arg_flag Switch.archived ~doc:"list archived features"
     and sort_most_recently_archived_first =
       no_arg_flag "-sort-most-recently-archived-first"
         ~doc:(sprintf "show most recently archived features first.  Implies [%s]"
                 Switch.archived)
     and depth = depth_option
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     and name_only =
       no_arg_flag "-name-only" ~doc:"show only the feature names, one per line"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let use_archived = use_archived || sort_most_recently_archived_first in
       let client_config = Client_config.get () in
       let depth =
         Option.first_some depth (Client_config.Cmd.List.depth client_config)
         |> Option.value ~default:1
       in
       let descendants_of = ok_exn descendants_of in
       if name_only && not sort_most_recently_archived_first
       then (
         let%map feature_paths =
           List_feature_names.rpc_to_server_exn { descendants_of; depth; use_archived }
         in
         feature_paths
         |> List.sort ~cmp:Feature_path.compare
         |> List.iter ~f:(fun feature_path ->
           print_endline (Feature_path.to_string feature_path)))
       else (
         let%map features =
           List_features.rpc_to_server_exn { descendants_of; depth; use_archived }
         in
         if not (List.is_empty features)
         then (
           let features =
             let cmp : List_features.Reaction.one -> List_features.Reaction.one -> int =
               if sort_most_recently_archived_first
               then
                 (fun f1 f2 ->
                    match f1.status, f2.status with
                    | Existing, Existing ->
                      Feature_path.compare f1.feature_path f2.feature_path
                    | Existing    , Archived _  -> (-1)
                    | Archived _  , Existing    -> 1
                    | Archived t1 , Archived t2 ->
                      Time.compare t2.archived_at t1.archived_at)
               else
                 (fun (f1 : List_features.Reaction.one) f2 ->
                    Feature_path.compare f1.feature_path f2.feature_path)
             in
             List.sort features ~cmp
           in
           if name_only
           then
             List.iter features ~f:(fun f ->
               print_endline (Feature_path.to_string f.feature_path))
           else (
             let preserve_input_ordering = sort_most_recently_archived_first in
             print_string
               (table { features
                      ; preserve_input_ordering
                      ; display_ascii
                      ; max_output_columns
                      })))))
;;
