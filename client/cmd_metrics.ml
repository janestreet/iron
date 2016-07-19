open! Core.Std
open! Async.Std
open! Import

let add =
  Command.async'
    ~summary:"add values to a metric"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = absolute_feature_path
     and metric_name = metric_name
     and values = metric_values
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       Metrics.Add_values.rpc_to_server_exn
         { feature_path
         ; metric_name
         ; values
         })
;;

let clear =
  Command.async'
    ~summary:"clear a metric"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and which_features =
       which_features
         ~allow_rec_flag:true
         ~allow_unexisting_features:true
         ~allow_empty_selection:false
         ~default_to_current_bookmark:false
         ()
     and metric_name = metric_name
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind which_features = force which_features in
       Metrics.Clear.rpc_to_server_exn
         { metric_name
         ; which_features
         })
;;

let aggregate_metrics m1 m2 =
  Map.merge m1 m2 ~f:(fun ~key:(_ : Metric_name.t) result ->
    match result with
    | `Left result | `Right result -> Some result
    | `Both (x, y) -> Some (Metric.Snapshot.aggregate x y))
;;

let aggregate_by_feature ~feature_to_metrics_map ~descendants_of ~depth_to_aggregate =
  let increment_depth depth ~by =
    if (Int.max_value - depth) < by
    then Int.max_value
    else depth + by
  in
  let shorten feature_path depth =
    if depth <= 0
    then
      raise_s
        [%sexp "shorten requires depth > 0"
             , { feature_path = (feature_path : Feature_path.t)
               ; depth        = (depth        : int)
               }
        ];
    List.take (Feature_path.parts feature_path) depth
    |> Feature_path.of_parts_exn
  in
  Map.fold feature_to_metrics_map ~init:Feature_path.Map.empty
    ~f:(fun ~key:descendant ~data:metrics acc ->
      let descendant =
        match (descendants_of : Which_ancestor.t) with
        | Any_root ->
          shorten descendant depth_to_aggregate
        | Feature ancestor ->
          shorten descendant
            (increment_depth ~by:(Feature_path.num_parts ancestor) depth_to_aggregate)
      in
      Map.update acc descendant ~f:(fun previous_metrics ->
        aggregate_metrics metrics
          (Option.value previous_metrics ~default:Metric_name.Map.empty)))
;;

let aggregate ~feature_to_metrics_map ~descendants_of ~depth_to_aggregate =
  match descendants_of, depth_to_aggregate with
  | Which_ancestor.Any_root, 0 ->
    `All
      (Map.fold feature_to_metrics_map ~init:Metric_name.Map.empty
         ~f:(fun ~key:(_ : Feature_path.t) ~data acc -> aggregate_metrics data acc))
  | _ ->
    `By_feature
      (aggregate_by_feature ~feature_to_metrics_map ~descendants_of ~depth_to_aggregate)
;;

let metric_names feature_to_metrics_map =
  Map.fold feature_to_metrics_map ~init:Metric_name.Set.empty
    ~f:(fun ~key:_ ~data:metrics metric_names ->
      Map.fold metrics ~init:metric_names
        ~f:(fun ~key:metric_name ~data:_ metric_names ->
          Metric_name.Set.add metric_names metric_name))
;;

let by_stat_feature_table
      (feature_to_metrics_map : Metric.Snapshot.t Metric_name.Map.t Feature_path.Map.t)
      ~decimals ~depth_to_aggregate ~descendants_of ~which_metrics ~stat_type =
  let columns =
    Ascii_table.Column.(
      string ~header:"feature" (cell fst)
      :: List.map which_metrics ~f:(fun metric_name ->
        string ~header:(Metric_name.to_string metric_name) ~align:Right
          (cell (fun (_, metrics) ->
             match Map.find metrics metric_name with
             | None -> ""
             | Some metric ->
               Metric.Snapshot.get_stat_as_string_hum metric stat_type ~decimals))))
  in
  let rows =
    match aggregate ~feature_to_metrics_map ~descendants_of ~depth_to_aggregate with
    | `All metrics -> [ "*", metrics ]
    | `By_feature feature_to_metrics_map ->
      let feature_to_metrics_map =
        let which_metrics = Metric_name.Set.of_list which_metrics in
        Map.filter_map feature_to_metrics_map ~f:(fun metrics ->
          let metrics = Map.filter_keys metrics ~f:(Set.mem which_metrics) in
          Option.some_if (not (Map.is_empty metrics)) metrics)
      in
      Feature_table.create (Map.to_alist feature_to_metrics_map) fst
        (fun ~feature data ->
           match data with
           | None              -> feature, Metric_name.Map.empty
           | Some (_, metrics) -> feature, metrics)
  in
  Ascii_table.create ~columns ~rows
;;

let by_metric_feature_table
      (feature_to_metrics_map : Metric.Snapshot.t Metric_name.Map.t Feature_path.Map.t)
      ~decimals ~depth_to_aggregate ~descendants_of ~which_stats ~metric_name =
  let columns =
    Ascii_table.Column.(
      string ~header:"feature" (cell fst)
      :: List.map which_stats ~f:(fun stat_type ->
        string ~header:(Enum.to_string_hum (module Metric.Stat_type) stat_type)
          ~align:Right (cell (fun (_, metric) ->
            match metric with
            | None -> ""
            | Some metric ->
              Metric.Snapshot.get_stat_as_string_hum metric stat_type ~decimals))))
  in
  let rows =
    let feature_to_metrics_map =
      Map.filter_map feature_to_metrics_map ~f:(fun metrics ->
        match Map.find metrics metric_name with
        | None -> None
        | Some metrics -> Some (Metric_name.Map.singleton metric_name metrics))
    in
    match aggregate ~feature_to_metrics_map ~descendants_of ~depth_to_aggregate with
    | `All metrics -> [ "*", Map.find metrics metric_name ]
    | `By_feature feature_to_metrics_map ->
      let feature_to_metrics_map =
        Map.filter_map feature_to_metrics_map
          ~f:(fun metrics -> Map.find metrics metric_name)
      in
      Feature_table.create (Map.to_alist feature_to_metrics_map) fst
        (fun ~feature data ->
           match data with
           | None             -> feature, None
           | Some (_, metric) -> feature, Some metric)
  in
  Ascii_table.create ~columns ~rows
;;

let descendants_of =
  let open Command.Let_syntax in
  let%map_open absolute_feature_path_option = absolute_feature_path_option in
  Or_error.map absolute_feature_path_option ~f:(function
    | None -> Which_ancestor.Any_root
    | Some feature_path -> Feature feature_path)
;;

let list =
  Command.async'
    ~summary:"list metrics available on the server"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and descendants_of = descendants_of
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let descendants_of = ok_exn descendants_of in
       let%map feature_to_metrics_map =
         Metrics.Get.rpc_to_server_exn { descendants_of }
       in
       Set.iter (metric_names feature_to_metrics_map) ~f:(fun metric_name ->
         print_endline (Metric_name.to_string metric_name)))
;;

let show =
  let show_by_stat_switch = "-show-by-stat" in
  Command.async'
    ~summary:"show statistics of data captured from features"
    ~readme:(fun () -> concat ["\
Metrics are shown for all features in the requested subtree(s), aggregated by -depth.
Tables are drawn by metric by default (i.e. showing all stats for a metric), and by stat
(i.e. count, mean, etc.) if the switch "; show_by_stat_switch ; " is supplied.
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     and descendants_of = descendants_of
     and depth_to_aggregate =
       let%map depth = depth_option in
       Option.value depth ~default:1
     and only_metrics =
       metric_name_regex_list_option ~doc:"REGEX,[REGEX..] select metrics to show"
     and which_stats =
       let%map for_stats_list =
         stat_type_enum_list
           ~doc:(sprintf "STAT,[STAT...] select stats to show %s"
                   (Metric.Stat_type.all
                    |> List.map ~f:(Enum.to_string_hum (module Metric.Stat_type))
                    |> List.sort ~cmp:String.alphabetic_compare
                    |> String.concat ~sep:"|"))
       in
       if List.is_empty for_stats_list
       then Metric.Stat_type.
              (all |> List.sort ~cmp:Compare_by_interest.compare)
       else for_stats_list
     and decimals =
       let default = 2 in
       flag "-decimals" (optional_with_default default int)
         ~doc:(sprintf "NUM round stats to n decimals.  Default %d" default)
     and show_by_stat =
       no_arg_flag show_by_stat_switch ~doc:" draw tables by stat"
     in fun () ->
       let open! Deferred.Let_syntax in
       let show_by_stat = show_by_stat in
       let descendants_of = ok_exn descendants_of in
       let%map feature_to_metrics_map =
         Metrics.Get.rpc_to_server_exn { descendants_of }
       in
       let which_metrics =
         let all_metrics = Set.to_list (metric_names feature_to_metrics_map) in
         match only_metrics with
         | None -> all_metrics
         | Some metric_regex_list ->
           let metric_regex_list = ok_exn metric_regex_list in
           List.filter all_metrics ~f:(fun metric_name ->
             let metric_name = Metric_name.to_string metric_name in
             List.exists metric_regex_list
               ~f:(fun metric_regex -> Regex.matches metric_regex metric_name))
       in
       let print_table ~name table =
         if not (Ascii_table.is_empty table) then begin
           print_endline name;
           print_endline (Ascii_table.to_string table ~display_ascii ~max_output_columns)
         end
       in
       if show_by_stat
       then
         List.iter which_stats ~f:(fun stat_type ->
           print_table ~name:(Enum.to_string_hum (module Metric.Stat_type) stat_type)
             (by_stat_feature_table ~stat_type ~decimals ~depth_to_aggregate
                ~descendants_of ~which_metrics feature_to_metrics_map))
       else
         List.iter which_metrics ~f:(fun metric_name ->
           print_table ~name:(Metric_name.to_string metric_name)
             (by_metric_feature_table ~metric_name ~decimals ~depth_to_aggregate
                ~descendants_of ~which_stats feature_to_metrics_map)))
;;

let subscribe =
  Command.async'
    ~summary:"subscribe to specified metric updates and print them"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path_option = absolute_feature_path_option
     and metric_name_option  = metric_name_option
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path_option = ok_exn feature_path_option in
       let (action : Notify_on_metric_updates.Action.t) =
         match feature_path_option with
         | Some feature_path -> Feature_path feature_path
         | None ->
           match metric_name_option with
           | Some metric_name -> Metric_name metric_name
           | None -> failwith "A least one of a feature path and a metric name \
                               should be supplied"
       in
       let%bind pipe = Notify_on_metric_updates.rpc_to_server_exn action in
       let is_metric_of_interest =
         match action, metric_name_option with
         | Metric_name  _, _
         | Feature_path _, None -> const true
         | Feature_path _, Some metric_name ->
           fun (t : Notify_on_metric_updates.Reaction.t) ->
             Metric_name.equal metric_name t.metric_name
       in
       Pipe.iter_without_pushback pipe ~f:(fun event ->
         let event = ok_exn event in
         if is_metric_of_interest event
         then
           printf !"%{sexp#mach:Notify_on_metric_updates.Reaction.t}\n"
             event)
    )
;;

let command =
  Command.group ~summary:"various commands for collecting data in features"
    [ "add"      , add
    ; "clear"    , clear
    ; "list"     , list
    ; "show"     , show
    ; "subscribe", subscribe
    ]
;;
