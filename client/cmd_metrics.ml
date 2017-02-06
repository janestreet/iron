open! Core
open! Async
open! Import

module Stat_column = struct
  module Percentile = struct
    type t = int [@@deriving compare, sexp_of]

    let all = [ 0; 5; 15; 25; 50; 75; 90; 95; 99 ]
  end

  module T = struct
    type t =
      | Percentile of Percentile.t
      | Count
    [@@deriving compare, enumerate]

    let sexp_of_t = function
      | Percentile p -> Sexp.Atom (sprintf "%d%%" p)
      | Count        -> Sexp.Atom "count"
    ;;
  end

  module Compare_by_interest = struct
    type t = T.t [@@deriving sexp_of]

    let compare = T.compare
  end

  include T
  include Comparable.Make_plain (T)

  let to_string_hum = Enum.to_string_hum (module T)
end

module Dimension = struct

  type t =
    | Int
    | Span
    | Other
  [@@deriving sexp_of]

  let of_metric ~metric_name =
    if Metric_name.(equal hydra_synchronize_state_latency  metric_name
                    || equal hydra_update_bookmark_latency metric_name)
    then Span
    else Other
  ;;

  let of_metric_stat ~metric_name ~stat_column =
    match stat_column with
    | None -> of_metric ~metric_name
    | Some stat_column ->
      match Stat_column.to_string_hum stat_column with
      | "count" -> Int
      | _ -> of_metric ~metric_name
  ;;
end

module Appendable_list = Core_extended.Appendable_list

module Values = struct
  type t = float Appendable_list.t

  let init (data_points : Metric.Data_point.t list) : t =
    data_points
    |> List.map ~f:Metric.Data_point.value
    |> Appendable_list.of_list
  ;;

  let aggregate = Appendable_list.append
end

module Stats = struct
  type t = (Stat_column.t * float) list [@@deriving sexp_of]

  let compute (t : Values.t) =
    let array = t |> Appendable_list.to_sequence |> Sequence.to_array in
    Array.sort array ~cmp:Float.compare;
    let length = Array.length array in
    List.map Stat_column.all ~f:(fun stat ->
      let value =
        match stat with
        | Count -> Int.to_float length
        | Percentile index ->
          if length = 0
          then Float.nan
          else (
            let index = Int.to_float (index * length) /. 100. in
            array.(max 0 (min (Int.of_float index) (pred length))))
      in
      stat, value)
  ;;

  let get_column t stat_column =
    List.Assoc.find t stat_column ~equal:Stat_column.equal
  ;;
end

let pp_value metric_name ~decimals ~stat_column value =
  match Dimension.of_metric_stat ~metric_name ~stat_column with
  | Int   -> value |> Int.of_float |> Int.to_string_hum
  | Span  -> Time.Span.(to_string_hum ~decimals (of_sec value))
  | Other -> Float.to_string_hum ~decimals ~strip_zero:false value
;;

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
    | `Both (x, y) -> Some (Values.aggregate x y))
;;

let increment_depth ~depth ~by =
  if Int.max_value - depth <= by
  then Int.max_value
  else depth + by
;;

let aggregate_by_feature ~feature_to_metrics_map ~descendants_of ~depth_to_aggregate =
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
            (increment_depth ~depth:depth_to_aggregate
               ~by:(Feature_path.num_parts ancestor))
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

let by_metric_feature_table
      (feature_to_metrics_map : Metric.Data_point.t list Metric_name.Map.t Feature_path.Map.t)
      ~decimals ~depth_to_aggregate ~descendants_of ~which_stats ~metric_name =
  let columns =
    Ascii_table.Column.(
      string ~header:"feature" (cell fst)
      :: List.map which_stats ~f:(fun stat_column ->
        string ~header:(Enum.to_string_hum (module Stat_column) stat_column)
          ~align:Right (cell (fun (_, metric) ->
            match metric with
            | None -> ""
            | Some metric ->
              match Stats.get_column metric stat_column with
              | None -> ""
              | Some value ->
                pp_value metric_name ~decimals
                  ~stat_column:(Some stat_column) value))))
  in
  let rows =
    let feature_to_metrics_map =
      Map.filter_map feature_to_metrics_map ~f:(fun metrics ->
        match Map.find metrics metric_name with
        | None -> None
        | Some metrics ->
          Some (Metric_name.Map.singleton metric_name (Values.init metrics)))
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
  let rows =
    List.map rows ~f:(fun (row, snapshots) ->
      row, Option.map ~f:Stats.compute snapshots)
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

let get =
  Command.async'
    ~summary:"get the raw list of metric data points captured from features"
    ~readme:(fun () -> concat ["\
This is a raw command to access the most recent internal data points directly.
To display stats on those values, see [show].
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     and descendants_of = descendants_of
     and only_metrics =
       metric_name_regex_list_option ~doc:"REGEX,[REGEX..] select metrics to show"
     and depth = depth_option
     and decimals =
       let default = 2 in
       flag "-decimals" (optional_with_default default int)
         ~doc:(sprintf "NUM round stats to n decimals.  Default %d" default)
     in fun () ->
       let open! Deferred.Let_syntax in
       let only_metrics = Option.map only_metrics ~f:ok_exn in
       let descendants_of = ok_exn descendants_of in
       let%map reaction =
         let%map features_map =
           Metrics.Get.rpc_to_server_exn { descendants_of }
         in
         let cut_depth =
           match descendants_of with
           | Any_root -> Option.value depth ~default:Int.max_value
           | Feature feature ->
             let num_parts = Feature_path.num_parts feature in
             match depth with
             | None       -> num_parts
             | Some depth -> increment_depth ~depth ~by:num_parts
         in
         Map.filter_keys features_map ~f:(fun feature_path ->
           Feature_path.num_parts feature_path <= cut_depth)
       in
       let print_table feature_path metric_name data_points =
         let columns =
           Ascii_table.Column.(
             [ string ~header:"at" (cell (fun data_point ->
                 Time.to_string (Metric.Data_point.at data_point)))
             ; string ~header:"value" (cell (fun data_point ->
                 let value = Metric.Data_point.value data_point in
                 pp_value metric_name ~decimals ~stat_column:None value))
             ])
         in
         let table = Ascii_table.create ~columns ~rows:data_points in
         if not (Ascii_table.is_empty table)
         then (
           print_endline (sprintf "%s: %s"
                            (Feature_path.to_string feature_path)
                            (Metric_name.to_string metric_name));
           print_endline (Ascii_table.to_string table ~display_ascii ~max_output_columns))
       in
       let include_metric_name =
         match only_metrics with
         | None -> const true
         | Some metric_regex_list ->
           fun metric_name ->
             let metric_name = Metric_name.to_string metric_name in
             List.exists metric_regex_list
               ~f:(fun metric_regex -> Regex.matches metric_regex metric_name)
       in
       Map.iteri reaction ~f:(fun ~key:feature_path ~data:metrics ->
         Map.iteri metrics ~f:(fun ~key:metric_name ~data:data_points ->
           if include_metric_name metric_name
           then print_table feature_path metric_name data_points)))
;;

let show =
  Command.async'
    ~summary:"show statistics of data captured from features"
    ~readme:(fun () -> concat ["\
Metrics are shown for all features in the requested subtree(s), aggregated by -depth.
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     and descendants_of = descendants_of
     and depth_to_aggregate = depth_option
     and only_metrics =
       metric_name_regex_list_option ~doc:"REGEX,[REGEX..] select metrics to show"
     and which_stats =
       let for_stats_doc =
         sprintf "STAT,[STAT...] select stats to show %s"
           (Stat_column.all
            |> List.sort ~cmp:Stat_column.Compare_by_interest.compare
            |> List.map ~f:(Enum.to_string_hum (module Stat_column))
            |> String.concat ~sep:"|")
       in
       let%map for_stats_list =
         map (enum_list "-stats" ~doc:for_stats_doc (module Stat_column))
           ~f:Stat_column.Set.stable_dedup_list
       in
       if List.is_empty for_stats_list
       then Stat_column.
              (all |> List.sort ~cmp:Compare_by_interest.compare)
       else for_stats_list
     and decimals =
       let default = 2 in
       flag "-decimals" (optional_with_default default int)
         ~doc:(sprintf "NUM round stats to n decimals.  Default %d" default)
     in fun () ->
       let open! Deferred.Let_syntax in
       let descendants_of = ok_exn descendants_of in
       let depth_to_aggregate =
         let default =
           match descendants_of with
           | Any_root  -> 1
           | Feature _ -> 0
         in
         Option.value depth_to_aggregate ~default
       in
       let only_metrics = Option.map only_metrics ~f:ok_exn in
       let%map feature_to_metrics_map =
         Metrics.Get.rpc_to_server_exn { descendants_of }
       in
       let which_metrics =
         let all_metrics = Set.to_list (metric_names feature_to_metrics_map) in
         match only_metrics with
         | None -> all_metrics
         | Some metric_regex_list ->
           List.filter all_metrics ~f:(fun metric_name ->
             let metric_name = Metric_name.to_string metric_name in
             List.exists metric_regex_list
               ~f:(fun metric_regex -> Regex.matches metric_regex metric_name))
       in
       let print_table ~name table =
         if not (Ascii_table.is_empty table)
         then (
           print_endline name;
           print_endline (Ascii_table.to_string table ~display_ascii ~max_output_columns))
       in
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
    ; "get"      , get
    ; "list"     , list
    ; "show"     , show
    ; "subscribe", subscribe
    ]
;;
