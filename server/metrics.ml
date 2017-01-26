open! Core
open! Import

module Metric_name_subscriptions  = Event_subscriptions_manager.Make (Metric_name)
module Feature_path_subscriptions = Event_subscriptions_manager.Make (Feature_path)

module Event = struct
  type t = Iron_protocol.Notify_on_metric_updates.Reaction.t =
    { feature_path : Feature_path.t
    ; metric_name  : Metric_name.t
    ; value        : float
    ; added_at     : Time.t
    }
  [@@deriving sexp_of]
end

type t =
  { forest                     : Metric.t Metric_name.Table.t Feature_forest.t
  ; metric_name_subscriptions  : Event.t Metric_name_subscriptions.t
  ; feature_path_subscriptions : Event.t Feature_path_subscriptions.t
  }
[@@deriving sexp_of]

let dump_subscriptions t =
  [%sexp
    { metric_name_subscriptions
      = (Metric_name_subscriptions.dump t.metric_name_subscriptions : Sexp.t)
    ; feature_path_subscriptions
      = (Feature_path_subscriptions.dump t.feature_path_subscriptions : Sexp.t)
    }
  ]
;;

let create () =
  { forest                     = Feature_forest.create ()
  ; metric_name_subscriptions  = Metric_name_subscriptions.create ()
  ; feature_path_subscriptions = Feature_path_subscriptions.create ()
  }
;;

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let { forest
        ; metric_name_subscriptions
        ; feature_path_subscriptions
        } = t in
    Feature_forest.iteri forest ~f:(fun feature_path metrics ->
      Feature_path.invariant feature_path;
      Hashtbl.iter metrics ~f:(Metric.invariant);
    );
    Metric_name_subscriptions.invariant metric_name_subscriptions;
    Feature_path_subscriptions.invariant feature_path_subscriptions;
  )
;;

let clear (t : t) { Iron_protocol.Metrics.Clear.Action.
                    metric_name
                  ; which_features
                  } =
  let clear metrics = Hashtbl.remove metrics metric_name in
  match which_features with
  | All_features -> Feature_forest.iteri t.forest ~f:(fun _ metrics -> clear metrics)
  | Features features ->
    List.iter features ~f:(fun { feature_path; include_descendants } ->
      if include_descendants
      then Feature_forest.iter_descendants t.forest feature_path     ~f:clear
      else Or_error.iter (Feature_forest.find t.forest feature_path) ~f:clear)
;;

let get (t : t) ({ descendants_of } : Iron_protocol.Metrics.Get.Action.t) =
  Feature_forest.list t.forest ~descendants_of ~depth:Int.max_value
  |> ok_exn
  |> Feature_path.Map.of_alist_exn
  |> Map.map ~f:(fun metrics ->
    metrics
    |> Metric_name.Map.of_hashtbl_exn
    |> Map.map ~f:Metric.data_points)
;;

let complete t ~prefix =
  let add_if_prefix set metric_name =
    let name = Metric_name.to_string metric_name in
    if String.is_prefix ~prefix name
    then String.Set.add set name
    else set
  in
  Feature_forest.list t.forest ~descendants_of:Any_root ~depth:Int.max_value
  |> ok_exn
  |> List.fold ~init:String.Set.empty ~f:(fun set (_, metric_table) ->
    Hashtbl.fold metric_table ~init:set ~f:(fun ~key ~data:(_ : Metric.t) set ->
      add_if_prefix set key))
  |> Set.to_list
;;

let add_values t
      { Iron_protocol.Metrics.Add_values.Action.
        feature_path
      ; metric_name
      ; values
      } =
  if List.is_empty values
  then
    raise_s
      [%sexp "Metrics.add_values: empty values list", [%here]
             , { feature_path = (feature_path : Feature_path.t)
               ; metric_name  = (metric_name  : Metric_name.t)
               }
      ];
  let metric =
    Feature_forest.add_ancestors t.forest feature_path ~f:Metric_name.Table.create;
    let metrics = ok_exn (Feature_forest.find t.forest feature_path) in
    Hashtbl.find_or_add metrics metric_name ~default:Metric.create
  in
  let added_at = Time.now () in
  List.iter values ~f:(fun value ->
    Metric.add metric { at = added_at; value };
    let event =
      { Event.
        feature_path
      ; metric_name
      ; value
      ; added_at
      }
    in
    Feature_path_subscriptions.tick t.feature_path_subscriptions feature_path event;
    Metric_name_subscriptions.tick  t.metric_name_subscriptions  metric_name  event;
  )
;;

let subscribe t query action =
  match (action : Iron_protocol.Notify_on_metric_updates.Action.t) with
  | Feature_path feature_path ->
    Feature_path_subscriptions.add t.feature_path_subscriptions query feature_path
      ~initial:None
  | Metric_name metric_name ->
    Metric_name_subscriptions.add t.metric_name_subscriptions query metric_name
      ~initial:None
;;
