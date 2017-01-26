module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module Properties = struct
    module V1 = struct
      type t =
        { by_rev_size : int }
      [@@deriving fields, sexp]
    end
    module Model = V1
  end
end

open! Core
open! Import

module Lru = Lru_cache.Make (Node_hash.First_12)

module Persist = struct
  module Properties = struct
    include Persistent.Make
        (struct let version = 1 end)
        (Stable.Properties.V1)
  end
  let properties_file = Relpath.of_string "properties"
end

module Properties = struct
  include Stable.Properties.Model

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      let positive t = assert (t > 0) in
      Fields.iter
        ~by_rev_size:(check positive))
  ;;

  let default =
    { by_rev_size = 10 }
  ;;
end

module Push_event = struct
  type t =
    { query           : Iron_protocol.Push_events.Add.Action.t Query.t
    ; feature_path    : Feature_path.t
    ; used_by_metrics : Metric_name.Hash_set.t
    }
  [@@deriving fields, sexp_of]

  let create query ~feature_path () =
    { query
    ; feature_path
    ; used_by_metrics = Metric_name.Hash_set.create ()
    }
  ;;

  let by t = Query.by t.query
  let at t = Query.at t.query

  let mark_as_used_by_metrics t metric_name =
    match Hash_set.strict_add t.used_by_metrics metric_name with
    | Error _ -> `Already_marked
    | Ok ()   -> `Ok
  ;;

  let sexp_of_t { query; feature_path; used_by_metrics } =
    [%sexp
      { feature_path    : Feature_path.t
      ; by              = (Query.by query : User_name.t)
      ; at              = (Query.at query : Time.t)
      ; used_by_metrics : Metric_name.Hash_set.t
      }
    ]
  ;;
end

type t =
  { push_events        : Push_event.t Lru.t Feature_id.Table.t
  ; mutable properties : Properties.t
  ; serializer         : Serializer.t
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let { push_events
        ; properties
        ; serializer = _ } = t in
    Properties.invariant properties;
    Hashtbl.iteri push_events ~f:(fun ~key:feature_id ~data:by_rev ->
      Feature_id.invariant feature_id;
      List.iter (Lru.to_alist by_rev) ~f:(fun (rev, query) ->
        Query.invariant
          (fun { Iron_protocol.Push_events.Add.Action.
                 feature_id = feature_id'
               ; tip
               } ->
            Node_hash.First_12.invariant rev;
            [%test_result: Feature_id.t] ~expect:feature_id feature_id';
            [%test_result: Node_hash.First_12.t] ~expect:rev
              (Rev.to_first_12 tip);
          )
          query.query;
      )))
;;

let dump_events_of_user t user_name =
  Feature_id.Map.of_hashtbl_exn t.push_events
  |> Map.filter_map ~f:(fun by_rev_map ->
    let by_rev_map =
      by_rev_map
      |> Lru.to_alist
      |> Node_hash.First_12.Map.of_alist_exn
      |> Map.filter ~f:(fun query ->
        User_name.equal user_name (Query.by query.query))
    in
    if Map.is_empty by_rev_map
    then None
    else Some by_rev_map)
  |> [%sexp_of: Push_event.t Node_hash.First_12.Map.t Feature_id.Map.t]
;;

let dump t (what_to_dump : Iron_protocol.Push_events.What_to_dump.t) =
  match what_to_dump with
  | `Feature_id feature_id ->
    [%sexp (Hashtbl.find t.push_events feature_id : Push_event.t Lru.t option)]
  | `User_name user_name -> dump_events_of_user t user_name
  | `Values ->
    [%sexp (t.push_events : Push_event.t Lru.t Feature_id.Table.t)]
  | `Stats ->
    let users = User_name.Table.create () in
    let feature_ids = Feature_id.Hash_set.create () in
    let total_rev_count = ref 0 in
    Hashtbl.iteri t.push_events ~f:(fun ~key:feature_id ~data:by_rev ->
      Hash_set.add feature_ids feature_id;
      List.iter (Lru.to_alist by_rev) ~f:(fun (_rev, query) ->
        Hashtbl.incr users (Query.by query.query);
        incr total_rev_count;
      ));
    let { Properties. by_rev_size } = t.properties in
    [%sexp
      { by_rev_size      = (by_rev_size                 : int)
      ; feature_id_count = (Hash_set.length feature_ids : int)
      ; total_rev_count  = (!total_rev_count            : int)
      ; users =
          (Hashtbl.length users : int)
        , (User_name.Map.of_hashtbl_exn users : int User_name.Map.t)
      }
    ]
;;

let deserializer = Deserializer.with_serializer (fun serializer ->
  let open Deserializer.Let_syntax in
  let%map_open () = return ()
  and properties =
    one ~default:Properties.default (module Persist.Properties)
      ~in_file:Persist.properties_file
  in
  { push_events     = Feature_id.Table.create ()
  ; properties
  ; serializer
  }
)
;;

let add t query ~feature_path =
  let { Iron_protocol.Push_events.Add.Action.feature_id ; tip } =
    Query.action query
  in
  let by_rev =
    Hashtbl.find_or_add t.push_events feature_id
      ~default:(fun () -> Lru.create ~max_size:t.properties.by_rev_size)
  in
  let key = Rev.to_first_12 tip in
  if not (Lru.mem by_rev key)
  then Lru.set by_rev ~key ~data:(Push_event.create query ~feature_path ())
;;

let persist_properties t properties =
  t.properties <- properties;
  Serializer.set_contents t.serializer t.properties (module Persist.Properties)
    ~file:Persist.properties_file
;;

let change_by_rev_size t by_rev_size =
  if by_rev_size <= 0
  then raise_s [%sexp "invalid by_rev_size", (by_rev_size : int)];
  Hashtbl.iter t.push_events ~f:(fun by_rev ->
    ignore ((Lru.set_max_size by_rev ~max_size:by_rev_size): [`Dropped of int]));
  persist_properties t { by_rev_size }
;;

let change t (update : Iron_protocol.Push_events.Change.Action.t) =
  match update with
  | Set_max_size_per_feature max_size -> change_by_rev_size t max_size
  | Clear_all -> Hashtbl.clear t.push_events
  | Clear_features feature_ids ->
    List.iter feature_ids ~f:(Hashtbl.remove t.push_events)
  | Clear_revs revs ->
    Hashtbl.filter_inplace t.push_events ~f:(fun by_rev ->
      List.iter revs ~f:(fun rev ->
        ignore (Lru.remove by_rev (Rev.to_first_12 rev) : [ `Ok | `No_such_key ]));
      not (Lru.is_empty by_rev))
  | Clear_users users ->
    let users = User_name.Set.of_list users in
    Hashtbl.filter_inplace t.push_events ~f:(fun by_rev ->
      (Lru.to_alist by_rev
       |> List.filter_map ~f:(fun (rev, event) ->
         if User_name.Set.mem users (Push_event.by event)
         then Some rev
         else None)
       |> List.iter ~f:(fun rev ->
         ignore (Lru.remove by_rev rev : [ `Ok | `No_such_key ])));
      not (Lru.is_empty by_rev))
;;

let find t tip ~feature_id =
  match Hashtbl.find t.push_events feature_id with
  | None        -> None
  | Some by_rev -> Lru.find by_rev tip
;;
