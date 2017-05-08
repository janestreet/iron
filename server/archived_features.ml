module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module Archived_feature = struct
    module V1 = struct
      type t = Archived_feature.t =
        { feature_id           : Feature_id.V1.t
        ; feature_path         : Feature_path.V1.t
        ; rev_zero             : Rev.V1.t
        ; owners               : User_name.V1.t list
        ; archived_at          : Time.V1_round_trippable.t
        ; reason_for_archiving : string [@default ""] [@sexp_drop_if Core.String.is_empty]
        }
      [@@deriving sexp]
    end
  end

  module Action = struct
    module V1 = struct
      type t =
        [ `Add    of Archived_feature.V1.t
        | `Remove of Archived_feature.V1.t
        | `Set_max_cache_size of int
        ]
      [@@deriving sexp]
    end

    module Model = V1
  end

  module Action_query = struct
    module V1 = struct
      type t = Action.V1.t Query.V1.t
      [@@deriving sexp]
    end

    module Model = V1
  end
end

open! Core
open! Import

module Action       = Stable.Action.      Model
module Action_query = Stable.Action_query.Model

module Persist = struct
  module Action_query = struct
    include Persistent.Make
        (struct let version = 1 end)
        (Stable.Action_query.V1)
  end
end

module Cached_features = Lru_cache.Make (struct
    include Feature_id
    include (Feature_id.Stable.V1 : Sexpable.S with type t := t)
  end)

type t =
  { features              : Archived_feature.t list Feature_forest.t
  ; features_by_id        : Archived_feature.t Feature_id.Table.t
  ; cached_features       : Iron_protocol.Feature.t Cached_features.t
  ; dynamic_upgrade_state : Dynamic_upgrade.State.t
  ; mutable serializer    : Serializer.t option
  }
[@@deriving fields, sexp_of]

module Cache = struct

  type data = Iron_protocol.Feature.t [@@deriving sexp_of]

  let add t (data : data) =
    Cached_features.set t.cached_features
      ~key:data.feature_id
      ~data
  ;;

  let find t feature_id = Cached_features.find t.cached_features feature_id

  let remove t feature_id =
    ignore (Cached_features.remove t.cached_features feature_id : [ `No_such_key | `Ok ])
  ;;

  let clear t =
    ignore (Cached_features.clear t.cached_features : [ `Dropped of int ]);
  ;;

  module What_to_dump = struct
    type t =
      [ `Stats
      | `Ids_and_feature_paths
      | `Value of Feature_id.t
      ]
  end

  let dump t = function
    | `Stats -> Cached_features.stats t.cached_features
    | `Ids_and_feature_paths ->
      t.cached_features
      |> Cached_features.to_alist
      |> List.map ~f:(fun (feature_id, cached_feature) ->
        (feature_id, cached_feature.feature_path))
      |> List.sort ~cmp:(fun (_, p1) (_, p2) -> Feature_path.compare p1 p2)
      |> [%sexp_of: (Feature_id.t * Feature_path.t) list]
    | `Value feature_id ->
      match Cached_features.find t.cached_features feature_id with
      | None ->
        raise_s
          [%sexp "not in the archived feature cache", (feature_id : Feature_id.t)]
      | Some data -> data |> [%sexp_of: data]
  ;;
end

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~features:(check (Feature_forest.invariant (fun features ->
        List.iter features ~f:Archived_feature.invariant)))
      ~features_by_id:(check (Feature_id.Table.iteri ~f:(fun ~key ~data ->
        [%test_result: Feature_id.t] ~expect:key (Archived_feature.feature_id data))))
      ~cached_features:(check (Cached_features.invariant (ignore : Cache.data -> unit)))
      ~dynamic_upgrade_state:(check Dynamic_upgrade.State.Reference.invariant)
      ~serializer:(check (Option.iter ~f:Serializer.invariant)))
;;

let iteri t ~f = Feature_forest.iteri t.features ~f

let archive_date_zone = Core.Time.Zone.find_exn "America/New_York"

let feature_dir archived_feature =
  let date =
    Time.to_date (Archived_feature.archived_at archived_feature) ~zone:archive_date_zone
  in
  Relpath.of_list
    (List.map ~f:File_name.of_string
       [ sprintf "%4d"  (Date.year date)
       ; sprintf "%02d" (Month.to_int (Date.month date))
       ; sprintf "%02d" (Date.day date)
       ; Feature_id.to_string (Archived_feature.feature_id archived_feature)
       ])
;;

let queries_file = Relpath.of_string "queries"

let serializer_exn t =
  match t.serializer with
  | Some s -> s
  | None -> raise_s [%sexp "serializer isn't defined", (t : t)]
;;

let persist_query t query action =
  let allowed_from =
    match action with
    (* Note to devs: please keep at least this line so that it is easy to add/remove
       lines there *)
    | #Action.t -> Dynamic_upgrade.U1
  in
  match Dynamic_upgrade.commit_to_upgrade t.dynamic_upgrade_state ~allowed_from with
  | `Not_allowed_yet -> ()
  | `Ok ->
    let action =
      let map (archived_feature : Archived_feature.t) =
        if String.is_empty archived_feature.reason_for_archiving
        then archived_feature
        else (
          match
            Dynamic_upgrade.commit_to_upgrade t.dynamic_upgrade_state ~allowed_from:U3
          with
          | `Not_allowed_yet -> { archived_feature with reason_for_archiving = "" }
          | `Ok -> archived_feature)
      in
      match action with
      | `Add    archived_feature -> `Add    (map archived_feature)
      | `Remove archived_feature -> `Remove (map archived_feature)
      | `Set_max_cache_size _ as action -> action
    in
    Serializer.append_to (serializer_exn t) ~file:queries_file
      (Query.with_action query action) (module Persist.Action_query)
;;

let add_internal t archived_feature =
  let feature_path = Archived_feature.feature_path archived_feature in
  Feature_forest.add_ancestors t.features feature_path ~f:(const []);
  Feature_forest.change_exn t.features feature_path (fun list -> archived_feature :: list);
  Hashtbl.set t.features_by_id
    ~key:archived_feature.feature_id ~data:archived_feature;
;;

let add t query archived_feature =
  add_internal t archived_feature;
  persist_query t query (`Add archived_feature);
;;

let remove_internal t (archived_feature : Archived_feature.t) =
  let feature_id = archived_feature.feature_id in
  Feature_forest.change_exn t.features archived_feature.feature_path (fun list ->
    List.filter list ~f:(fun archived_feature ->
      not (Feature_id.equal archived_feature.feature_id feature_id)));
  Cache.remove t feature_id;
  Hashtbl.remove t.features_by_id feature_id;
;;

let remove t query archived_feature =
  remove_internal t archived_feature;
  persist_query t query (`Remove archived_feature);
;;

let set_max_cache_size_internal t ~max_size =
  ignore
    (Cached_features.set_max_size t.cached_features ~max_size : [ `Dropped of int ]);
;;

let set_max_cache_size t query ~max_size =
  set_max_cache_size_internal t ~max_size;
  persist_query t query (`Set_max_cache_size max_size);
;;

let apply_query_internal t query =
  match Query.action query with
  | `Add archived_feature    -> add_internal    t archived_feature
  | `Remove archived_feature -> remove_internal t archived_feature
  | `Set_max_cache_size size -> set_max_cache_size_internal t ~max_size:size
;;

let deserializer ~dynamic_upgrade_state =
  Deserializer.with_serializer (fun serializer ->
    let open Deserializer.Let_syntax in
    let%map_open queries =
      sequence_of (module Persist.Action_query) ~in_file:queries_file
    in
    let t =
      { features        = Feature_forest.create ()
      ; features_by_id  = Feature_id.Table.create ()
      ; cached_features = Cached_features.create ~max_size:500
      ; dynamic_upgrade_state
      ; serializer      = None
      }
    in
    List.iter queries ~f:(fun query -> apply_query_internal t query);
    t.serializer <- Some serializer;
    t)
;;

let find_by_id t feature_id =
  match Hashtbl.find t.features_by_id feature_id with
  | Some feature -> Ok feature
  | None -> error "no archived feature with id" feature_id [%sexp_of: Feature_id.t]
;;

let find_by_path t feature_path =
  match Feature_forest.find t.features feature_path with
  | Error _ -> []
  | Ok list -> list
;;

let mem_feature_path t feature_path =
  is_ok (Feature_forest.find t.features feature_path)
;;

let list_features t ~descendants_of ~depth =
  Or_error.map (Feature_forest.list t.features ~descendants_of ~depth)
    ~f:(fun descendants ->
      List.concat_map descendants ~f:(fun (_, archived_features) ->
        List.map archived_features ~f:Archived_feature.to_list_protocol))
;;

let list_feature_names t ~descendants_of ~depth =
  Or_error.map (Feature_forest.list t.features ~descendants_of ~depth)
    ~f:(fun descendants ->
      List.map descendants ~f:(fun (feature_path, _) -> feature_path))
;;

let complete t ~prefix = Feature_forest.complete t.features ~prefix
