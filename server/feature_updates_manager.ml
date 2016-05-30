open Core.Std
open Async.Std
open Import

module Subscription = struct
  type 'a t =
    { subscriber : User_name.t
    ; writer     : 'a Or_error.t Pipe.Writer.t
    }

  let tick { subscriber = _; writer } ~value =
    Pipe.write_without_pushback_if_open writer (Ok value)
  ;;

  let close { subscriber = _; writer } = Pipe.close writer
end

module Subscribers (Key : sig
    type t [@@deriving sexp_of]
    include Hashable.S with type t := t
  end) : sig
  type 'a t [@@deriving sexp_of]

  val invariant : _ t -> unit

  val dump   : _ t -> Sexp.t
  val create : unit -> _ t
  val tick   : 'a t -> Key.t -> 'a -> unit
  val add
    : 'a t -> _ Query.t -> Key.t -> initial:'a option -> 'a Or_error.t Pipe.Reader.t
  val close  : 'a t -> Key.t -> reason:'a -> unit

end = struct
  type 'a t = 'a Subscription.t Bag.t Key.Table.t

  let sexp_of_t _ t =
    Hashtbl.map t ~f:(fun bag ->
      let decreasing_counts =
        let table = User_name.Table.create () in
        Bag.iter bag ~f:(fun { Subscription. subscriber; writer = _ } ->
          Hashtbl.incr table subscriber);
        table
        |> Hashtbl.to_alist
        |> List.map ~f:(fun (user, value) -> value, user)
        |> List.sort ~cmp:(fun (v1, _) (v2, _) -> Int.compare v2 v1)
      in
      match decreasing_counts with
      | [ one ] -> one |> [%sexp_of: int * User_name.t]
      | _ ->
        [%sexp
          (Bag.length bag : int) :: (decreasing_counts : (int * User_name.t) list)
        ]
    )
    |> Hashtbl.to_alist
    |> List.sort ~cmp:(fun (k1, _) (k2, _) -> Key.compare k1 k2)
    |> [%sexp_of: (Key.t * Sexp.t) list]
  ;;

  let dump t = sexp_of_t () t

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      let no_empty_bags t =
        Hashtbl.iteri t ~f:(fun ~key ~data:bag ->
          if Bag.is_empty bag
          then failwiths "bag is empty for key" key [%sexp_of: Key.t])
      in
      no_empty_bags t)
  ;;

  let create () = Key.Table.create ()

  let tick t key value =
    Option.iter (Hashtbl.find t key)
      ~f:(Bag.iter ~f:(Subscription.tick ~value))
  ;;

  let add t query key ~initial =
    let bag = Hashtbl.find_or_add t key ~default:Bag.create in
    let (reader, writer) = Pipe.create () in
    begin match initial with
    | None -> ()
    | Some v -> Pipe.write_without_pushback writer (Ok v)
    end;
    let subscription = { Subscription. subscriber = Query.by query; writer } in
    let elt = Bag.add bag subscription in
    upon (Pipe.closed writer) (fun () ->
      Bag.remove bag elt;
      if Bag.is_empty bag then Hashtbl.remove t key);
    reader
  ;;

  let close t key ~reason =
    tick t key reason;
    let close_all bag = Bag.iter bag ~f:Subscription.close in
    Option.iter (Hashtbl.find_and_remove t key) ~f:close_all
  ;;
end

module Id_subscribers   = Subscribers (Feature_id)
module Path_subscribers = Subscribers (Feature_path)

module Invalidated_feature = struct
  type t =
    { feature       : Feature.t
    ; feature_paths : Feature_path.Hash_set.t
    }
  [@@deriving fields]

  let create feature =
    { feature
    ; feature_paths = Feature_path.Hash_set.of_list [ Feature.feature_path feature ]
    }
  ;;
end

module Invalidated_features = struct

  type t = Invalidated_feature.t Feature_id.Table.t

  let create () = Feature_id.Table.create ()

  let add t feature =
    let invalidated_feature =
      Hashtbl.find_or_add t (Feature.feature_id feature)
        ~default:(fun () -> Invalidated_feature.create feature)
    in
    Hash_set.add invalidated_feature.feature_paths (Feature.feature_path feature)
  ;;
end

type t =
  { feature_only_subscriptions            : [ `Updated | `Archived ] Id_subscribers.t
  ; feature_and_descendants_subscriptions
    : Iron_protocol.Notify_on_descendant_updates.Reaction.t Path_subscribers.t
  ; invalidated_features                  : Invalidated_features.t sexp_opaque
  ; children_of : Feature_path.t -> Feature.t list
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~feature_only_subscriptions:(check Id_subscribers.invariant)
      ~feature_and_descendants_subscriptions:(check Path_subscribers.invariant)
      ~invalidated_features:ignore
      ~children_of:ignore)
;;

let dump
   { feature_only_subscriptions
   ; feature_and_descendants_subscriptions
   ; invalidated_features                  = _
   ; children_of                           = _
   } =
  [%sexp
    { feature_only_subscriptions
      = (Id_subscribers.dump feature_only_subscriptions : Sexp.t)
    ; feature_and_descendants_subscriptions
      = (Path_subscribers.dump feature_and_descendants_subscriptions : Sexp.t)
    }
  ]
;;

let create ~children_of () =
  { feature_only_subscriptions            = Id_subscribers.create ()
  ; feature_and_descendants_subscriptions = Path_subscribers.create ()
  ; invalidated_features                  = Invalidated_features.create ()
  ; children_of
  }
;;

let broadcast_updates t =
  let features_to_tick =
    (* Tick children of updated features because their next steps might have changed. *)
    let htbl = Hashtbl.copy t.invalidated_features in
    Hashtbl.iter t.invalidated_features ~f:(fun invalidated_feature ->
      (* In theory, when looking by [feature_path] at the end of a RPC we might find a
         different feature than the one that was originally invalidated during the RPC.
         If we have jane/a/a, and jane/a is compressed, then jane/a now refers to a
         different feature.  In practice the children are modified anyway, so thanks to
         the consolidation of updates in [broadcast] that shouldn't cause any additional
         `Updates. *)
      match t.children_of (Feature.feature_path invalidated_feature.feature) with
      | exception _ -> ()
      | children ->
        List.iter children ~f:(fun child_feature ->
          Invalidated_features.add htbl child_feature));
    htbl
  in
  let feature_paths_to_tick_with_updates_in_subtree = Feature_path.Hash_set.create () in
  let rec mark_feature_and_ancestors feature_path =
    Hash_set.add feature_paths_to_tick_with_updates_in_subtree feature_path;
    match Feature_path.parent feature_path with
    | Error _ -> ()
    | Ok feature_path -> mark_feature_and_ancestors feature_path
  in
  Hashtbl.iteri features_to_tick ~f:(fun ~key:feature_id ~data:invalidated_feature ->
    Id_subscribers.tick t.feature_only_subscriptions feature_id `Updated;
    Hash_set.iter invalidated_feature.feature_paths ~f:mark_feature_and_ancestors);
  Hash_set.iter feature_paths_to_tick_with_updates_in_subtree ~f:(fun feature_path ->
    Path_subscribers.tick t.feature_and_descendants_subscriptions
      feature_path `Updates_in_subtree);
  Hashtbl.clear t.invalidated_features
;;

let subscribe_feature_only t query feature_id ~when_to_first_notify =
  Id_subscribers.add t.feature_only_subscriptions query feature_id
    ~initial:(match (when_to_first_notify : When_to_first_notify.t) with
              | Now -> Some `Updated
              | At_next_change -> None)
;;

let subscribe_feature_and_descendants t query feature_path ~when_to_first_notify =
  Path_subscribers.add t.feature_and_descendants_subscriptions query feature_path
    ~initial:(match (when_to_first_notify : When_to_first_notify.t) with
              | Now -> Some `Updates_in_subtree
              | At_next_change -> None)
;;

let on_update t feature = Invalidated_features.add t.invalidated_features feature
;;

let on_archive t feature =
  on_update t feature;
  Id_subscribers.close t.feature_only_subscriptions
    (Feature.feature_id feature) ~reason:`Archived;
  Path_subscribers.close t.feature_and_descendants_subscriptions
    (Feature.feature_path feature) ~reason:`Archived;
;;

let on_rename t feature =
  on_update t feature;
  Path_subscribers.close t.feature_and_descendants_subscriptions
    (Feature.feature_path feature) ~reason:`Renamed;
;;
