module Stable = struct
  open Core.Core_stable

  module Properties = struct
    module V1 = struct
      type t =
        { max_subscriptions_per_user : int
        ; max_subscriptions_global   : int
        }
      [@@deriving sexp]
    end

    module Model = V1
  end
end

open Core
open Async
open Import

module Subscription_t = struct
  type t = T : 'a unpacked * ('a -> Sexp.t) -> t
  and 'a unpacked =
    { rpc_name              : string
    ; rpc_version           : int
    ; query                 : 'a Query.t
    ; closed                : [ `Removed
                              | `Dropped of Error.t
                              ] Ivar.t
    ; opened_at             : Time.t
    ; mutable ticks         : int
    }
  [@@deriving fields]

  let sexp_of_t (T (t, sexp_of_action)) =
    let { rpc_name
        ; rpc_version
        ; query
        ; closed        = _
        ; opened_at
        ; ticks
        } = t
    in
    (* The query is at the end because it might be big and the short field are easier
       to spot when put at the beginning *)
    [%sexp
      { rpc_name    : string
      ; rpc_version : int
      ; opened_at   : Time.t
      ; ticks       : int
      ; query       : action Query.t
      }
    ]
  ;;

  let invariant ((T (unpacked, _)) as t) =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field unpacked f in
      let non_negative t = (assert (t >= 0)) in
      Fields_of_unpacked.iter
        ~rpc_name:(check ignore)
        ~rpc_version:(check ignore)
        ~query:(check ignore)
        ~closed:(check ignore)
        ~opened_at:(check ignore)
        ~ticks:(check non_negative))
  ;;

  let create ~rpc_name ~rpc_version query ~sexp_of_action =
    let unpacked =
      { rpc_name
      ; rpc_version
      ; query
      ; closed        = Ivar.create ()
      ; opened_at     = Time.now ()
      ; ticks         = 0
      }
    in
    T (unpacked, sexp_of_action)
  ;;

  let closed (T (t, _)) =
    Ivar.read t.closed
  ;;

  let tick (T (t, _)) =
    t.ticks <- t.ticks + 1
  ;;

  let by (T (t, _)) =
    Query.by t.query
  ;;

  let close (T (t, _)) value =
    Ivar.fill_if_empty t.closed value
  ;;
end

module Subscription = struct
  type t =
    { mutable elt  : Subscription_t.t Bag.Elt.t option
    ; subscription : Subscription_t.t
    }

  let tick   t = Subscription_t.tick   t.subscription
  let closed t = Subscription_t.closed t.subscription

  let create elt = { elt = Some elt; subscription = Bag.Elt.value elt }
end

module Persist = struct
  module Properties = struct
    include Persistent.Make
        (struct let version = 1 end)
        (Stable.Properties.V1)
  end
  let properties_file = Relpath.of_string "properties"
end

module Properties = struct
  type t = Stable.Properties.Model.t =
    { max_subscriptions_per_user : int
    ; max_subscriptions_global   : int
    }
  [@@deriving fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      let non_negative t = assert (t >= 0) in
      Fields.iter
        ~max_subscriptions_per_user:(check non_negative)
        ~max_subscriptions_global:(check non_negative))
  ;;

  let default =
    { max_subscriptions_per_user = 50
    ; max_subscriptions_global   = 500
    }
  ;;
end

type t =
  { mutable properties                : Properties.t
  ; subscriptions                     : Subscription_t.t Bag.t
  ; subscription_counts               : int User_name.Table.t
  ; mutable subscription_count_global : int
  ; mutable serializer                : Serializer.t
  }
[@@deriving fields, sexp_of]

let invariant t =
  (* There is no invariant that the counts are smaller than the maximums, because when we
     change the maximums, we don't drop subscriptions. *)
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~properties:(check Properties.invariant)
      ~subscriptions:(check (Bag.iter ~f:Subscription_t.invariant))
      ~subscription_counts:(check (fun subscription_counts ->
        let subscription_counts =
          User_name.Map.of_alist_exn (Hashtbl.to_alist subscription_counts)
        in
        let subscription_counts' =
          List.map (Bag.to_list t.subscriptions) ~f:(fun sub -> Subscription_t.by sub, ())
          |> User_name.Map.of_alist_fold ~init:0 ~f:(fun acc () -> acc + 1)
        in
        [%test_result: int User_name.Map.t]
          subscription_counts ~expect:subscription_counts'))
      ~subscription_count_global:(check (fun n ->
        [%test_result: int] n ~expect:(Bag.length t.subscriptions)))
      ~serializer:ignore)
;;

let add t ~rpc_name ~rpc_version query ~sexp_of_action =
  let by = Query.by query in
  let user_subscription_count =
    Option.value (Hashtbl.find t.subscription_counts by) ~default:0
  in
  if user_subscription_count >= t.properties.max_subscriptions_per_user then
    Or_error.errorf "user %s exceeded the per-user feature update subscription limit (%d)"
      (User_name.to_string by) t.properties.max_subscriptions_per_user
  else if t.subscription_count_global >= t.properties.max_subscriptions_global then
    Or_error.errorf
      "exceeded the global maximum number of feature update subscriptions (%d)"
      t.properties.max_subscriptions_global
  else (
    let subscription =
      Subscription_t.create ~rpc_name ~rpc_version query ~sexp_of_action
    in
    let elt = Bag.add t.subscriptions subscription in
    Hashtbl.set t.subscription_counts ~key:by ~data:(user_subscription_count + 1);
    t.subscription_count_global <- t.subscription_count_global + 1;
    Ok (Subscription.create elt))
;;

let remove t (s : Subscription.t) =
  match s.elt with
  | None -> ()
  | Some elt ->
    s.elt <- None;
    let subscription = Bag.Elt.value elt in
    Subscription_t.close subscription `Removed;
    Bag.remove t.subscriptions elt;
    Hashtbl.change t.subscription_counts (Subscription_t.by subscription) ~f:(function
      | None -> None
      | Some n -> if n - 1 > 0 then Some (n - 1) else None);
    t.subscription_count_global <- max 0 (pred t.subscription_count_global);
;;

let deserializer = Deserializer.with_serializer (fun serializer ->
  let open Deserializer.Let_syntax in
  let%map_open () = return ()
  and properties =
    one ~default:Properties.default (module Persist.Properties)
      ~in_file:Persist.properties_file
  in
  { properties
  ; subscriptions = Bag.create ()
  ; subscription_counts = User_name.Table.create ()
  ; subscription_count_global = 0
  ; serializer
  }
)
;;

let check_non_negative_exn max =
  if max < 0
  then raise_s [%sexp "subscription limits must be non-negative", (max : int)]
;;

let set_and_persist_properties t properties =
  t.properties <- properties;
  Serializer.set_contents t.serializer t.properties (module Persist.Properties)
    ~file:Persist.properties_file
;;

let set_max_subscriptions_per_user t max =
  check_non_negative_exn max;
  if max <> t.properties.max_subscriptions_per_user then
    set_and_persist_properties t { t.properties with max_subscriptions_per_user = max }
;;

let set_max_subscriptions_global t max =
  check_non_negative_exn max;
  if max <> t.properties.max_subscriptions_global then
    set_and_persist_properties t { t.properties with max_subscriptions_global = max }
;;

let drop_all_by_user t query by_whom =
  (* Dropping the subscriptions causes the pipes to close, which causes the pipes to be
     removed from the state and the subscription limits. *)
  let some_subscription_was_dropped = ref false in
  let drop_msg =
    lazy (
      Error.create "subscription was dropped"
        query [%sexp_of: Iron_protocol.With_event_subscriptions.Action.t Query.t])
  in
  Bag.iter t.subscriptions ~f:(fun subscription ->
    let drop_it =
      match by_whom with
      | `User user_name -> User_name.equal user_name (Subscription_t.by subscription)
      | `All_users -> true
    in
    if drop_it
    then (
      Subscription_t.close subscription (`Dropped (force drop_msg));
      some_subscription_was_dropped := true));
  if not !some_subscription_was_dropped
  then
    raise_s [%sexp "no subscriptions to drop for" , (by_whom : User_name.Or_all.t)]
;;

let dump
      { properties = { max_subscriptions_global; max_subscriptions_per_user }
      ; subscriptions
      ; subscription_counts
      ; subscription_count_global = current_count_global
      ; serializer = _
      } =
  let current_count_by_user = User_name.Map.of_hashtbl_exn subscription_counts in
  [%sexp
    { max_subscriptions_global   : int
    ; current_count_global       : int
    ; max_subscriptions_per_user : int
    ; current_count_by_user      : int User_name.Map.t
    ; subscriptions              : Subscription_t.t Bag.t
    }
  ]
;;
