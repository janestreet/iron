open! Core
open! Import

include Lru_cache_intf

module Make (H : H) = struct
  module Hq = struct
    include Hash_queue.Make (H)

    let to_alist t =
      list_of_iter (fun ~f -> iteri t ~f:(fun ~key ~data -> f (key, data)))
    ;;

    let sexp_of_t (type a) (sexp_of_a : a -> Sexp.t) t =
      t |> to_alist |> [%sexp_of: (H.t * a) list]
    ;;
  end


  type 'a t =
    { mutable max_size : int
    ; items            : 'a Hq.t
    }

  let sexp_of_t (type a) (sexp_of_a : a -> Sexp.t) (t : a t) =
    [%sexp
      { max_size = (t.max_size        : int)
      ; length   = (Hq.length t.items : int)
      ; items    = (t.items           : a Hq.t)
      }
    ]
  ;;

  let stats ?(sexp_of_key = [%sexp_of: H.t]) t =
    [%sexp
      { max_size = (t.max_size        : int)
      ; length   = (Hq.length t.items : int)
      ; keys     = (Hq.keys   t.items : key list)
      }
    ]
  ;;

  let max_size_lower_bound = 0

  let invariant invariant_a t =
    Invariant.invariant [%here] (stats t) [%sexp_of: Sexp.t] (fun () ->
      assert (Hq.length t.items <= t.max_size);
      assert (t.max_size >= max_size_lower_bound);
      Hq.iteri t.items ~f:(fun ~key ~data ->
        Invariant.invariant [%here] key [%sexp_of: H.t] (fun () ->
          H.invariant key;
          invariant_a data));
    )
  ;;

  let check_max_size_exn max_size =
    if max_size < max_size_lower_bound then
      raise_s
        [%sexp
          "invalid Lru.max_size argument",
          { requested_max_size     = (max_size             : int)
          ; smallest_value_allowed = (max_size_lower_bound : int)
          }
        ]
  ;;

  let create ~max_size =
    check_max_size_exn max_size;
    { max_size
    ; items = Hq.create ()
    }
  ;;

  let to_alist t = Hq.to_alist t.items
  let length   t = Hq.length   t.items
  let is_empty t = Hq.is_empty t.items
  let max_size t = t.max_size

  let find t key =
    match Hq.lookup t.items key with
    | None -> None
    | Some data as result ->
      Hq.remove_exn t.items key;
      Hq.enqueue_exn t.items key data;
      result
  ;;

  let mem t key = Option.is_some (find t key)

  let clear t =
    let len = length t in
    Hq.clear t.items;
    `Dropped len
  ;;

  let drop_lru_items (type a) (t : a t) =
    let max_size = max 0 t.max_size in
    while length t > max_size do
      ignore (Hq.dequeue_exn t.items : a);
    done
  ;;

  let remove t key = Hq.remove t.items key

  let set t ~key ~data =
    (match Hq.remove t.items key with `Ok | `No_such_key -> ());
    Hq.enqueue_exn t.items key data;
    drop_lru_items t;
  ;;

  let set_max_size t ~max_size =
    check_max_size_exn max_size;
    let len = length t in
    t.max_size <- max_size;
    drop_lru_items t;
    let len' = length t in
    `Dropped (len - len')
  ;;
end

