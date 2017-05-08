open! Core
open! Async
open! Import

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

module Make (Key : sig
    type t [@@deriving sexp_of]
    include Hashable.S_plain with type t := t
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
          then raise_s [%sexp "bag is empty for key", (key : Key.t)])
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
    (match initial with
     | None -> ()
     | Some v -> Pipe.write_without_pushback writer (Ok v));
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
