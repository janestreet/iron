open! Core
open! Import

module Key = struct
  module T = struct
    type t = Iron_protocol.Get_rpc_stats.Key.t =
      { by          : User_name.t
      ; rpc_name    : string
      ; rpc_version : int
      }
    [@@deriving compare, fields, sexp]
    let hash (t : t) = Hashtbl.hash t
  end
  include T
  include Hashable.Make (T)

  let invariant t =
    let check f = Invariant.check_field t f in
    Fields.iter
      ~by:(check User_name.invariant)
      ~rpc_name:ignore
      ~rpc_version:ignore
  ;;
end

module Data = struct
  include Iron_protocol.Get_rpc_stats.Data

  let invariant t =
    let check f = Invariant.check_field t f in
    Fields.iter
      ~hits:(check (fun hits -> assert (hits >= 0)))
      ~took:(check (fun took -> assert (Time.Span.( >= ) took Time.Span.zero)))
  ;;

  let zero = { hits = 0; took = Time.Span.zero }

  let add_hit t ~took =
    { hits = succ t.hits
    ; took = Time.Span.(+) t.took took
    }
  ;;
end

type t = Data.t Key.Table.t
[@@deriving sexp_of]

let invariant t =
  Hashtbl.iteri t ~f:(fun ~key ~data ->
    Invariant.invariant [%here] (key, data) [%sexp_of: Key.t * Data.t] (fun () ->
      Key.invariant key;
      Data.invariant data;
    ))
;;

let create () = Key.Table.create ()

let add_hit t key ~took =
  Hashtbl.update t key ~f:(fun present ->
    Data.add_hit (Option.value present ~default:Data.zero) ~took)
;;

let to_protocol table = Hashtbl.to_alist table
