module Stable = struct
  open Core.Core_stable

  module V1 = struct
    (* Note: keep the equality between [t] and [Async.Rpc.Description.t].  The day it
       breaks, mint V2.t with the same equality. *)
    type t = Async.Rpc.Description.t =
      { name    : string
      ; version : int
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 4521f44dbc6098c0afc2770cc84552b1 |}]
    ;;
  end
end

open! Core
open! Import

module T = Stable.V1
include T
include Comparable.Make (T)

module Ascii_table = Iron_ascii_table

let to_ascii_table (ts : t list) =
  let rows =
    ts
    |> List.map ~f:(fun description ->
      description.name, description.version)
    |> String.Table.of_alist_multi
    |> Hashtbl.to_alist
    |> List.map ~f:(fun (name, versions) ->
      let versions =
        versions
        |> List.sort ~cmp:Int.compare
        |> List.map ~f:Int.to_string_hum
        |> String.concat ~sep:", "
      in
      name, versions)
    |> List.sort ~cmp:(fun (name1, _) (name2, _) -> String.compare name1 name2)
  in
  Ascii_table.create
    ~columns:Ascii_table.Column.(
      [ string ~header:"name"     (cell fst)
      ; string ~header:"versions" (cell snd)
      ])
    ~rows
;;
