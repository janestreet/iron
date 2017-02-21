module Stable = struct
  open Core.Core_stable

  module V2 = struct
    type t =
      { name                : string
      ; version             : int
      ; query_bin_digest    : string Lazy.V1.t
      ; response_bin_digest : string Lazy.V1.t
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 27bdb15928446e01f84e14b539e67395 |}]
    ;;

    let of_model m = m

  end

  module V1 = struct
    type t = Async.Rpc.Description.t =
      { name    : string
      ; version : int
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 4521f44dbc6098c0afc2770cc84552b1 |}]
    ;;

    open! Core

    let of_v2 m =
      let { V2.name; version; query_bin_digest = _; response_bin_digest = _ } =
        V2.of_model m
      in
      { name; version; }
    ;;

  end

  module Model = V2
end

open! Core
open! Import

include Stable.Model

let create ~name ~version ~query ~response =
  let digest shape =
    lazy (Bin_prot.Shape.Digest.to_hex (Bin_prot.Shape.eval_to_digest shape))
  in
  { name
  ; version
  ; query_bin_digest    = digest query
  ; response_bin_digest = digest response
  }
;;

module Compare_by_name_and_version = struct
  type nonrec t = t [@@deriving compare, sexp_of]

  let compare t t' =
    let rv = [%compare: string] t.name t'.name in
    if Int.(<>) rv 0
    then rv
    else [%compare: int] t.version t'.version
  ;;
end

let to_ascii_table (ts : t list) =
  Iron_ascii_table.create
    ~rows:(List.sort ts ~cmp:[%compare: Compare_by_name_and_version.t])
    ~columns:Iron_ascii_table.Column.(
      [ string ~header:"name"     (cell name)
      ; int    ~header:"version"  (cell version)
      ; string ~header:"query"    (cell (Fn.compose force query_bin_digest))
      ; string ~header:"response" (cell (Fn.compose force response_bin_digest))
      ])
;;
