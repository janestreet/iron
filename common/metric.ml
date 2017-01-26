module Stable = struct

  open! Core.Core_stable
  module Time = Import.Time.Stable

  module Data_point = struct
    module V1 = struct
      type t =
        { at    : Time.V1_round_trippable.t
        ; value : float
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b045a92511846c42231c91245f9fedab |}]
      ;;
    end
    module Model = V1
  end
end

open! Core
open! Import

module Data_point = struct
  include Stable.Data_point.Model

  let compare_by_time t1 t2 = Time.compare t1.at t2.at
end

type t =
  { max_size    : int
  ; data_points : Data_point.t Queue.t
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of : t] (fun () ->
    let { max_size; data_points } = t in
    assert (Queue.length data_points <= max_size))
;;

let create () =
  { max_size    = 1000
  ; data_points = Queue.create ()
  }
;;

let add t data_point =
  Queue.enqueue t.data_points data_point;
  if Queue.length t.data_points > t.max_size
  then ignore (Queue.dequeue t.data_points : Data_point.t option);
;;

let data_points t =
  Queue.to_list t.data_points
  |> List.sort ~cmp:Data_point.compare_by_time
;;
