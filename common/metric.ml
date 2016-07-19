module Stable = struct

  open! Core.Stable

  module Snapshot = struct
    module V1 = struct
      type t =
        { count : int
        ; max   : float
        ; mean  : float
        ; min   : float
        }
      [@@deriving bin_io, fields, sexp]
    end
    module Model = V1
  end
end

open! Core.Std
open! Import

module Stat_type = struct
  module T = struct
    type t =
      | Count
      | Max
      | Min
      | Mean
      | Total
    [@@deriving compare, enumerate, sexp_of]
  end

  module Compare_by_interest = struct
    type t = T.t [@@deriving sexp_of]

    let rank = function
      | T.Mean   -> 0
      | Min    -> 1
      | Max    -> 2
      | Count  -> 3
      | Total  -> 4
    ;;

    let compare t1 t2 = Int.compare (rank t1) (rank t2)
  end

  include T
  include Comparable.Make_plain (T)
end

let aggregate_mean v1 c1 v2 c2 =
  let total mean count = mean *. Int.to_float count in
  let count = c1 + c2 in
  if count = 0
  then 0.
  else (total v1 c1 +. total v2 c2) /. Int.to_float count
;;

module Snapshot = struct
  include Stable.Snapshot.Model

  let total t =
    t.mean *. Int.to_float t.count
  ;;

  let aggregate r1 r2 =
    let count = r1.count + r2.count in
    let max = Float.max r1.max r2.max in
    let mean = aggregate_mean r1.mean r1.count r2.mean r2.count in
    let min = Float.min r1.min r2.min in
    { count; max; min; mean }
  ;;

  let get_stat_as_string_hum (t : t) stat_type ~decimals =
    (* Not stripping zero for better alignment in the ascii columns.  Also, in real life
       trailing zero are unlikely, so the cosmetic downside is not really present. *)
    let float f = Float.to_string_hum ~decimals ~strip_zero:false f in
    match (stat_type : Stat_type.t) with
    | Count -> t.count |> Int.to_string_hum
    | Max   -> t.max   |> float
    | Min   -> t.min   |> float
    | Total -> total t |> float
    | Mean  -> t.mean  |> float
  ;;
end

type t =
  { mutable count : int
  ; mutable max   : float
  ; mutable mean  : float
  ; mutable min   : float
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of : t] (fun () ->
    let check f = Invariant.check_field t f in
    let non_negative t = assert (t >= 0) in
    Fields.iter
     ~count:(check non_negative)
     ~max:ignore
     ~mean:ignore
     ~min:ignore
  )
;;

let create () =
  { count = 0
  ; mean  = 0.
  ; min   = Float.max_value
  ; max   = Float.min_value
  }
;;

let add_value ({ count; mean; min; max } as t) data =
  t.count <- count + 1;
  t.mean  <- aggregate_mean mean count data 1;
  t.min   <- Float.min min data;
  t.max   <- Float.max max data;
;;

let snapshot { count; mean; min; max } =
  { Snapshot.
    count
  ; max
  ; min
  ; mean
  }
;;
