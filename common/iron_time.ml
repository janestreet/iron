module Stable = struct

  open Core.Core_stable

  module V1_round_trippable = struct

    type nonrec t = Time_ns.V1.t [@@deriving bin_io, compare]

    type sexp_repr =
      { human_readable     : Time_ns.V1.t
      ; int_ns_since_epoch : int
      }
    [@@deriving sexp]

    let sexp_of_t t =
      { human_readable     = t
      ; int_ns_since_epoch = Core.Time_ns.to_int_ns_since_epoch t
      }
      |> [%sexp_of: sexp_repr]
    ;;

    let t_of_sexp sexp =
      let { int_ns_since_epoch; _ } = sexp |> [%of_sexp: sexp_repr] in
      Core.Time_ns.of_int_ns_since_epoch int_ns_since_epoch
    ;;
  end

end

open! Core

include (Time_ns : (module type of Time_ns with module Stable := Time_ns.Stable))
