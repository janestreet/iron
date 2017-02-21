module T1 = struct
  open! Core
  open! Import

  module T0 = struct
    type t =
      | Archive
      | Release
    [@@deriving compare, enumerate, sexp]

    let to_string t = t |> sexp_of_t |> Sexp.to_string |> String.uncapitalize
    let of_string t = t |> String.capitalize |> Sexp.of_string |> t_of_sexp
  end
  include T0
  include Sexpable.Of_stringable (T0)
end

module Stable = struct
  module V1 = struct
    open! Core.Core_stable

    module T = Make_stable.Of_stable_format.V1 (struct
        type t = string [@@deriving bin_io, sexp]
      end) (struct
        type t = T1.t [@@deriving compare]
        let to_stable_format = T1.to_string
        let of_stable_format = T1.of_string
      end)

    include T
    include Comparable.V1.Make (T)

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
    ;;
  end
end

open! Core
open! Import

module T2 = struct
  include T1
  type comparator_witness = Stable.V1.comparator_witness
  let comparator = Stable.V1.comparator
end
include T2
include Comparable.Make_plain_using_comparator (T2)

let default =
  Set.of_list
    [ Archive
    ; Release
    ]
;;
