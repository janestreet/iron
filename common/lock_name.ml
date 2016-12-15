module Stable = struct
  module V2 = struct
    type t =
      | Rebase
      | Release
      | Release_into
      | Rename
    [@@deriving bin_io, compare, enumerate, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| ce2471a84a580d8a799a566d5ca014f6 |}]
    ;;
  end

  module V1 = struct
    type t =
      | Rebase
      | Release
      | Release_into
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 87cdeca4e3128fc2c80f2225d290c51b |}]
    ;;

    open! Core.Std
    open! Import

    let of_v2 (v2 : V2.t) =
      match v2 with
      | Rebase       -> Some Rebase
      | Release      -> Some Release
      | Release_into -> Some Release_into
      | Rename       -> None
    ;;

    let to_v2 = function
      | Rebase       -> V2.Rebase
      | Release      -> Release
      | Release_into -> Release_into
    ;;
  end

  module Model = V2
end

module T = struct
  include Stable.Model
  let hash = Hashtbl.hash
end

open Core.Std
open! Import

include T
include Comparable.Make (T)
include Hashable.Make (T)

let to_string_hum t = Enum.to_string_hum (module T) t
