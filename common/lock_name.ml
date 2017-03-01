module Stable = struct
  module V3 = struct
    type t =
      | Create_child
      | Rebase
      | Release
      | Release_into
      | Rename
      | Second
    [@@deriving bin_io, compare, enumerate, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| a605da217532e39ff71bfb4976f5c5da |}]
    ;;
  end

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

    open! Core
    open! Import

    let of_v3 (v3 : V3.t) =
      match v3 with
      | Create_child -> None
      | Rebase       -> Some Rebase
      | Release      -> Some Release
      | Release_into -> Some Release_into
      | Rename       -> Some Rename
      | Second       -> None
    ;;

    let to_v3 = function
      | Rebase       -> V3.Rebase
      | Release      -> Release
      | Release_into -> Release_into
      | Rename       -> Rename
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

    open! Core
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

  module Model = V3
end

module T = struct
  include Stable.Model
  let hash = Hashtbl.hash
end

open! Core
open! Import

include T
include Comparable.Make_plain (T)
include Hashable.  Make_plain (T)

let to_string_hum t = Enum.to_string_hum (module T) t
