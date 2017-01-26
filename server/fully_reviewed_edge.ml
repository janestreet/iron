module Stable = struct
  open Core.Core_stable
  open Import_stable
  module Reason = struct
    module V1 = struct
      type t =
        | External of unit Query.V1.t
        | Release of Feature_path.V1.t * Rev.V1.t
        | Review
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b1e3e0e9ed87120a630994cffec81cf4 |}]
      ;;
    end
  end

  module V1 = struct
    type t =
      { rev_zero : Rev.V1.t
      ; from_    : Rev.V1.t
      ; to_      : Rev.V1.t
      ; reason   : Reason.V1.t
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| ff247abd650baef22ed35d3e82c38aae |}]
    ;;
  end
end

open! Core
open! Import

include Stable.V1

module Reason = Stable.Reason.V1

let invariant (_ : t) = ()
