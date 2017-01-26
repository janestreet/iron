module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = Unit

    module Model = V1
  end

  module Key = struct

    module V1 = struct
      type t =
        { by          : User_name.V1.t
        ; rpc_name    : string
        ; rpc_version : int
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 6da9b5b2a0991162b3b4f8a45b22a8e9 |}]
      ;;

      let hash (t : t) = Core.Hashtbl.hash t
    end

    module Model = V1
  end

  module Data = struct

    module V1 = struct
      type t =
        { hits : int
        ; took : Span.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 0750f7a8e10dc225f94c88c305349517 |}]
      ;;
    end

    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t = (Key.V1.t * Data.V1.t) list
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 42dc109231e7e7d67b803eae1a431528 |}]
      ;;

      let of_model m = m
    end

    module Model = V1
  end
end

open! Core
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "get-rpc-stats" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action       = Stable.Action.  Model
module Data         = Stable.Data.    Model
module Key          = Stable.Key.     Model
module Reaction     = Stable.Reaction.Model
