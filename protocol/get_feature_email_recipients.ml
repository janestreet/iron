module Stable = struct

  open! Import_stable

  module Sent_upon = struct
    module V1 = struct
      type t =
        | Archive
        | Release
        | Release_into
      [@@deriving bin_io, enumerate, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ee1479ec74614568eadf13e80834a559 |}]
      ;;
    end

    module Model = V1
  end

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; sent_upon    : Sent_upon.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 2c4b2582982ea372cf35e3d3a2df5ed5 |}]
      ;;

      let to_model t = t
    end

    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t = { recipients : Email_address.V1.Set.t }
      [@@deriving bin_io, fields, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 57f7ecc01c7b64f10ea073c333e0bf4d |}]
      ;;

      let of_model m = m
    end

    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "get-feature-email-recipients" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

open! Core
open! Import

module Sent_upon = Stable.Sent_upon. Model
module Action    = Stable.Action.    Model
module Reaction  = Stable.Reaction.  Model
