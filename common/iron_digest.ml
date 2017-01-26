module Stable = struct
  open! Core.Core_stable
  module Hash_consing = Hash_consing.Stable

  module V1 = struct

    module Unshared = struct
      type t = string [@@deriving compare]

      let hash =
        Core.String.hash
      ;;

      let module_name = "Iron_common.Digest"

      include Binable.Of_stringable.V1 (struct
          type nonrec t = t
          let of_string t = t
          let to_string t = t
        end)

      include Sexpable.Of_stringable.V1 (struct
          type nonrec t = t
          let of_string = Digest.from_hex
          let to_string = Digest.to_hex
        end)

    end
    include Hash_consing.Make_stable_private (Unshared) ()

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
    ;;
  end
end

open! Core
open! Import

module T = Stable.V1
include T
include Comparable.Make (T)
include Hashable.Make (T)

let invariant (_ : t) = ()

let create str = Digest.string str |> shared_t

let of_empty_string = create ""
