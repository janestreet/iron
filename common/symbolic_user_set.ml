module Stable_format = struct
  open! Core.Core_stable
  module User_name  = User_name.Stable
  module Group_name = Group_name.Stable

  module V1 = struct
    type t =
      | Users of User_name.V1.Set.t
      | Group of Group_name.V1.t
      | Union of t list
      | Intersection of t * t list (* I.e., a non-empty list *)
      | Difference of t * t
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| ae9adb3f4022f9737992287b4e566758 |}]
    ;;
  end
end

open! Core
open! Import

type t = User_name.Set.t [@@deriving compare, sexp_of]

let invariant _ = ()

module Stable = struct
  module V1 = struct
    include Make_stable.Of_stable_format.V1 (Stable_format.V1) (struct
        type nonrec t = t [@@deriving compare]

        let to_stable_format t = Stable_format.V1.Users t

        let of_stable_format = function
          | Stable_format.V1.Users users  -> users
          | v1 ->
            raise_s [%sexp "Symbolic_user_set.of_stable", (v1 : Stable_format.V1.t)]
        ;;
      end)

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| ae9adb3f4022f9737992287b4e566758 |}]
    ;;
  end
end
