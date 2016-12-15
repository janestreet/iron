module Stable_format = struct
  open! Core.Stable
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

open! Core.Std
open! Import

type t = User_name.Set.t [@@deriving sexp_of]

let invariant _ = ()

module Stable = struct
  module V1 = struct
    include Wrap_stable.F
        (Stable_format.V1)
        (struct
          type nonrec t = t

          module V1 = Stable_format.V1

          let to_stable t : V1.t = Users t

          let of_stable : V1.t -> _ = function
            | Users users  -> users
            | v1 -> failwiths "Symbolic_user_set.of_stable" v1 [%sexp_of: V1.t]
          ;;
        end)

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| ae9adb3f4022f9737992287b4e566758 |}]
    ;;
  end
end
