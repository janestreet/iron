module Stable = struct
  open Core.Core_stable
  module User_name = User_name.Stable

  module Users = struct
    module V1 = struct
      type t =
        | All_users
        | Users of User_name.V1.Set.t
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 6b1f0a0ded957a6f8ddf90e5276d34da |}]
      ;;

      let hash = function
        | All_users -> Core.Int.hash 1
        | Users users -> Hash_consing.fold_hash 2 (User_name.V1.Set.hash users)
      ;;
    end
  end

  module V1 = struct
    type one =
      { reviewed_for : Users.V1.t
      ; reviewed_by  : Users.V1.t
      }
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: one];
      [%expect {| 212e584615015f7a824c361545d3f6cd |}]
    ;;

    type t = one list
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 08102ebed15d708874f044bf72f4f8ec |}]
    ;;

    let hash_one { reviewed_for; reviewed_by } =
      Hash_consing.fold_hash
        (Users.V1.hash reviewed_for)
        (Users.V1.hash reviewed_by)
    ;;

    let hash t = Hash_consing.list_hash hash_one t
  end
end

open! Core
open! Import

module Users = struct
  include Stable.Users.V1

  let all_users = All_users
  let empty = Users User_name.Set.empty

  let users users = Users users

  let mem t user =
    match t with
    | All_users   -> true
    | Users users -> User_name.Set.mem users user
  ;;

  let add t user =
    match t with
    | All_users   -> All_users
    | Users users -> Users (User_name.Set.add users user)
  ;;

  let union t1 t2 =
    match t1, t2 with
    | _, All_users | All_users, _ -> All_users
    | Users u1, Users u2 -> Users (User_name.Set.union u1 u2)
  ;;

  let union_list ts = List.fold ts ~init:empty ~f:union
end

include Stable.V1

let none = []

let all =
  [ { reviewed_for = Users.all_users
    ; reviewed_by  = Users.all_users
    }
  ]
;;

let also_allow t ~reviewed_for ~reviewed_by = { reviewed_by; reviewed_for } :: t

let may_be_reviewed_by t ~reviewed_for =
  Users.union_list
    (List.filter_map t ~f:(fun one ->
       if Users.mem one.reviewed_for reviewed_for
       then Some one.reviewed_by
       else None))
;;

let check t ~reviewed_for ~reviewed_by =
  let may_be_reviewed_by = may_be_reviewed_by t ~reviewed_for in
  if User_name.equal reviewed_by reviewed_for
  || Users.mem may_be_reviewed_by reviewed_by
  then Ok ()
  else
    error_s
      [%sexp
        "this is not allowed -- see [Allow_review_for] in .fe/obligations-repo.sexp",
        { reviewed_for       : User_name.t
        ; reviewed_by        : User_name.t
        ; may_be_reviewed_by : Users.t
        }
      ]
;;
