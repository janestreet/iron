module Stable = struct
  open! Core.Core_stable

  module Relpath = Relpath.Stable

  module V1 = struct
    module Unshared = struct
      include Relpath.V1
      let module_name = "Iron_common.Path_in_repo"
      let hash (t : t) = Path.Relpath.hash t
    end
    include Hash_consing.Stable.Make_stable_private (Unshared) ()

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 296be80010ace497614f92952e5510c4 |}]
    ;;
    let to_string t   = Unshared.to_string (unshared_t t)
    let of_string str = shared_t (Unshared.of_string str)
  end

  module Model = V1
end

open! Core
open! Import

module T = Stable.Model
include T

include Identifiable.Make (T)

let invariant t = Relpath.invariant (unshared_t t)

let root = shared_t Relpath.empty

let chop_prefix ~prefix t =
  Relpath.chop_prefix ~prefix:(unshared_t prefix) (unshared_t t)
;;

let is_prefix ~prefix t = Relpath.is_prefix ~prefix:(unshared_t prefix) (unshared_t t)

let split_dir_file_exn t =
  let t, file_name = Relpath.split_dir_file_exn (unshared_t t) in
  shared_t t, file_name
;;

let of_list file_names = Relpath.of_list file_names |> shared_t
let parts t = Relpath.parts (unshared_t t)

let append t relpath   = Relpath.append (unshared_t t) relpath   |> shared_t
let extend t file_name = Relpath.extend (unshared_t t) file_name |> shared_t

let parent     t = Option.map ~f:shared_t (Relpath.parent (unshared_t t))
let parent_exn t = Relpath.parent_exn (unshared_t t) |> shared_t

let of_relpath = shared_t
let to_relpath = unshared_t

let default_review_compare t1 t2 =
  Relpath.default_review_compare (unshared_t t1) (unshared_t t2)
;;

let low_review_file build_projection_name =
  extend
    (of_string ".fe")
    (File_name.of_string
       (concat [ "low-review-in-"
               ; Build_projection_name.to_string build_projection_name
               ]))
;;
