module Stable = struct
  open Core.Core_stable

  module V1 = struct
    module Unshared = struct
      type t = string [@@deriving bin_io, compare, sexp]
      let module_name = "Iron_common.File_name"
      let hash = Core.String.hash
    end
    include Hash_consing.Stable.Make_stable_private (Unshared) ()

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
    ;;
  end
end

open Core
open! Import

module T = struct

  include Stable.V1

  let to_string = unshared_t

  let invariant t =
    let t = to_string t in
    if String.is_empty t then failwith "file name must be nonempty";
    if String.contains t '/'
    then raise_s [%sexp "a file name may not contain '/'", (t : string)];
    if String.contains t '\000'
    then raise_s [%sexp "a file name may not contain '\\000'", (t : string)];
  ;;

  let of_string s =
    let s = shared_t s in
    invariant s;
    s
  ;;
end

include T

include Identifiable.Make (struct
    include T
    include Sexpable.Of_stringable (T)
  end)

let dot    = of_string "."
let dotdot = of_string ".."

let%test _ = Exn.does_raise (fun () -> of_string "")
let%test _ = Exn.does_raise (fun () -> of_string "a/b")
let%test _ = Exn.does_raise (fun () -> of_string "a\000b")

let dot_fe = of_string ".fe.sexp"

let scaffold_sexp = of_string "scaffold.sexp"

let suffixes_priority_order = [ "_intf.ml" ; ".mli" ; "_stubs.c"; ".ml" ]

let with_suffix str =
  let index = ref (-1) in
  List.find_map suffixes_priority_order ~f:(fun suffix ->
    incr index;
    match String.chop_suffix str ~suffix with
    | Some prefix -> Some (prefix, Some !index)
    | None -> None)
  |> Option.value ~default:(str, None)
;;

let alphabetic_compare t1 t2 =
  String.alphabetic_compare (to_string t1) (to_string t2)
;;

let default_review_compare t1 t2 =
  let str1 = to_string t1 and str2 = to_string t2 in
  let (str1, index1) = with_suffix str1 in
  let (str2, index2) = with_suffix str2 in
  match String.alphabetic_compare str1 str2 with
  | 0 -> [%compare: int option] index1 index2
  | c -> c
;;
