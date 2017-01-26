open Core

module T = struct
  type t =
    [ `b1_b2_f1_f2
    | `b1_b2_f1
    | `b1_b2_f2
    | `b1_b2__f1_f2
    | `b1_b2
    | `b1_f1_f2
    | `b1_f1__b2_f2
    | `b1_f1
    | `b1_f2__b2_f1
    | `b1_f2
    | `b2_f1_f2
    | `b2_f1
    | `b2_f2
    | `f1_f2
    | `conflict
    ]
  [@@deriving sexp, enumerate, compare]

  let hash : t -> int = Hashtbl.hash
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let to_groups = function
  | `b1_b2_f1_f2   -> [ [ `b1 ; `b2 ; `f1 ; `f2 ] ]
  | `b1_b2_f1      -> [ [ `b1 ; `b2 ; `f1 ] ; [ `f2 ] ]
  | `b1_b2_f2      -> [ [ `b1 ; `b2 ; `f2 ] ; [ `f1 ] ]
  | `b1_b2__f1_f2  -> [ [ `b1 ; `b2 ] ; [ `f1 ; `f2 ] ]
  | `b1_b2         -> [ [ `b1 ; `b2 ] ; [ `f1 ] ; [ `f2 ] ]
  | `b1_f1_f2      -> [ [ `b1 ; `f1 ; `f2 ] ; [ `b2 ] ]
  | `b1_f1__b2_f2  -> [ [ `b1 ; `f1 ] ; [ `b2 ; `f2 ] ]
  | `b1_f1         -> [ [ `b1 ; `f1 ] ; [ `b2 ] ; [ `f2 ] ]
  | `b1_f2__b2_f1  -> [ [ `b1 ; `f2 ] ; [ `b2 ; `f1 ] ]
  | `b1_f2         -> [ [ `b1 ; `f2 ] ; [ `f1 ] ; [ `b2 ] ]
  | `b2_f1_f2      -> [ [ `b1 ] ; [ `b2 ; `f1 ; `f2 ] ]
  | `b2_f1         -> [ [ `b1 ] ; [ `b2 ; `f1 ] ; [ `f2 ] ]
  | `b2_f2         -> [ [ `b1 ] ; [ `f1 ] ; [ `b2 ; `f2 ] ]
  | `f1_f2         -> [ [ `b1 ] ; [ `f1 ; `f2 ] ; [ `b2 ] ]
  | `conflict      -> [ [ `b1 ] ; [ `b2 ] ; [ `f1 ] ; [ `f2 ] ]
;;

let to_string = function
  | `b1_b2_f1_f2   -> "{ B1 B2 F1 F2 }"
  | `b1_b2_f1      -> "{ B1 B2 F1 }"
  | `b1_b2_f2      -> "{ B1 B2 F2 }"
  | `b1_b2__f1_f2  -> "{ B1 B2 } { F1 F2 }"
  | `b1_b2         -> "{ B1 B2 }"
  | `b1_f1_f2      -> "{ B1 F1 F2 }"
  | `b1_f1__b2_f2  -> "{ B1 F1 } { B2 F2 }"
  | `b1_f1         -> "{ B1 F1 }"
  | `b1_f2__b2_f1  -> "{ B1 F2 } { B2 F1 }"
  | `b1_f2         -> "{ B1 F2 }"
  | `b2_f1_f2      -> "{ B2 F1 F2 }"
  | `b2_f1         -> "{ B2 F1 }"
  | `b2_f2         -> "{ B2 F2 }"
  | `f1_f2         -> "{ F1 F2 }"
  | `conflict      -> "{ }"
;;

let%test_unit _ =
  let node b = b
               |> [%sexp_of: [`b1 | `b2 | `f1 | `f2]]
               |> Sexp.to_string
               |> String.uppercase
  in
  let group group =
    if Int.(<=) (List.length group) 1 then None
    else Some (sprintf "{ %s }" (String.concat ~sep:" " (List.map group ~f:node)))
  in
  let groups groups =
    match List.filter_map groups ~f:group with
    | [] -> "{ }"
    | list -> String.concat ~sep:" " list
  in
  List.iter all ~f:(fun t ->
    let s = to_string t in
    let s' = groups (to_groups t) in
    [%test_result: string] s' ~expect:s
  )
;;

module Shown_class = struct
  type class_ = t
  module T = struct
    type t =
      [ `b1_b2_f1
      | `b1_b2_f2
      | `b1_b2
      | `b1_f1_f2
      | `b1_f1
      | `b1_f2__b2_f1
      | `b1_f2
      | `b2_f1
      | `b2_f2
      | `f1_f2
      | `conflict
      ]
    [@@deriving sexp, compare, enumerate]

    let hash : t -> int = Hashtbl.hash
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let to_class t = (t :> class_)
  ;;

  let of_class = function
    | `b1_b2_f1_f2
    | `b1_b2__f1_f2
    | `b1_f1__b2_f2
    | `b2_f1_f2
      -> None
    | `b1_b2_f1
    | `b1_b2_f2
    | `b1_b2
    | `b1_f1_f2
    | `b1_f1
    | `b1_f2__b2_f1
    | `b1_f2
    | `b2_f1
    | `b2_f2
    | `f1_f2
    | `conflict
      as t -> Some (t :> t)
  ;;
end

let is_shown t = Option.is_some (Shown_class.of_class t)
;;

let classify ~equal:eq ~b1 ~b2 ~f1 ~f2 =
  if eq b1 b2
  then
    if eq b2 f1
    then
      if eq f1 f2
      then `b1_b2_f1_f2
      else `b1_b2_f1
    else
    if eq b2 f2
    then `b1_b2_f2
    else
    if eq f1 f2
    then `b1_b2__f1_f2
    else `b1_b2
  else
  if eq b1 f1
  then
    if eq f1 f2
    then `b1_f1_f2
    else
    if eq b2 f2
    then `b1_f1__b2_f2
    else `b1_f1
  else
  if eq b1 f2
  then
    if eq b2 f1
    then `b1_f2__b2_f1
    else `b1_f2
  else
  if eq b2 f1
  then
    if eq f1 f2
    then `b2_f1_f2
    else `b2_f1
  else
  if eq b2 f2
  then `b2_f2
  else
  if eq f1 f2
  then `f1_f2
  else `conflict
;;

(* Reading the code above can be a bit difficult. The tests are probably more
   explanatory. *)

let%test_module _ =
  (module struct
    let check b1 b2 f1 f2 = classify ~equal:Int.equal ~b1 ~b2 ~f1 ~f2

    let%test _ =
      let run = function
        | `b1_b2_f1_f2   -> check 0 0 0 0
        | `b1_b2_f1      -> check 0 0 0 1
        | `b1_b2_f2      -> check 0 0 1 0
        | `b1_b2__f1_f2  -> check 0 0 1 1
        | `b1_b2         -> check 0 0 1 2
        | `b1_f1_f2      -> check 0 1 0 0
        | `b1_f1__b2_f2  -> check 0 1 0 1
        | `b1_f1         -> check 0 1 0 2
        | `b1_f2__b2_f1  -> check 0 1 1 0
        | `b1_f2         -> check 0 1 2 0
        | `b2_f1_f2      -> check 1 0 0 0
        | `b2_f1         -> check 1 0 0 2
        | `b2_f2         -> check 1 0 2 0
        | `f1_f2         -> check 1 2 0 0
        | `conflict      -> check 0 1 2 3
      in
      List.for_all all ~f:(fun case -> case = run case)
    ;;
  end)

let is_forget = function
  | `b2_f1_f2 -> true
  | _ -> false
;;
