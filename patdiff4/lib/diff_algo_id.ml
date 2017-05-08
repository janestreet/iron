open Core
open Iron_common.Std

module To_stringable_with_dashes ( X : Sexpable ) = struct
  let of_string s =
    let s = String.map ~f:(fun c -> if Char.(=) c '-' then '_' else c) s in
    X.t_of_sexp (Sexp.Atom s)
  ;;
  let to_string t =
    let s = Sexp.to_string (X.sexp_of_t t) in
    String.map ~f:(fun c -> if Char.(=) c '_' then '-' else c) s
  ;;
end

module T = struct

  type metadata = string [@@deriving sexp, compare]
  let all_of_metadata = [ ]

  type t =
    [ `old_base_to_old_tip
    | `old_base_to_new_base
    | `old_base_to_new_tip
    | `old_tip_to_new_tip
    | `new_base_to_new_tip
    | `feature_ddiff
    | `base_ddiff
    | `story
    | `conflict_resolution
    | `metadata of metadata
    ]
  [@@deriving sexp, compare, enumerate]
  let hash : t -> int = Hashtbl.hash
end

include T
include To_stringable_with_dashes(T)
include Comparable.Make (T)
include Hashable.Make (T)

let is_simple_diff = function
  | `old_base_to_old_tip
  | `old_base_to_new_base
  | `old_base_to_new_tip
  | `old_tip_to_new_tip
  | `new_base_to_new_tip
    -> true
  | `feature_ddiff
  | `base_ddiff
  | `story
  | `conflict_resolution
  | `metadata _
    -> false
;;

let simple_diff ~from ~to_ =
  match from, to_ with
  | (`old_base , `old_tip)  -> `old_base_to_old_tip
  | (`old_base , `new_base) -> `old_base_to_new_base
  | (`old_base , `new_tip)  -> `old_base_to_new_tip
  | (`old_tip  , `new_tip)  -> `old_tip_to_new_tip
  | (`new_base , `new_tip)  -> `new_base_to_new_tip
  | (a, b) -> raise_s [%sexp "invalid diff", (a : Diamond.Node.t), (b : Diamond.Node.t)]
;;

let to_string = function
  | `metadata str -> str
  | a -> to_string a
;;

let%test _ =
  (* Note for dev: beware the names are used in the patdiff4.el file *)
  let referenced_by_patdiff4_el =
    [ `conflict_resolution , "conflict-resolution"
    ; `feature_ddiff       , "feature-ddiff"
    ; `base_ddiff          , "base-ddiff"
    ]
  in
  List.for_all referenced_by_patdiff4_el
    ~f:(fun (t, expect) -> String.equal (to_string t) expect)
;;
