module Stable_workaround = struct
  open! Core.Core_stable

  module Feature_name = Feature_name.Stable

  module V1 = struct

    module T0 = struct

      (* The feature names in the internal representation of a feature path are in order
         from leaf to root.  I.e.

         {[
           of_string "a/b/c" = [ "c"; "b"; "a"]
         ]}
      *)
      type t = Feature_name.V1.t list [@@deriving bin_io]

      open Core

      let to_string t =
        String.concat ~sep:"/" (List.rev_map t ~f:Feature_name.V1.to_string)
      ;;

      let compare t1 t2 = Iron_string.alphabetic_compare (to_string t1) (to_string t2)

      (* The grammar spec for a feature path:
         A feature path (fp)
         - must have at least one feature-name element (fn)
         - may end in an optional slash.

         fp ::= fn
         | fn "/"
         | fn "/" fp
      *)
      let of_string string =
        (* Strip a trailing /, if there is one. *)
        let string = if String.is_suffix ~suffix:"/" string
          then String.drop_suffix string 1
          else string
        in
        try List.rev_map (String.split string ~on:'/') ~f:Feature_name.V1.of_string
        with exn ->
          raise_s [%sexp "Invalid feature path", (string : string), (exn : exn)]
      ;;
    end
    module T1 = struct
      include T0
      include Sexpable.Of_stringable.V1 (T0)
    end
    module T2 = struct
      include T1
      include Comparator.V1.Make (T1)
    end
    module T3 = Comparable.V1.Make (T2)
  end
end

module Stable = struct
  module V1 = struct
    include Stable_workaround.V1
    include Stable_workaround.V1.T2
    include Stable_workaround.V1.T3

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 296be80010ace497614f92952e5510c4 |}]
    ;;
  end
end

open! Core
open! Import

module T = struct
  include Stable_workaround.V1.T2

  let hash t =
    List.fold t ~init:0 ~f:(fun ac feature_name ->
      ac lxor Feature_name.hash feature_name)
  ;;
end
include T
include Comparable.Make_plain_using_comparator (Stable.V1)
include Hashable.Make_plain (T)

let invariant t =
  assert (not (List.is_empty t));
  List.iter t ~f:Feature_name.invariant
;;

let num_parts t = List.length t

let is_root = function
  | [] -> assert false
  | [_] -> true
  | _ :: _ :: _ -> false
;;

let as_root = function
  | [ root ] -> Ok root
  | t -> error "not a root feature" t [%sexp_of: t]
;;

let root t = List.last_exn t

let of_root root = [ root ]

let root_path t = of_root (root t)

let%test_unit _ =
  List.iter
    [ "jane"    , "jane"
    ; "jane/foo", "jane"
    ]
    ~f:(fun (bookmark, expect) ->
      [%test_result: string] (Feature_name.to_string (root (of_string bookmark)))
        ~expect)
;;

let of_string_or_error str = Or_error.try_with (fun () -> of_string str)

let%test_module _ =
  (module struct
    let a = Feature_name.of_string "a"
    let b = Feature_name.of_string "b"
    let%test_unit _ = [%test_eq: t] (of_string "a/b") [b; a]
    let%test_unit _ = [%test_eq: string] "a/b" (to_string [b; a])

    let%test_unit _ = [%test_eq: Sexp.t]
                        ([%sexp_of: Set.t] (Set.of_list [of_string "a/b"]))
                        (Sexp.of_string "(a/b)")
  end)
;;

let extend t name = name :: t

let parts t = List.rev t

let to_relpath t =
  parts t
  |> List.map ~f:Feature_name.to_file_name
  |> Relpath.of_list
;;

let of_parts_exn list =
  if List.is_empty list then failwith "Feature_path.of_parts_exn of empty list";
  List.rev list
;;

let%test_unit _ =
  List.iter
    [ [ "a" ]
    ; [ "a"; "b" ]
    ]
    ~f:(fun strings ->
      [%test_result: string list]
        (List.map ~f:Feature_name.to_string
           (parts (of_string (String.concat strings ~sep:"/"))))
        ~expect:strings)
;;

let parent_and_basename = function
  | [] -> assert false
  | [basename] -> None, basename
  | basename :: (_ :: _ as parent) -> Some parent, basename
;;

let basename t =
  match t with
  | [] -> assert false
  | basename :: _ -> basename
;;

let%test_unit _ =
  List.iter
    [ "a"    , "a"
    ; "a/b"  , "b"
    ; "a/b/c", "c"
    ]
    ~f:(fun (input, expect) ->
      [%test_result: Feature_name.t] (basename (of_string input))
        ~expect:(Feature_name.of_string expect))
;;

let parent t =
  match t with
  | [] -> assert false
  | [_] -> error "feature has no parent" t [%sexp_of: t]
  | _ :: (_ :: _ as parent) -> Ok parent
;;

let compress_parent_exn t =
  match List.rev (parts t) with
  | part :: _ :: parts -> of_parts_exn (List.rev (part :: parts))
  | _ ->
    raise_s [%sexp "Feature_path.compress_parent_exn got feature without enough parts"
                 , (t : t)]
;;

let%test_unit _ =
  List.iter
    [ "a/b"    , "b"
    ; "a/b/c"  , "a/c"
    ; "a/b/c/d", "a/b/d"
    ]
    ~f:(fun (input, expect) ->
      [%test_result: t] ~expect:(of_string expect)
        (compress_parent_exn (of_string input)))
;;

let is_ancestor ~ancestor ~descendant =
  List.is_prefix ~prefix:(List.rev ancestor) (List.rev descendant)
    ~equal:Feature_name.equal
;;

let check_renameable ~from ~to_ =
  let error msg =
    error_s
      [%sexp
        (msg : string),
        { from : t ; to_ : t }
      ]
  in
  if Result.is_error (parent from)
  then error "cannot rename a root feature"
  else if Result.is_error (parent to_)
  then error "cannot rename to a root feature"
  else if not (Feature_name.equal (root from) (root to_))
  then error "features must have the same root"
  else if is_ancestor ~ancestor:from ~descendant:to_
  then error "cannot rename a feature to a descendant of itself"
  else Ok ()
;;

let%test_unit _ =
  List.iter
    [ "a"  , "b"    , false
    ; "a"  , "a/b/c", true
    ; "a/b", "a/b/c", true
    ; "a/d", "a/b/c", false
    ; "a/b", "a"    , false
    ; "a"  , "a"    , true
    ]
    ~f:(fun (ancestor, descendant, expect) ->
      [%test_result: bool]
        (is_ancestor ~ancestor:(of_string ancestor) ~descendant:(of_string descendant))
        ~expect)
;;

let match_ ~prefix of_what =
  lazy (
    let leader =
      match of_what with
      | `Of_full_name    -> "()"
      | `Of_partial_name -> "(.*/)?"
    in
    let prefix, ending_slash =
      match String.chop_suffix prefix ~suffix:"/" with
      | None        -> prefix, ""
      | Some prefix -> prefix, "/"
    in
    let optional_suffix = concat [ "("; ending_slash; "[^/]*"; ")?" ] in
    let regex =
      Regex.create_exn (concat [ "^"
                               ; leader
                               ; "("; Regex.escape prefix; optional_suffix; ")"
                               ; "$"
                               ])
    in
    fun t ->
      match Regex.get_matches regex (to_string t) ~max:1 with
      | Ok [ matches ] -> Regex.Match.get ~sub:(`Index 2) matches
      | Ok ([]|_::_::_) | Error _ -> None)
;;

let%test_unit _ =
  let check (feature_path, prefix, of_what, expect) =
    [%test_result: string option] ~expect
      (force (match_ ~prefix of_what) (of_string feature_path))
  in
  List.iter ~f:check
    [ "jane"               , "ja"          , `Of_partial_name, Some "jane"
    ; "jane/traffic/jpm"   , "jp"          , `Of_partial_name, Some "jpm"
    ; "jane/traffica"      , "traffic/"    , `Of_partial_name, None
    ; "jane/traffic/abc"   , "traffic/a"   , `Of_partial_name, Some "traffic/abc"
    ; "jane/a/bar/foo"     , "f"           , `Of_partial_name, Some "foo"
    ; "jane/a/bar/foo"     , "ba"          , `Of_partial_name, None
    ; "jane/a/bar/foo"     , "a/ba"        , `Of_partial_name, None
    ; "jane"               , "jane/"       , `Of_partial_name, Some "jane"
    ; "jane/a"             , "jane/a/"     , `Of_partial_name, Some "jane/a"
    ; "jane/a"             , "a/"          , `Of_partial_name, Some "a"
    ; "jane/a"             , "jane/"       , `Of_partial_name, Some "jane/a"
    ; "jane/a"             , "jane"        , `Of_partial_name, None
    ; "jane/a/b"           , "a/b/"        , `Of_partial_name, Some "a/b"
    ; "jane/a/b/c"         , "a/b/"        , `Of_partial_name, Some "a/b/c"
    ; "jane"               , "ja"          , `Of_full_name   , Some "jane"
    ; "jane/traffic/jpm"   , "jp"          , `Of_full_name   , None
    ; "jane/traffica"      , "traffic/"    , `Of_full_name   , None
    ; "jane/traffic/abc"   , "traffic/a"   , `Of_full_name   , None
    ; "jane/a/bar/foo"     , "f"           , `Of_full_name   , None
    ; "jane/a/bar/foo"     , "ba"          , `Of_full_name   , None
    ; "jane/a/bar/foo"     , "a/ba"        , `Of_full_name   , None
    ; "jane"               , "jane/"       , `Of_full_name   , Some "jane"
    ; "jane/a"             , "jane/a/"     , `Of_full_name   , Some "jane/a"
    ; "jane/a"             , "a/"          , `Of_full_name   , None
    ; "jane/a"             , "jane/"       , `Of_full_name   , Some "jane/a"
    ; "jane/a"             , "jane"        , `Of_full_name   , None
    ; "jane/a/b"           , "a/b/"        , `Of_full_name   , None
    ; "jane/a/b/c"         , "a/b/"        , `Of_full_name   , None
    ]
;;

let prevent_final_space_if_more_completion_will_follow = function
  | [s] when String.is_suffix s ~suffix:"/" -> [s; s ^ "x"]
  | l -> l
;;

(* Note on completion:
   Let's call a "partial name" some suffix of a feature path beginning either after a
   slash or at the start of the feature path. We want to allow completion to work given
   some prefix of a partial name.

   Example:
   $ fe show fe/ba<Tab>

   rather than having to enter the entire prefix:

   $ fe show jane/fe/ba<Tab>

   Full paths confuse bash when there are multiple matches. Let's say you have:
   -jane/foo/good
   -jane/bar/goodnes

   If an attempt to complete "goo" returned both paths, bash would complete to the longest
   common prefix (which is "jane/") and stop there. Subsequent <Tab>s would complete
   children of jane/, which is confusing

   $ fe show goo<Tab>   # This completes to the following...
   $ fe show jane/      # ...which is pretty confusing

   Thus, the function *extends* the current line by *only* adding text to the end of what
   the user has entered, unless there is only one possible match, in which case the full
   feature path is returned as a singleton to permit bash to replace the user input.

   $ fe show fe/b<Tab>           # This completes to the following...
   $ fe show fe/ba<Tab>          # This shows the following completion candidates...
   fe/basename-feature-fix
   fe/battle-for-more-comments
   $ fe show fe/bas<Tab>         # User enters "s" and now we have a unique match
   $ fe show jane/fe/basename-feature-fix       # So we can complete!
*)
let complete ~iter_features ~prefix of_what =
  let module Feature_path_hash_set = Hash_set in
  let module Hash_set = Core.Hash_set in
  let match_ = match_ ~prefix of_what in
  let result =
    let have_children = Feature_path_hash_set.create () in
    let matches = ref [] in
    iter_features ~f:(fun feature_path ->
      (match parent feature_path with
       | Ok parent -> Hash_set.add have_children parent
       | Error _   -> ());
      (match force match_ feature_path with
       | None -> ()
       | Some match_ -> matches := (feature_path, match_) :: !matches));
    let has_children feature_path = Hash_set.mem have_children feature_path in
    match !matches with
    | [ single_match, _ ] ->
      let str = to_string single_match in
      if has_children single_match then [str ^ "/"] else [str]
    | paths ->
      List.filter_map paths ~f:(fun (feature_path, str) ->
        let str = if has_children feature_path then str ^ "/" else str in
        Option.some_if (String.is_prefix ~prefix str) str)
  in
  prevent_final_space_if_more_completion_will_follow result
;;
