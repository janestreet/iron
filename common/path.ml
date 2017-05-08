module Stable = struct
  open Core.Core_stable

  module Parts = struct
    module V1 = struct

      type t = File_name.Stable.V1.t list [@@deriving bin_io, compare, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 296be80010ace497614f92952e5510c4 |}]
      ;;

      open Core

      let join t =
        String.concat ~sep:Filename.dir_sep (List.map t ~f:File_name.to_string)
      ;;

      let to_string_absolute t = "/" ^ join t

      let to_string_relative = function
        | [] -> "."
        | t  -> join t
      ;;
    end
  end

  module V1 = struct

    (* The list is ordered [root; dir; ... dir; leaf].
       E.g.,
       "a/b/c.ml"  -> Relpath ["a" ; "b" ; "c.ml"]
       "/a/b/c.ml" -> Abspath ["a" ; "b" ; "c.ml"]
       ""          -> Relpath []                        Boundary case
       "/"         -> Abspath []                        Boundary case
       "a/b/"      -> Relpath ["a" ; "b"]               A final slash is ignored.
    *)
    type t =
      | Abspath of Parts.V1.t
      | Relpath of Parts.V1.t
    [@@deriving bin_io, compare]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| f37b8e55abbd6d0520508f62ff92e006 |}]
    ;;

    let to_string = function
      | Abspath a -> Parts.V1.to_string_absolute a
      | Relpath r -> Parts.V1.to_string_relative r
    ;;

    let of_string string =      (* Used by the various of_string's below. *)
      let open Core in
      (* [Filename.parts] splits a filename at slashes.  It always returns a non-empty
         list whose first element is either "/" or ".".  - [File_name.of_string] will
         check the path elements for nul chars, for us. *)
      match Filename.parts string with
      | [] -> raise_s [%sexp "Filename.parts returned empty list.", (string : string)]
      | d1 :: rest ->
        let elts = List.map rest ~f:File_name.of_string in
        match d1 with
        | "." -> Relpath elts
        | "/" -> Abspath elts
        | _   ->
          raise_s [%sexp
            "Filename.parts produced unexpected list, not beginning with '.' or '/'."
          , ((d1::rest) : string list)]
    ;;

    include Sexpable.Of_stringable.V1 (struct
        type nonrec t = t
        let of_string = of_string
        let to_string = to_string
      end)
  end

  module Abspath = struct

    module V1 = struct
      include Parts.V1

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 296be80010ace497614f92952e5510c4 |}]
      ;;

      let to_string = to_string_absolute

      let of_string string =
        match V1.of_string string with
        | Abspath file_names -> file_names
        | Relpath _ ->
          Core.raise_s [%sexp "Abspath.of_string got relative path", (string : string)]
      ;;

      include Sexpable.Of_stringable.V1 (struct
          type nonrec t = t
          let of_string = of_string
          let to_string = to_string
        end)
    end
  end

  module Relpath = struct
    module V1 = struct
      include Parts.V1

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 296be80010ace497614f92952e5510c4 |}]
      ;;

      let to_string = to_string_relative

      let of_string string =
        match V1.of_string string with
        | Relpath file_names -> file_names
        | Abspath _ ->
          Core.raise_s [%sexp "Relpath.of_string got absolute path", (string  : string)]
      ;;

      include Sexpable.Of_stringable.V1 (struct
          type nonrec t = t
          let of_string = of_string
          let to_string = to_string
        end)
    end
  end

end

open Core
open Import

(* Some internal, unexported utilities
 ******************************************************************************)

module T0 = struct
  (* This type only used internally to resolve a reference circularity. *)
  include Stable.V1

end

module Parts = struct
  (* The list is ordered [root; dir; ... dir; leaf]. E.g.:
     {v
       "/a/b/c.ml" -> ["a" ; "b" ; "c.ml"]
       "/"         -> []                        Boundary case
     v}
  *)
  include Stable.Parts.V1

  let invariant t = List.iter t ~f:File_name.invariant

  let append t1 r2 = List.append t1 r2

  let extend t file_name = t @ [ file_name ]

  let of_list file_names = file_names

  let to_list t = t

  let join elts =
    String.concat ~sep:Filename.dir_sep (List.map elts ~f:File_name.to_string)
  ;;

  let hash elts =
    let rotate_left x i =
      let  i = i % Int.num_bits in
      let hi = Int.shift_left x i in
      let lo = Int.shift_right_logical x (Int.num_bits - i) in
      Int.bit_or hi lo
    in
    List.fold elts ~init:0 ~f:(fun acc x ->
      Int.bit_xor (rotate_left acc 1) (File_name.hash x))
  ;;

  let of_string string =
    match T0.of_string string with
    | Abspath t -> t
    | Relpath t -> t
  ;;

  let parent t =
    match t with
    | []       -> None
    | f1 :: fs ->
      let rec droplast f1 fs = (* Drop last elt of f1::fs *)
        match fs with
        | []          -> []
        | f2 :: frest -> f1 :: (droplast f2 frest)
      in
      Some (droplast f1 fs)
  ;;

  let parent_exn t =
    match parent t with
    | Some t -> t
    | None -> failwith "parent_exn of empty path"
  ;;

  let%test_unit _ =
    List.iter
      [ "a/b/c", Some "a/b"
      ; "a"    , Some ""
      ; ""     , None
      ]
      ~f:(fun (input, expect) ->
        [%test_result: t option]
          (parent (of_string input)) ~expect:(Option.map expect ~f:of_string))
  ;;

  let is_prefix ~prefix parts =
    List.is_prefix ~prefix parts ~equal:File_name.equal
  ;;

  let%test_unit _ =
    let a = File_name.of_string "a" in
    let b = File_name.of_string "b" in
    List.iter ~f:(fun (prefix, parts, expect) ->
      [%test_result: bool]
        (is_prefix ~prefix parts) ~expect)
      [ [],  [],    true
      ; [],  [a],   true
      ; [a], [],    false
      ; [a], [a],   true
      ; [a], [a;b], true
      ; [a], [b],   false
      ]
  ;;
end

module Abspath = struct

  module Relpath = Stable.Relpath.V1

  module Stable = Stable.Abspath

  module T = struct

    include Stable.V1
    include (Parts : module type of Parts with type t := Parts.t)

    (* The bit_not separates absolute & relative hashes. *)
    let hash t = Int.bit_not (Parts.hash t)

  end

  include T

  include Identifiable.Make (struct
      let module_name = "Iron_common.Abspath"
      include T
      include Sexpable.Of_stringable (T)
    end)

  let alphabetic_compare a b = String.alphabetic_compare (to_string a) (to_string b)

  let root = []

  let is_root = List.is_empty

  let last_exn t =
    if is_root t then failwith "Abspath.last_exn of empty path";
    List.last_exn t
  ;;

  let split_dir_file_exn t =
    if is_root t then failwith "Abspath.split_dir_file_exn of root path";
    Option.value_exn (parent t), last_exn t
  ;;

  let program_started_in =
    Or_error.try_with (fun () -> Unix.getcwd () |> of_string)
  ;;

  let dev_null = of_string "/dev/null"

  let rec chop_prefix_internal ~prefix t =
    match prefix, t with
    | ([], _)    -> Some t
    | (_::_, []) -> None
    | (prefix1 :: prefix, t1 :: t) ->
      if File_name.equal prefix1 t1
      then chop_prefix_internal ~prefix t
      else None
  ;;

  let chop_prefix ~prefix t =
    match chop_prefix_internal ~prefix t with
    | Some t -> Ok t
    | None -> error "chop_prefix got non-prefix" (`prefix prefix, t)
                [%sexp_of: [ `prefix of t ] * t]
  ;;

  let chop_suffix t ~suffix =
    match chop_prefix_internal ~prefix:(List.rev suffix) (List.rev t) with
    | Some t -> Ok (List.rev t)
    | None -> error "chop_suffix got non-suffix" (t, `suffix suffix)
                [%sexp_of: t * [ `suffix of t ]]
  ;;

  let%test_unit _ =
    List.iter
      [ "/"   , ""   , Some "/"
      ; "/"   , "."  , Some "/"
      ; "/"   , "a"  , None
      ; "/a"  , ""   , Some "/a"
      ; "/a"  , "a"  , Some "/"
      ; "/a"  , "b"  , None
      ; "/a/b", ""   , Some "/a/b"
      ; "/a/b", "b"  , Some "/a"
      ; "/a/b", "a/b", Some "/"
      ] ~f:(fun (t, suffix, expect) ->
        [%test_result: t option] ~expect:(Option.map expect ~f:of_string)
          (match chop_suffix (of_string t) ~suffix:(Relpath.of_string suffix) with
           | Ok t -> Some t
           | Error _ -> None))
  ;;

  open Async

  let file_exists_exn t =
    Sys.file_exists_exn (to_string t)
  ;;

  let rm_rf_exn dst =
    Unix.system_exn (sprintf !"/bin/rm -rf -- %{sh}" (to_string dst))
  ;;

  let in_dir t ~f =
    Unix.getcwd ()
    >>= fun cwd ->
    Unix.chdir (to_string t)
    >>= fun () ->
    Monitor.protect f ~finally:(fun () -> Unix.chdir cwd)
  ;;

  let rename_exn ~src ~dst__delete_if_exists:dst =
    let open Async in
    Monitor.try_with ~extract_exn:true (fun () ->
      let dst_parent = to_string (parent_exn dst) in
      Unix.mkdir ~p:() dst_parent
      >>= fun () ->
      rm_rf_exn dst
      >>= fun () ->
      Sys.rename (to_string src) (to_string dst))
    >>| function
    | Ok () -> ()
    | Error exn ->
      raise_s
        [%sexp
          "Abspath.rename_exn failed",
          { src : t
          ; dst : t
          ; exn : Exn.t
          }
        ]
  ;;

  let simplify_dotdots_syntax filenames =
    (* Convert filenames to a (nat, File_name.t list) pair, where the
       nat says how many ..'s to prepend to the ..-free file-name list. *)
    let leading, filenames =
      List.fold_right filenames ~init:(0, []) ~f:(fun f (leading, filenames) ->
        if File_name.equal f File_name.dotdot
        then (leading+1, filenames)
        else if Int.(>) leading 0
        then (leading-1, filenames)
        else (0, f::filenames))
    in
    if Int.(=) 0 leading
    then Ok filenames
    else
      Or_error.error_s
        [%sexp "Abspath.simplify_dotdots_syntax: Invalid abspath"
             , (filenames : t)]
  ;;

  let%test_unit _ =
    List.iter
      [ ["a"; "b"; ".."; "c"] , Some ["a"; "c"]
      ; ["a"; ".."; ".."; "c"], None
      ]
      ~f:(fun (input, expected_output) ->
        [%test_result: t option]
          (match simplify_dotdots_syntax (List.map ~f:File_name.of_string input) with
           | Ok t -> Some t
           | Error _ -> None)
          ~expect:(Option.map expected_output ~f:(List.map ~f:File_name.of_string)))
  ;;
end

type abspath = Abspath.t

module Relpath = struct

  module Stable = Stable.Relpath

  module T = struct
    include Stable.V1
    include (Parts : module type of Parts with type t := Parts.t)
  end

  include T

  include Identifiable.Make (struct
      let module_name = "Iron_common.Relpath"
      include T
      include Sexpable.Of_stringable (T)
    end)

  let empty = []

  let is_empty = List.is_empty

  let append t1 t2 = List.append t1 t2

  let extend t file_name = t @ [ file_name ]

  let of_list file_names = file_names

  let parts t = t

  let last_exn t =
    if is_empty t then failwith "Relpath.last_exn of empty path";
    List.last_exn t
  ;;

  let split_dir_file_exn t =
    if is_empty t then failwith "Relpath.split_dir_file_exn of empty path";
    Option.value_exn (parent t), last_exn t
  ;;

  let split_first t =
    match t with
    | [] -> None
    | file_name :: t -> Some (file_name, t)
  ;;

  let alphabetic_compare a b = String.alphabetic_compare (to_string a) (to_string b)

  let rec default_review_compare t1 t2 =
    match t1, t2 with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | [ file1 ], [ file2 ] -> File_name.default_review_compare file1 file2
    | [ _ ], _ :: _ :: _ -> -1
    | _ :: _ :: _, [ _ ] -> 1
    | dir1 :: (_ :: _ as t1), dir2 :: (_ :: _ as t2) ->
      match File_name.alphabetic_compare dir1 dir2 with
      | 0 -> default_review_compare t1 t2
      | c -> c
  ;;

  let%test_module _ =
    (module struct
      let%test_unit _ =
        List.iter ~f:(fun (a, b, ordering) ->
          [%test_result: Ordering.t] ~expect:ordering
            (Ordering.of_int (default_review_compare (of_string a) (of_string b))))
          [ "a/a.mli"      , "a/a.ml"   , Less
          ; "a/a_intf.ml"  , "a/a.mli"  , Less
          ; "a/a_intf.ml"  , "a/a.ml"   , Less
          ]
      ;;

      let%test_unit _ =
        let check a ~expect =
          [%test_result: string list] ~expect
            (a
             |> List.map ~f:of_string
             |> List.sort ~cmp:default_review_compare
             |> List.map ~f:to_string)
        in
        check [ "a.ml" ; "b.mli" ; "a.mli" ; "a_intf.ml" ]
          ~expect:[ "a_intf.ml" ; "a.mli" ; "a.ml" ; "b.mli" ];

        check
          [ "base/core/.fe.sexp"
          ; "base/core/.hgignore.in"
          ; "base/core/COPYRIGHT.txt"
          ; "base/core/kernel/lib/.fe.sexp"
          ; "base/core/lib/weak_hashtbl.ml"
          ; "base/core/mlton-license.txt"
          ]
          ~expect:
            [ "base/core/.fe.sexp"
            ; "base/core/.hgignore.in"
            ; "base/core/COPYRIGHT.txt"
            ; "base/core/mlton-license.txt"
            ; "base/core/kernel/lib/.fe.sexp"
            ; "base/core/lib/weak_hashtbl.ml"
            ]
      ;;
    end)
  ;;

  let chop_prefix = Abspath.chop_prefix
end

(* One might consider using phantom types to distinguish absolute & relative paths,
   so we could ensure things statically. *)

module T = struct

  include T0

  let module_name = "Iron_common.Path"

  let hash = function
    | Relpath r -> Relpath.hash r
    | Abspath a -> Abspath.hash a
  ;;
end

include T

include Identifiable.Make (struct
    include T
    include Sexpable.Of_stringable (T)
  end)

let alphabetic_compare t1 t2 = String.alphabetic_compare (to_string t1) (to_string t2)

let root  = Abspath Abspath.root
let empty = Relpath Relpath.empty

let append p1 p2 =
  match p2 with
  | Abspath _  -> p2
  | Relpath r2 ->
    match p1 with
    | Relpath r1 -> Relpath (Relpath.append r1 r2)
    | Abspath a1 -> Abspath (Abspath.append a1 r2)
;;

let is_prefix ~prefix path =
  match prefix, path with
  | (Relpath _, Abspath _) | (Abspath _, Relpath _) -> false
  | (Abspath prefix, Abspath path)
  | (Relpath prefix, Relpath path) -> Parts.is_prefix ~prefix path
;;

let%test _ = is_prefix ~prefix:(of_string "a/b")  (of_string "a/b")    ;;
let%test _ = is_prefix ~prefix:(of_string "a/b")  (of_string "a/b/c")  ;;
let%test _ = is_prefix ~prefix:(of_string "/a/b") (of_string "/a/b")   ;;
let%test _ = is_prefix ~prefix:(of_string "/a/b") (of_string "/a/b/c") ;;

let%test _ = not( is_prefix ~prefix:(of_string "a/b") (of_string "/b")   );;
let%test _ = not( is_prefix ~prefix:(of_string "a/b") (of_string "/a/c") );;
let%test _ = not( is_prefix ~prefix:(of_string "a")   (of_string "/a")   );;
let%test _ = not( is_prefix ~prefix:(of_string "a")   (of_string "/a/b") );;
let%test _ = not( is_prefix ~prefix:(of_string "/a")  (of_string "a")    );;

let%test _ = is_prefix ~prefix:(of_string "")  (of_string "a/b/c")  ;;
let%test _ = is_prefix ~prefix:(of_string "")  (of_string "")       ;;
let%test _ = is_prefix ~prefix:(of_string "/") (of_string "/a/b/c") ;;
let%test _ = is_prefix ~prefix:(of_string "/") (of_string "/")      ;;

let%test _ = not( is_prefix ~prefix:(of_string "/a")  (of_string "/") );;
let%test _ = not( is_prefix ~prefix:(of_string "a")   (of_string "")  );;

let chop_prefix ~prefix path =
  match (prefix,path) with
  | (Relpath _, Abspath _) ->
    error "relative path cannot be prefix of absolute path" (`prefix prefix, path)
      [%sexp_of: [ `prefix of t ] * t]
  | (Abspath _, Relpath _) ->
    error "absolute path cannot be prefix of relative path" (`prefix prefix, path)
      [%sexp_of: [ `prefix of t ] * t]
  | (Abspath prefix, Abspath path_elts)
  | (Relpath prefix, Relpath path_elts) ->
    Or_error.map (Abspath.chop_prefix ~prefix path_elts)
      ~f:(fun path -> Relpath path)
;;

let%test_unit _ =
  [%test_result: t Or_error.t] ~expect:(Ok (of_string "b/c"))
    (chop_prefix ~prefix:(of_string "/a") (of_string "/a/b/c"))
;;

let compare p1 p2 =
  let path_comp = [%compare: File_name.t list] in
  match (p1,p2) with
  | ((Relpath eltsa), (Relpath eltsb)) -> path_comp eltsa eltsb
  | ((Abspath eltsa), (Abspath eltsb)) -> path_comp eltsa eltsb
  | ((Relpath _), (Abspath _))         -> -1 (* Arbitrarily make relative < absolute *)
  | ((Abspath _), (Relpath _))         ->  1 (* to make this a total order.          *)
;;

let%test _ = Int.(=) 0 (compare (of_string "a/b/c.ml") (of_string "a/b/c.ml")) ;;
let%test _ = Int.(>) 0 (compare (of_string "a.c")      (of_string "b.c"))      ;;
let%test _ = Int.(>) 0 (compare (of_string "a/b.c")    (of_string "z.c"))      ;;
let%test _ = Int.(>) 0 (compare (of_string "a")        (of_string "a/b.c"))    ;;
let%test _ = Int.(>) 0 (compare (of_string "a")        (of_string "/a"))       ;; (* rel < abs *)

let resolve t ~relative_to =
  match t with
  | Relpath rel -> Abspath.append relative_to rel
  | Abspath abs -> abs
;;

let resolve_relative_to_program_started_in t =
  match t with
  | Relpath rel -> Abspath.append (ok_exn Abspath.program_started_in) rel
  | Abspath abs -> abs
;;

let%test_unit _ =
  let test = resolve (of_string "/c/d") ~relative_to:(Abspath.of_string "/a/b") in
  [%test_result: Abspath.t] test ~expect: (Abspath.of_string "/c/d")
;;

let%test_unit _ =
  let test = resolve (of_string "c/d") ~relative_to:(Abspath.of_string "/a/b") in
  [%test_result: Abspath.t] test ~expect: (Abspath.of_string "/a/b/c/d")
;;

let with_temp_dir ?in_dir name ~f =
  let open Async in
  let%bind in_dir =
    match in_dir with
    | Some in_dir ->
      let%map () = Unix.mkdir ~p:() (Abspath.to_string in_dir) in
      in_dir
    | None ->
      return (Abspath.of_string Filename.temp_dir_name)
  in
  let%bind dir = Unix.mkdtemp (Abspath.to_string (Abspath.extend in_dir name)) in
  let dir = Abspath.of_string dir in
  let cleanup = Cleanup.create (fun () -> Abspath.rm_rf_exn dir) in
  Monitor.protect (fun () -> f dir)
    ~finally:(fun () -> Cleanup.run cleanup);
;;
