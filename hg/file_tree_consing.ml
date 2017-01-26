open! Core
open! Import

include File_tree_consing_intf

module type X = sig
  type t [@@deriving compare, sexp_of]
  val hash : t -> int
  val module_name : string
end

module Make (X : X) () : S with type data := X.t = struct

  module T = struct
    type t =
      { files : X.t Path_in_repo.Map.t
      ; dirs  : t File_name.Map.t
      ; hash  : int
      }
    [@@deriving compare, fields, sexp_of]

    let hash t = t.hash
    let module_name = X.module_name
  end

  include T
  module H = Hash_consing.Make (T) ()

  let invariant t =
    let rec aux dir_path t =
      (* Check files at this level *)
      Invariant.invariant [%here] (dir_path, t) [%sexp_of: Path_in_repo.t * t]
        (fun () -> Map.iteri t.files ~f:(fun ~key:path_in_repo ~data:_ ->
           [%test_result: Path_in_repo.t] ~expect:dir_path
             (Path_in_repo.parent_exn path_in_repo)));
      (* And check children directories recursively *)
      Map.iteri t.dirs ~f:(fun ~key:subdir ~data:t ->
        aux (Path_in_repo.extend dir_path subdir) t);
    in
    aux Path_in_repo.root t
  ;;

  let shared_t = H.shared_t

  module Not_hashed = struct
    type t =
      { files : X.t Path_in_repo.Map.t
      ; dirs  : t File_name.Map.t
      }

    let empty =
      { files = Path_in_repo.Map.empty
      ; dirs  = File_name.Map.empty
      }
    ;;

    (* Since this function is called a high number of time and is quite central, it seems
       worth it to have it rather not be a closure apply *)
    let rec add path_in_repo data t parent = function
      | [] -> { t with files = Map.add t.files ~key:path_in_repo ~data }
      | hd :: tl ->
        let dir = Option.value (Map.find t.dirs parent) ~default:empty in
        let dir = add path_in_repo data dir hd tl in
        { t with dirs = Map.add t.dirs ~key:parent ~data:dir }
    ;;
  end

  let rec finish { Not_hashed. files; dirs } =
    let dirs = Map.map dirs ~f:finish in
    let hash =
      Hash_consing.fold_hash
        (Hash_consing.map_hash Path_in_repo.hash X.hash files)
        (Hash_consing.map_hash File_name.hash T.hash dirs)
    in
    shared_t { T. files; dirs; hash }
  ;;

  let of_alist files =
    List.fold files ~init:Not_hashed.empty ~f:(fun t (path_in_repo, data) ->
      match Path_in_repo.parts path_in_repo with
      | [] -> assert false (* the root is a directory, so no attributes *)
      | hd :: tl -> Not_hashed.add path_in_repo data t hd tl)
    |> finish
  ;;

  let rec fold t ~init:acc ~f =
    let acc = Map.fold t.files ~init:acc ~f in
    Map.fold t.dirs ~init:acc ~f:(fun ~key:_dirname ~data acc -> fold data ~init:acc ~f)
  ;;

  let to_alist t = fold t ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc)
end

module Cr_comments = Make (struct
    type t = Cr_comment.Structurally_compared.t list [@@deriving compare, sexp_of]
    let module_name = "Cr_comments_tree"
    let hash t = Hash_consing.list_hash Cr_comment.hash t
  end) ()

module Cr_soons = Make (struct
    type t = Cr_soon.Structurally_compared.t list [@@deriving compare, sexp_of]
    let module_name = "Cr_soons_tree"
    let hash t =
      Hash_consing.list_hash
        Cr_soon.Compare_ignoring_minor_text_changes.hash
        t
    ;;
  end) ()

module Obligations = Make (struct
    type t = Review_attributes.t [@@deriving compare, sexp_of]
    let module_name = "Obligations_tree"
    let hash = Review_attributes.hash
  end) ()

let%test_module _ =
  (module struct

    let debug = false

    type t = { x : int } [@@deriving compare, sexp_of]

    module M = Make (struct
        type nonrec t = t [@@deriving compare, sexp_of]
        let module_name = "File_tree_consing_test"
        let hash { x } = Hashtbl.hash (x : int)
      end) ()

    let test files =
      let sort files =
        List.sort files ~cmp:(fun (p1, _) (p2, _) ->
          Path_in_repo.default_review_compare p1 p2)
      in
      let files = sort files in
      let tree = M.of_alist files in
      M.invariant tree;
      if debug
      then (
        List.iter files ~f:(fun (p, v) ->
          Printf.printf !"%{Path_in_repo} %{Sexp}\n" p ([%sexp_of: t] v));
        Debug.eprints "files" files [%sexp_of: (Path_in_repo.t * t) list];
        Debug.eprints "tree" tree [%sexp_of: M.t]);
      [%test_result: (Path_in_repo.t * t) list]
        ~expect:files
        (tree |> M.to_alist |> sort)
    ;;

    let%test_unit _ =
      let files =
        [ "a", { x = 0 }
        ; "b", { x = 1 }
        ; "c", { x = 2 }
        ]
      in
      let abc = [ "a"; "b"; "c" ] in
      let rec gen_t ~depth path =
        if depth = 0 then []
        else (
          let files =
            List.rev_map files ~f:(fun (file, v) ->
              Path_in_repo.extend path (File_name.of_string file), v)
          in
          List.rev_append files
            (List.concat_map abc ~f:(fun str ->
               gen_t ~depth:(pred depth)
                 (Path_in_repo.extend path (File_name.of_string str)))))
      in
      test (gen_t ~depth:4 Path_in_repo.root)
    ;;

    let%test_unit _ =
      let files =
        List.map ~f:(fun (file, v) -> Path_in_repo.of_string file, v)
          [ "a/b/a", { x = 0 }
          ; "a/b/b", { x = 1 }
          ; "a/b/c", { x = 2 }
          ]
      in
      test files
    ;;
  end)
