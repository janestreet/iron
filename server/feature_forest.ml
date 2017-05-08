open! Core
open! Import

module Node = struct
  type 'a t =
    { feature_path  : Feature_path.t
    ; mutable value : 'a
    ; parent        : 'a t sexp_opaque option
    ; children      : 'a t Feature_name.Table.t
    }
  [@@deriving fields, sexp_of]

  let rec root t =
    match t.parent with
    | None -> t
    | Some parent -> root parent
  ;;

  let invariant invariant_a t =
    Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~feature_path:(check Feature_path.invariant)
        ~value:(check invariant_a)
        ~parent:(check (function
          | None -> ()
          | Some parent ->
            assert (phys_equal t
                      (Hashtbl.find_exn parent.children
                         (Feature_path.basename t.feature_path)))))
        ~children:ignore)
  ;;

  let create feature_path value parent =
    { feature_path
    ; value
    ; parent
    ; children = Feature_name.Table.create ()
    }
  ;;

  let change t f = t.value <- f t.value

  let add_child t child_name child =
    Hashtbl.add_exn t.children ~key:child_name ~data:child;
  ;;

  let remove_child t child_name =
    assert (Hashtbl.mem t.children child_name);
    Hashtbl.remove t.children child_name;
  ;;

  let has_children t = not (Hashtbl.is_empty t.children)

  let rec iter_strict_descendants node ~f =
    Hashtbl.iteri node.children ~f:(fun ~key:_ ~data:child_node ->
      f child_node.value;
      iter_strict_descendants child_node ~f)
  ;;

  let iter_descendants t ~f = f t.value; iter_strict_descendants t ~f
end

type 'a t =
  { node_by_path : 'a Node.t Feature_path.Table.t
  ; root_by_name : 'a Node.t Feature_name.Table.t
  }
[@@deriving fields]

let sexp_of_t sexp_of_a t =
  Hashtbl.data t.root_by_name |> [%sexp_of: a Node.t list]
;;

let mem t feature_path  = Hashtbl.mem t.node_by_path feature_path

let find_node t feature_path =
  match Hashtbl.find t.node_by_path feature_path with
  | Some node -> Ok node
  | None -> error "no such feature" feature_path [%sexp_of: Feature_path.t]
;;

let find_root t feature_name =
  match Hashtbl.find t.root_by_name feature_name with
  | Some node -> Ok node.value
  | None -> error "no such root feature" feature_name [%sexp_of: Feature_name.t]
;;

let root_of t feature_path =
  Or_error.map (find_node t feature_path) ~f:(fun node ->
    Node.value (Node.root node))
;;

let find_node_exn t feature_path = Hashtbl.find_exn t.node_by_path feature_path

let find t feature_path = Or_error.map (find_node t feature_path) ~f:Node.value

let invariant invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~node_by_path:(check (fun node_by_path ->
        Hashtbl.iteri node_by_path ~f:(fun ~key:feature_path ~data:({ Node. children; _ } as node) ->
          Node.invariant invariant_a node;
          (match Feature_path.parent_and_basename feature_path with
           | None, root ->
             assert (Hashtbl.mem t.root_by_name root);
           | Some parent_path, child ->
             assert (Hashtbl.mem (Node.children (find_node_exn t parent_path)) child);
             assert (mem t parent_path));
          Hashtbl.iteri children ~f:(fun ~key:child ~data:node ->
            assert (phys_equal node
                      (find_node_exn t (Feature_path.extend feature_path child)))))))
      ~root_by_name:(check (fun root_by_name ->
        Hashtbl.iteri root_by_name ~f:(fun ~key:feature_name ~data:node ->
          assert (phys_equal
                    (find_node_exn t (Feature_path.of_root feature_name))
                    node)))))
;;

let roots t =
  List.map (Hashtbl.to_alist t.root_by_name) ~f:(fun (feature_name, node) ->
    (feature_name, node.value))
;;

let create () =
  { node_by_path = Feature_path.Table.create ()
  ; root_by_name = Feature_name.Table.create ()
  }
;;

let iteri (t : _ t) ~f =
  Hashtbl.iteri t.node_by_path ~f:(fun ~key ~data:node -> f key node.value)
;;

let iteri_roots t ~f =
  Hashtbl.iteri t.root_by_name ~f:(fun ~key ~data:node -> f key node.value)
;;

let iter_children t feature_path ~f =
  Hashtbl.iteri (find_node_exn t feature_path).children
    ~f:(fun ~key:_ ~data:node -> f node.value)
;;

let iter_descendants t feature_path ~f =
  Node.iter_descendants (find_node_exn t feature_path) ~f
;;

let has_children_exn t feature_path = Node.has_children (find_node_exn t feature_path)

let check_add t feature_path =
  if mem t feature_path
  then error "feature already exists" feature_path [%sexp_of: Feature_path.t]
  else (
    match Feature_path.parent feature_path with
    | Error _ -> Ok ()
    | Ok parent_path ->
      if mem t parent_path
      then Ok ()
      else error "parent feature does not exist" parent_path [%sexp_of: Feature_path.t])
;;

let add_exn t feature_path value =
  ok_exn (check_add t feature_path);
  let node =
    match Feature_path.parent_and_basename feature_path with
    | None, root ->
      let node = Node.create feature_path value None in
      Hashtbl.add_exn t.root_by_name ~key:root ~data:node;
      node
    | Some parent_path, child_name ->
      let parent = find_node_exn t parent_path in
      let node = Node.create feature_path value (Some parent) in
      Node.add_child parent child_name node;
      node
  in
  Hashtbl.add_exn t.node_by_path ~key:feature_path ~data:node;
;;

let rec add_ancestors t feature_path ~f =
  if not (mem t feature_path)
  then (
    (match Feature_path.parent feature_path with
     | Error _ -> ()
     | Ok parent_path -> add_ancestors t parent_path ~f);
    add_exn t feature_path (f ()))
;;

let change_exn t feature_path f = Node.change (find_node_exn t feature_path) f

let remove_exn t feature_path =
  let node = find_node_exn t feature_path in
  if Node.has_children node
  then raise_s [%sexp "feature has children"
                    , ((Hashtbl.keys (Node.children node) |> Feature_name.Set.of_list)
                       : Feature_name.Set.t)];
  Hashtbl.remove t.node_by_path feature_path;
  match Feature_path.parent_and_basename feature_path with
  | None, root         -> Hashtbl.remove t.root_by_name root
  | Some parent, child -> Node.remove_child (find_node_exn t parent) child
;;

let strict_descendants t start =
  match find_node t start with
  | Error _ as e -> e
  | Ok node -> Ok (list_of_iter (Node.iter_strict_descendants node))
;;

let%test_unit _ =
  let t = create () in
  List.iter
    [ "a"
    ; "a/b"
    ]
    ~f:(fun string ->
      add_exn t (Feature_path.of_string string) ();
      invariant ignore t)
;;

let complete t ~prefix of_what =
  Feature_path.complete
    ~iter_features:(fun ~f -> iteri t ~f:(fun key _node -> f key))
    ~prefix
    of_what
;;

let rec list_descendants node_by_name ~depth accum =
  if depth = 0
  then accum
  else (
    let depth = depth - 1 in
    Hashtbl.fold node_by_name ~init:accum ~f:(fun ~key:_ ~data:node accum ->
      list_descendants node.Node.children ~depth
        ((node.feature_path, node.value) :: accum)))
;;

let list t ~descendants_of ~depth =
  if depth < 0
  then error "negative depth is not allowed" depth [%sexp_of: int]
  else (
    match (descendants_of : Which_ancestor.t) with
    | Any_root -> Ok (list_descendants t.root_by_name ~depth [])
    | Feature feature_path ->
      Or_error.map (find_node t feature_path) ~f:(fun node ->
        list_descendants node.children ~depth [ (node.feature_path, node.value) ]))
;;
