module Stable = struct

  open Import_stable

  module Cr_soon_multiset = Cr_soon_multiset.Stable

  module Root = struct

    module V1 = struct
      type t =
        { tip_cr_soons : Cr_soon_multiset.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 6c52cf608251d702b8a5523217477514 |}]
      ;;
    end
  end

  module Non_root = struct

    module V1 = struct
      type t =
        { active : Cr_soon_multiset.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 9956ecb3727cf1ec95b9967b1c0801c0 |}]
      ;;
    end
  end

  module In_feature = struct

    module V1 = struct

      module Info = struct
        type t =
          | Root     of Root.V1.t
          | Non_root of Non_root.V1.t
        [@@deriving bin_io, compare, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 323ad25e263162c713ed99907f854f5b |}]
        ;;
      end

      type t =
        { feature_path : Feature_path.V1.t
        ; info         : Info.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 63ba2df9755430bd4d4842abf1bf20af |}]
      ;;
    end
  end

end

open Core
open Import

(* We don't use CR-soons if there are conflicts or invalid obligations. *)
let can_use_cr_soons (facts : Rev_facts.t) =
  let rev = Rev_facts.rev facts in
  ok_exn (Rev_facts.Is_conflict_free.check         facts.is_conflict_free      rev)
  && ok_exn (Rev_facts.Obligations_are_valid.check facts.obligations_are_valid rev)
;;

module Root = struct

  include Stable.Root.V1

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~tip_cr_soons:(check Cr_soon_multiset.invariant))
  ;;

  let equal t1 t2 = Cr_soon_multiset.equal t1.tip_cr_soons t2.tip_cr_soons

  let create ~feature_path ~tip_facts ~tip_cr_soons =
    try
      if not (Feature_path.is_root feature_path)
      then failwith "feature path is not a root";
      if not (can_use_cr_soons tip_facts) then failwith "unacceptable tip facts";
      Ok { tip_cr_soons }
    with exn ->
      error "Root.create" (feature_path, tip_facts, exn)
        [%sexp_of: Feature_path.t * Rev_facts.t * exn]
  ;;
end

module Non_root = struct

  include Stable.Non_root.V1

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~active:(check Cr_soon_multiset.invariant))
  ;;

  let equal t1 t2 =
    Cr_soon_multiset.equal t1.active t2.active
    (* [Cr_soon_multiset.equal] doesn't take the feature path into account, because
       [C_soon_multiset] is all about comparing CR-soons across features.  However, we
       don't want to consider two non-root sets of CR-soons to be equal if they have
       different feature paths, so we also compare the feature paths here.  All CR-soons
       in a [t] have the same feature path, so comparing a single one is sufficient. *)
    && Option.equal Feature_path.equal
         (Cr_soon_multiset.choose_feature_path t1.active)
         (Cr_soon_multiset.choose_feature_path t2.active)
  ;;

  let rename_feature t ~from ~to_ =
    { active = Cr_soon_multiset.rename_feature t.active ~from ~to_ }
  ;;

  let create ~feature_path ~base_facts ~base_cr_soons ~tip_facts ~tip_cr_soons
        ~base_is_ancestor_of_tip =
    try
      if Feature_path.is_root feature_path then failwith "feature path is a root";
      if not (can_use_cr_soons base_facts) then failwith "unacceptable base facts";
      if not (can_use_cr_soons tip_facts)  then failwith "unacceptable tip facts";
      let base = Rev_facts.rev base_facts in
      let tip  = Rev_facts.rev tip_facts  in
      if not (ok_exn (Rev_facts.Is_ancestor.check base_is_ancestor_of_tip
                        ~ancestor:base ~descendant:tip))
      then failwith "base is not ancestor of tip";
      Ok { active = Cr_soon_multiset.diff base_cr_soons tip_cr_soons }
    with exn ->
      error "Non_root.create" (feature_path, base_facts, tip_facts, exn)
        [%sexp_of: Feature_path.t * Rev_facts.t * Rev_facts.t * exn]
  ;;
end

module In_feature = struct

  module Root     = Root
  module Non_root = Non_root

  include Stable.In_feature.V1

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~feature_path:(check Feature_path.invariant)
        ~info:(check (function
          | Info.Root f -> Root.invariant f
          | Non_root f -> Non_root.invariant f)))
  ;;

  let equal t1 t2 =
    Feature_path.equal t1.feature_path t2.feature_path
    && (match t1.info, t2.info with
      | Root r1, Root r2 -> Root.equal r1 r2
      | Non_root n1, Non_root n2 -> Non_root.equal n1 n2
      | (Root _ | Non_root _), _ -> false)
  ;;

  let create ~feature_path ~base_facts ~base_cr_soons ~tip_facts ~tip_cr_soons
        ~base_is_ancestor_of_tip =
    Or_error.try_with (fun () ->
      let is_root = Feature_path.is_root feature_path in
      let base_cr_soons = Cr_soon_multiset.create base_cr_soons feature_path in
      let tip_cr_soons  = Cr_soon_multiset.create tip_cr_soons  feature_path in
      let info =
        if is_root
        then Info.Root (ok_exn (Root.create ~feature_path ~tip_facts ~tip_cr_soons))
        else Non_root (ok_exn (Non_root.create ~feature_path ~base_facts ~base_cr_soons
                                 ~tip_facts ~tip_cr_soons ~base_is_ancestor_of_tip))
      in
      { feature_path; info })
  ;;

  let rename_non_root ({ feature_path; info } as t) ~to_ =
    match info with
    | Root _ ->
      raise_s [%sexp "Cr_soons.In_feature_tree.rename_non_root cannot rename root feature"
                   , (t.feature_path : Feature_path.t)
                   , (to_ : Feature_path.t)]
    | Non_root non_root ->
      { feature_path = to_
      ; info         = Non_root (Non_root.rename_feature non_root ~from:feature_path ~to_)
      }
  ;;
end

module In_feature_tree = struct

  type t =
    { root                 : Root.t option Var.t
    ; non_root_by_path     : Non_root.t Var.t Feature_path.Table.t
    ; non_roots            : Non_root.t Incr.t list Var.t
    ; active_by_assignee   : Cr_soon_multiset.t User_name.Map.t Observer.t
    ; inactive_by_assignee : Cr_soon_multiset.t User_name.Map.t Observer.t
    }
  [@@deriving fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      let root_name = ref None in
      let check_root_name feature_name =
        match !root_name with
        | None -> root_name := Some feature_name
        | Some root_name ->
          [%test_result: Feature_name.t] feature_name ~expect:root_name
      in
      Fields.iter
        ~root:(check (fun root ->
          Option.iter (Var.value root) ~f:(fun root ->
            Cr_soon_multiset.iter (Root.tip_cr_soons root)
              ~f:(fun cr_soon_in_feature ~num_occurrences:_ ->
                match
                  Feature_path.as_root
                    (Cr_soon_multiset.Cr_soon_in_feature.feature_path cr_soon_in_feature)
                with
                | Error e -> Error.raise e
                | Ok name -> check_root_name name))))
        ~non_root_by_path:(check (fun non_root_by_path ->
          Hashtbl.iteri non_root_by_path ~f:(fun ~key:feature_path ~data ->
            assert (not (Feature_path.is_root feature_path));
            check_root_name (Feature_path.root feature_path);
            let cr_soon_multiset = Non_root.active (Var.value data) in
            Cr_soon_multiset.iter cr_soon_multiset
              ~f:(fun cr_soon_in_feature ~num_occurrences:_ ->
                [%test_result: Feature_path.t]
                  (Cr_soon_multiset.Cr_soon_in_feature.feature_path cr_soon_in_feature)
                  ~expect:feature_path))))
        ~non_roots:ignore
        ~active_by_assignee:(check (fun obs -> ignore (Observer.value_exn obs : _ Map.t)))
        ~inactive_by_assignee:(check (fun obs -> ignore (Observer.value_exn obs : _ Map.t))))
  ;;

  let create alternate_names_var =
    let root = Var.create None in
    Incr.set_cutoff (Var.watch root)
      (Incr.Cutoff.create (fun ~old_value ~new_value ->
         Option.equal Root.equal old_value new_value));
    let non_root_by_path = Feature_path.Table.create () in
    let non_roots       = Var.create [] in
    let active =
      Incr.bind (Var.watch non_roots) ~f:(fun non_roots ->
        Incr.map (Incr.all non_roots) ~f:(fun non_roots ->
          Cr_soon_multiset.unions
            (List.map non_roots ~f:Non_root.active)))
    in
    let active_by_assignee =
      Incr.map2 active (Var.watch alternate_names_var)
        ~f:Cr_soon_multiset.partition_by_assignee
    in
    let inactive =
      Incr.map2 (Var.watch root) active ~f:(fun root active ->
        match root with
        | None -> Cr_soon_multiset.empty
        | Some root -> Cr_soon_multiset.diff root.Root.tip_cr_soons active)
    in
    let inactive_by_assignee =
      Incr.map2 inactive (Var.watch alternate_names_var)
        ~f:Cr_soon_multiset.partition_by_assignee
    in
    let t =
      { root
      ; non_root_by_path
      ; non_roots
      ; active_by_assignee   = Incr.observe active_by_assignee
      ; inactive_by_assignee = Incr.observe inactive_by_assignee
      }
    in
    Incr.stabilize ();
    t
  ;;

  let disallow_future_use t =
    Observer.disallow_future_use t.active_by_assignee;
    Observer.disallow_future_use t.inactive_by_assignee;
  ;;

  let get t ~for_or_all ~include_active =
    let find map =
      let map = Observer.value_exn map in
      match for_or_all with
      | `User user -> Option.value (Map.find map user) ~default:Cr_soon_multiset.empty
      | `All_users -> Cr_soon_multiset.unions (Map.data map)
    in
    let inactive = find t.inactive_by_assignee in
    if not include_active
    then inactive
    else (
      let active = find t.active_by_assignee in
      Cr_soon_multiset.union active inactive)
  ;;

  let is_empty t =
    Option.is_none (Var.value t.root)
    && Hashtbl.is_empty t.non_root_by_path
  ;;

  let set_non_roots t =
    Var.set t.non_roots
      (List.map (Hashtbl.data t.non_root_by_path) ~f:Var.watch)
  ;;

  let update_feature t (in_feature : In_feature.t) =
    (match in_feature.info with
     | Root root ->
       Var.set t.root (Some root)
     | Non_root non_root ->
       let key = in_feature.feature_path in
       match Hashtbl.find t.non_root_by_path key with
       | Some var -> Var.set var non_root
       | None ->
         let var = Var.create non_root in
         Incr.set_cutoff (Var.watch var)
           (Incr.Cutoff.create (fun ~old_value ~new_value ->
              Non_root.equal old_value new_value));
         Hashtbl.add_exn t.non_root_by_path ~key ~data:var;
         set_non_roots t);
    Incr.stabilize ();
  ;;

  let remove t feature_path =
    (if Feature_path.is_root feature_path
     then Var.set t.root None
     else if Hashtbl.mem t.non_root_by_path feature_path then (
       Hashtbl.remove t.non_root_by_path feature_path;
       set_non_roots t));
    Incr.stabilize ();
  ;;

  (* [rename_non_root_feature] is an optimized version of [remove] followed by
     [update_feature]. *)
  let rename_non_root_feature t ~from ~to_ =
    match Hashtbl.find t.non_root_by_path from with
    | None ->
      (* This can happen if the feature has been created but the CR soons have not yet
         been computed. *)
      ()
    | Some var ->
      Hashtbl.add_exn t.non_root_by_path ~key:to_ ~data:var;
      Hashtbl.remove  t.non_root_by_path      from;
      Var.set var (Non_root.rename_feature (Var.value var) ~from ~to_);
      Incr.stabilize ();
  ;;

  let iter_inactive_assignees t ~f =
    match Observer.value t.inactive_by_assignee with
    | Error _ -> ()
    | Ok map ->
      Map.iteri map ~f:(fun ~key:user_name ~data:multiset ->
        if not (Cr_soon_multiset.is_empty multiset)
        then f user_name)
  ;;
end

type t =
  { feature_tree_by_root : In_feature_tree.t Feature_name.Table.t
  ; alternate_names_var  : User_name_by_alternate_name.t Var.t
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~feature_tree_by_root:(check (fun feature_tree_by_root ->
        Hashtbl.iteri feature_tree_by_root ~f:(fun ~key:_ ~data ->
          In_feature_tree.invariant data)))
      ~alternate_names_var:(check (fun var ->
        User_name_by_alternate_name.invariant (Var.latest_value var))))
;;

let create () =
  let var = Var.create User_name_by_alternate_name.not_available in
  Incr.set_cutoff (Var.watch var)
    (Incr.Cutoff.of_compare User_name_by_alternate_name.compare);
  { feature_tree_by_root = Feature_name.Table.create ()
  ; alternate_names_var  = var
  }
;;

let get_feature_tree t feature_path =
  Hashtbl.find_or_add t.feature_tree_by_root (Feature_path.root feature_path)
    ~default:(fun () -> In_feature_tree.create t.alternate_names_var)
;;

let update_feature t (in_feature : In_feature.t) =
  In_feature_tree.update_feature
    (get_feature_tree t in_feature.feature_path)
    in_feature;
;;

let repartition t ~alternate_names =
  Var.set t.alternate_names_var alternate_names;
  Incr.stabilize ()
;;

let remove_feature t feature_path =
  let feature_tree = get_feature_tree t feature_path in
  In_feature_tree.remove feature_tree feature_path;
  if In_feature_tree.is_empty feature_tree
  then (
    In_feature_tree.disallow_future_use feature_tree;
    Hashtbl.remove t.feature_tree_by_root (Feature_path.root feature_path));
;;

let rename_non_root_feature t ~from ~to_ =
  ok_exn (Feature_path.check_renameable ~from ~to_);
  let feature_tree = get_feature_tree t from in
  In_feature_tree.rename_non_root_feature feature_tree ~from ~to_
;;

let iter_inactive_assignees t ~f =
  Hashtbl.iteri t.feature_tree_by_root ~f:(fun ~key:_ ~data:feature_tree ->
    In_feature_tree.iter_inactive_assignees feature_tree ~f)
;;

let get_all t ~for_or_all ~include_active =
  Cr_soon_multiset.unions
    (Hashtbl.fold t.feature_tree_by_root ~init:[]
       ~f:(fun ~key:_ ~data:in_feature_tree accum ->
         In_feature_tree.get in_feature_tree ~for_or_all ~include_active :: accum))
;;

let get_for_feature_tree t ~root_feature ~for_or_all ~include_active =
  match Hashtbl.find t.feature_tree_by_root root_feature with
  | None -> Cr_soon_multiset.empty
  | Some in_feature_tree ->
    In_feature_tree.get in_feature_tree ~for_or_all ~include_active
;;
