open! Core
open! Import

type t =
  { diff2s                  : Diff2s.t
  ; base                    : Rev_facts.t
  ; tip                     : Rev_facts.t
  ; base_is_ancestor_of_tip : Rev_facts.Is_ancestor.t
  }
[@@deriving fields, sexp_of]

module Problem = struct
  module Node = struct
    type t = [ `base | `tip ]
    [@@deriving compare, enumerate, sexp_of]
  end

  type t =
    | Is_not_ancestor_of        of Node.t * Node.t
    | Is_not_conflict_free      of Node.t
    | Is_not_cr_clean           of Node.t
    | Obligations_are_invalid   of Node.t
  [@@deriving compare, sexp_of]

  let or_error result problems =
    if List.is_empty problems
    then Ok result
    else Error (Error.create "invalid review goal" problems [%sexp_of: t list])
  ;;
end

let base_rev t = Rev_facts.rev t.base
let tip_rev t  = Rev_facts.rev t.tip

let base_facts t = t.base
let tip_facts t  = t.tip

let problems t =
  let get_node node =
    match node with
    | `base -> t.base
    | `tip  -> t.tip
  in
  let is_ok_true = function
    | Ok true -> true
    | Ok false | Error _ -> false
  in
  let is_conflict_free node =
    let rev_facts : Rev_facts.t = get_node node in
    let rev = Rev_facts.rev rev_facts in
    Rev_facts.Is_conflict_free.check rev_facts.is_conflict_free rev,
    Problem.Is_not_conflict_free node
  in
  let is_cr_clean node =
    let rev_facts : Rev_facts.t = get_node node in
    let rev = Rev_facts.rev rev_facts in
    Rev_facts.Is_cr_clean.check rev_facts.is_cr_clean rev,
    Problem.Is_not_cr_clean node;
  in
  let obligations_are_valid node =
    let rev_facts : Rev_facts.t = get_node node in
    let rev = Rev_facts.rev rev_facts in
    Rev_facts.Obligations_are_valid.check rev_facts.obligations_are_valid rev,
    Problem.Obligations_are_invalid node
  in
  let base_is_ancestor_of_tip =
    Rev_facts.Is_ancestor.check t.base_is_ancestor_of_tip
      ~ancestor:  (Rev_facts.rev t.base)
      ~descendant:(Rev_facts.rev t.tip)
    ,
    Problem.Is_not_ancestor_of (`base, `tip)
  in
  [ [ base_is_ancestor_of_tip ]
  ; List.map ~f:obligations_are_valid Problem.Node.all
  ; List.map ~f:is_conflict_free      Problem.Node.all
  ; List.map ~f:is_cr_clean           [ `base ]
  ]
  |> List.concat
  |> List.filter_map
       ~f:(fun (result, problem) -> if is_ok_true result then None else Some problem)
;;

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    [%test_result: Problem.t list] (problems t) ~expect:[];
    let check f = Invariant.check_field t f in
    Fields.iter
      ~diff2s:(check Diff2s.invariant)
      ~base:(check Rev_facts.invariant)
      ~tip:(check Rev_facts.invariant)
      ~base_is_ancestor_of_tip:ignore)
;;

let create diff2s ~base_facts ~tip_facts ~base_is_ancestor_of_tip =
  let t =
    { diff2s
    ; base                    = base_facts
    ; tip                     = tip_facts
    ; base_is_ancestor_of_tip
    }
  in
  Problem.or_error t (problems t)
;;

let review_edge t =
  { Review_edge.
    base = base_rev t
  ; tip  = tip_rev t
  }
;;

let restrict_to_may_review_or_follow t reviewer =
  { t with diff2s = Diff2s.restrict_to_may_review_or_follow t.diff2s reviewer }
;;

module By_diff2 = Diff2.Ignoring_rev.Table

let by_diff2 (t : t) ~f =
  Hashtbl.map
    (By_diff2.of_alist_multi
       (List.map t.diff2s ~f:(fun a -> (a, f a))))
    ~f:List.hd_exn
;;
