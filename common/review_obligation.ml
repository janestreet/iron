module Stable_format = struct

  module V1 = struct
    open! Core.Core_stable
    module Symbolic_user_set = Symbolic_user_set.Stable
    module User_name         = User_name.Stable

    type t =
      | All             of Symbolic_user_set.V1.t
      | At_least_wide   of int * Symbolic_user_set.V1.t
      | At_least_narrow of int * Symbolic_user_set.V1.t
      | And             of t list
      | Or_wide         of t list
      | Or_narrow       of t list
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 3cb07fe6ca3e08b4a5c839d953f7a289 |}]
    ;;
  end
end

open! Core
open! Import

module T = struct
  type t =
    | All_of        of User_name.Set.t
    | At_least_wide of int * User_name.Set.t
    | And           of t list
    | Or_wide       of t list
  [@@deriving compare, sexp_of]

  let module_name = "Iron_common.Review_obligation"

  let hash_users = User_name.Stable.V1.Set.hash
  let rec hash = function
    | All_of users -> Hash_consing.fold_hash 1 (hash_users users)
    | At_least_wide (k, users) -> Hash_consing.(fold_hash (fold_hash 2 k)
                                                  (hash_users users))
    | And ts     -> Hash_consing.fold_hash 3 (Hash_consing.list_hash hash ts)
    | Or_wide ts -> Hash_consing.fold_hash 4 (Hash_consing.list_hash hash ts)
  ;;
end

include T
module H = Hash_consing.Make (T) ()

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let rec aux t =
      match t with
      | All_of users -> Set.iter users ~f:User_name.invariant
      | At_least_wide (k, users) ->
        Set.iter users ~f:User_name.invariant;
        if k <= 0
        then raise_s [%sexp "At_least_wide with k <= 0", (t : t)];
        if k > Set.length users
        then raise_s [%sexp "unsatisfiable At_least_wide", (t : t)];
      | And ts -> List.iter ts ~f:aux
      | Or_wide ts ->
        if List.is_empty ts then failwith "empty Or_wide";
        List.iter ts ~f:aux
    in
    aux t)
;;

let equal t1 t2 = compare t1 t2 = 0

(* If [min_reviewers t = (k, users)], then the minimum number of people that can satisfy
   [t] is at least [k], and in particular some [k]-sized subset of [users] will do (but
   we don't model which one).

   [users] isn't particularly interesting on its own, but returning it allows us to be
   more precise in [num_reviewers_lower_bound]. E.g. if:

   {v
     t = And (At_least 1 [X; Y; Z]) (At_least 1 [A; B; C])
   v}

   then you want to be able to say: because [X;Y;Z] and [A;B;C] don't intersect, the
   num_reviewers_lower_bound is actually 2, not 1.

   If [min_reviewers t = k, users] then [users] always has at least [k] elements (assuming
   that [invariant t] succeeds).

   And:
   We need at_least people to satisfy the first branch, and at_least' to satisfy the
   second. But there might be an overlap between those two up to a size of the
   intersection of from and from'. Finally we max with at_least and at_least' as
   they are clearly lower bounds.

   This is the best we can do if we process the sets one-by one. Ideally we would
   do some kind of inclusion-exclusion calculation; see footnote [1]

   Or_wide:
   It is tempting to figure out which of [at_least] and [at_least'] is smaller and
   take either [from] or [from'] respectively. But that is incorrect, e.g.:

   t = And (All_of [X;Y]) (Or [All_of [X;Y]; All_of [A]])

   Then using the "tempting" algorithm we would see:

   min_reviewers (Or [All_of [X;Y]; All_of [A]]) = (1, [A])
   min_reviewers (All_of [X;Y])                  = (2, [X;Y])
   min_reviewers t                               = (3, [X;Y;A])

   But in fact 2 people are sufficient to satisfy t (X and Y). *)
let rec min_reviewers t =
  match t with
  | All_of users -> Set.length users, users
  | At_least_wide (k, users) -> k, users
  | And ts ->
    let combine (at_least, from) (at_least', from') =
      Int.max
        (Int.max at_least at_least')
        (at_least + at_least' - Set.(length (inter from from')))
    , Set.union from from'
    in
    List.fold ts ~init:(0, User_name.Set.empty)
      ~f:(fun acc t -> combine acc (min_reviewers t))
  | Or_wide ts ->
    let combine (at_least, from) (at_least', from') =
      Int.min at_least at_least'
    , Set.union from from'
    in
    (* [reduce_exn] is okay because [invariant t] guarantees [ts] is nonempty. *)
    List.reduce_exn ~f:combine (List.map ~f:min_reviewers ts)
;;

let num_reviewers_lower_bound t =
  let k, _users = min_reviewers t in
  k
;;

module type Reviewed_by = sig
  type review_obligation
  type t [@@deriving sexp_of]
  val synthesize : review_obligation -> t
end with type review_obligation := t

let to_string_hum (module Reviewed_by : Reviewed_by) t =
  t
  |> Reviewed_by.synthesize
  |> [%sexp_of: Reviewed_by.t]
  |> Sexp.to_string_hum
;;

let none = All_of User_name.Set.empty

let all_of users = All_of users

let at_least_wide e k users =
  if k > Set.length users then Error_context.raise_f e "unsatisfiable [At_least]" ();
  if k <= 0 then Error_context.raise_f e "[At_least] must get positive int" ();
  At_least_wide (k, users)
;;

let or_wide e = function
  | [] -> Error_context.raise_f e "[Or] must have at least one clause" ()
  | [ t ] -> t
  | (_ :: _ :: _) as ts -> Or_wide ts
;;

let and_ ts =
  let users = ref User_name.Set.empty in
  let conjuncts = ref [] in
  let rec loop t =
    match t with
    | All_of more_users -> users := Set.union !users more_users;
    | And ts -> List.iter ts ~f:loop;
    | At_least_wide _ | Or_wide _ -> conjuncts := t :: !conjuncts;
  in
  List.iter ts ~f:loop;
  let conjuncts =
    if Set.is_empty !users
    then !conjuncts
    else All_of !users :: !conjuncts
  in
  match conjuncts with
  | [] -> none
  | [ t ] -> t
  | _     -> And (List.sort conjuncts ~cmp:compare)
;;

let rec may_reviewers = function
  | All_of users | At_least_wide (_, users) -> users
  | And ts | Or_wide ts -> User_name.Set.union_list (List.map ts ~f:may_reviewers)
;;

let rec has_a_may_reviewer = function
  | All_of users | At_least_wide (_, users) -> not (Set.is_empty users)
  | And ts | Or_wide ts -> List.exists ts ~f:has_a_may_reviewer
;;

let may_review t user =
  let rec loop = function
    | All_of users -> Set.mem users user
    | At_least_wide (_, users) -> Set.mem users user
    | And ts | Or_wide ts -> List.exists ts ~f:loop
  in
  loop t
;;

let is_satisfied t ~by =
  let rec loop = function
    | All_of users -> Set.is_subset users ~of_:by
    | At_least_wide (k, users) -> Set.length (Set.inter users by) >= k
    | Or_wide ts -> List.exists  ts ~f:loop
    | And ts     -> List.for_all ts ~f:loop
  in
  loop t
;;

let de_alias t user_name_by_alias =
  let de_alias user_name =
    User_name_by_alternate_name.to_user_name user_name_by_alias
      (User_name.to_unresolved_name user_name)
  in
  let de_alias_users user_names = User_name.Set.map user_names ~f:de_alias in
  let rec loop = function
    | All_of users -> All_of (de_alias_users users)
    | At_least_wide (k, users) -> At_least_wide (k, de_alias_users users)
    | And ts     -> And     (List.map ts ~f:loop)
    | Or_wide ts -> Or_wide (List.map ts ~f:loop)
  in
  loop t
;;

let%test_module _ =
  (module struct

    module Generator = Quickcheck.Generator

    let users_generator ?(at_least = 0) () =
      let open Generator in
      let open Monad_infix in
      Int.gen_incl at_least 8
      >>= fun num_users ->
      List.gen' ~length:(`Between_inclusive (1, 5)) Char.gen_lowercase
      >>| fun name ->
      let name = String.of_char_list name in
      List.init num_users ~f:(fun i -> User_name.of_string (sprintf "%s%d" name i))
      |> User_name.Set.of_list
    ;;

    let t_generator =
      let open Generator in
      recursive (fun t ->
        size >>= fun size ->
        let all_of = map (users_generator ()) ~f:(fun users -> All_of users) in
        let at_least_wide =
          let open Monad_infix in
          users_generator () ~at_least:1
          >>= fun users ->
          Int.gen_incl 1 (Set.length users)
          >>| fun k ->
          At_least_wide (k, users)
        in
        let or_wide =
          if size = 0
          then None
          else Some (map ~f:(fun ts -> Or_wide ts) (List.gen' ~length:(`At_least 1) t))
        in
        let and_ =
          if size = 0
          then None
          else Some (map ~f:(fun ts -> And ts) (List.gen t))
        in
        filter_map
          (union (List.filter_opt [Some all_of; Some at_least_wide; or_wide; and_]))
          ~f:(fun t -> Option.try_with (fun () -> invariant t; t)))
    ;;

    let t_with_may_reviewers_generator =
      let open Generator in
      t_generator
      >>= fun t ->
      List.gen_permutations (Set.to_list (may_reviewers t))
      >>| fun may_reviewers ->
      t, may_reviewers
    ;;

    let%test_unit _ =
      let debug = false in
      Quickcheck.test
        t_with_may_reviewers_generator
        ~sexp_of:[%sexp_of: t * User_name.t list]
        ~f:(fun (t, may_reviewers) ->
          let num_reviewers_lower_bound = num_reviewers_lower_bound t in
          (* O(n) subsets is sufficient testing and a good compromise as opposed to
             exhaustively testing the [2^n] subsets of [may_reviewers]. *)
          let (_all_users, subsets) =
            List.fold may_reviewers
              ~init:(User_name.Set.empty, [User_name.Set.empty])
              ~f:(fun (users, subsets) user ->
                let users = Set.add users user in
                users, users :: subsets)
          in
          let increasing_subsets = List.rev subsets in
          if debug then
            Debug.eprints "subsets" increasing_subsets [%sexp_of: User_name.Set.t list];
          with_return (fun return ->
            List.iter increasing_subsets ~f:(fun users ->
              if is_satisfied t ~by:users
              then (
                if debug
                then (
                  let rec is_satisfied_tautology = function
                    | All_of users when Set.is_empty users -> true
                    | And ts -> List.for_all ts ~f:is_satisfied_tautology
                    | Or_wide ts -> List.exists ts
                                      ~f:is_satisfied_tautology
                    | _ -> false
                  in
                  if not (is_satisfied_tautology t)
                  then
                    Debug.eprints "is_satisfied" (t, users) [%sexp_of: t * User_name.Set.t]);
                [%test_pred: int] (fun num -> num <= Set.length users)
                  num_reviewers_lower_bound;
                (* All remaining subsets include that subset so they also satisfy [t] and
                   their length is strictly bigger, so the predicate checked by the test is
                   implied.  Just skip them then. *)
                return.return ()))))
    ;;

    let users strings =
      User_name.Set.of_list (List.map strings ~f:User_name.of_string)
    ;;

    let%test_unit _ =
      let t = At_least_wide (1, users ["foo"; "bar"; "baz"]) in
      [%test_result: int] (num_reviewers_lower_bound t) ~expect:1
    ;;

    let%test_unit _ =
      let t = All_of (users ["foo"; "bar"; "baz"]) in
      [%test_result: int] (num_reviewers_lower_bound t) ~expect:3
    ;;

    let%test_unit _ =
      let t =
        And [ At_least_wide (1, users [ "foo"; "bar"])
            ; At_least_wide (1, users [ "baz"; "quux"])
            ]
      in
      [%test_result: int] (num_reviewers_lower_bound t) ~expect:2
    ;;

    let%test_unit _ =
      let t =
        Or_wide [ All_of (users ["foo"; "bar"; "baz"])
                ; At_least_wide (1, users ["foo"; "bar"; "baz"])
                ]
      in
      [%test_result: int] (num_reviewers_lower_bound t) ~expect:1
    ;;

    let%test_unit _ =
      let t =
        And [ At_least_wide (1, users ["foo"; "bar"; "baz"])
            ; At_least_wide (1, users ["foo"; "bar"; "baz"])
            ]
      in
      [%test_result: int] (num_reviewers_lower_bound t) ~expect:1
    ;;

    let%test_unit _ =
      let t =
        And [ At_least_wide (1, users ["a"; "b"])
            ; At_least_wide (1, users ["b"; "c"])
            ; At_least_wide (1, users ["c"; "d"])
            ]
      in
      (* This is a case where it would be nice to return 2, but we cannot, because we only
         process the sets one-by-one, rather than doing the full inclusion-exclusion. *)
      [%test_result: int] (num_reviewers_lower_bound t) ~expect:1
    ;;

    let%test_unit _ =
      let t =
        And [ Or_wide [ At_least_wide (1, users ["a"])
                      ; At_least_wide (1, users ["b"; "c"])
                      ]
            ; At_least_wide (1, users ["b"; "c"])
            ]
      in
      [%test_result: int] (num_reviewers_lower_bound t) ~expect:1
    ;;

  end)
;;

module Stable = struct
  module V1 = struct
    let hash = hash
    include Make_stable.Of_stable_format.V1 (Stable_format.V1) (struct
        type nonrec t = t [@@deriving compare]

        module V1 = Stable_format.V1

        let rec to_stable_format : t -> V1.t = function
          | All_of users             -> All users
          | At_least_wide (k, users) -> At_least_wide (k, users)
          | And ts                   -> And     (List.map ts ~f:to_stable_format)
          | Or_wide ts               -> Or_wide (List.map ts ~f:to_stable_format)
        ;;

        let rec of_stable_format : V1.t -> t = fun v1 -> H.shared_t (aux v1)
        and aux : V1.t -> t = function
          | All users                -> All_of users
          | At_least_wide (k, users) -> At_least_wide (k, users)
          | And ts                   -> And     (List.map ts ~f:of_stable_format)
          | Or_wide  ts              -> Or_wide (List.map ts ~f:of_stable_format)
          | v1 -> raise_s [%sexp "Review_obligation.of_stable", (v1 : V1.t)]
        ;;
      end)

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 3cb07fe6ca3e08b4a5c839d953f7a289 |}]
    ;;
  end
end

(* [1]

   First note that we could alternatively have said: you can say we need at_least for one
   branch and at_least' for the other, but the overlap might be as large as [min(at_least,
   at_least', int)], where [int = Set.(length (inter from from'))]. This leads to the
   equivalent formula:

   {v
     at_least + at_least' - min(at_least, at_least', int)
   v}

   Proof of equivalence: let's call the formula in use in [min_reviewers] "formula A", and
   the one above this sentence "formula B". Assume wlog that at_least <= at_least'. If int
   <= at_least, then formula B is [at_least + at_least' - int]. With formula A, note that
   [at_least - int >= 0], so [at_least' + at_least - int >= at_least'], so the max
   evaluates to [at_least + at_least' - int], good.

   If int > at_least then formula B yields [at_least']. And [at_least - int < 0], so that
   [at_least + at_least' - int < at_least], and the max picks out [at_least']. Good. #

   Let's introduce some new notation. Let's use [at_least_i] and [from_i] to mean the two
   elements of the pair [List.nth_exn ts i]. Define two functions on a set of indexes:

   {v
     int(is) = Set.length(intersection of all the from_i for i in s)
     f(is) = min(int(is) :: [at_least_i for i in is])
   v}

   f(is) is the maximum number of people that can read and contribute towards the
   obligations for each branch i for i in is. Note in particular that overlap([i]) =
   at_least_i. (Since [at_least_i <= Set.length from_i].)

   Then another way of deriving formula B is: the worst case to satisfy this obligation is
   that as many people common to both from_1 and from_2 read, and then some more people
   in the symmetric difference read in order to satisfy both branches. That is:

   {v

     f(1,2)             -- i.e. the common people
     + (f(1) - f(1,2))  -- "topping up" branch 1
     + (f(2) - f(1,2))  -- "topping up" branch 2
   = f(1) + f(2) - f(1,2)

   v}

   This then readily extends to three sets:

   {v

     f(1,2,3)                 -- i.e. the people common to all three sets
   + (f(1,2) - f(1,2,3))      -- i.e. the people common to 1,2 but not 3
   + (f(2,3) - f(1,2,3))
   + (f(3,1) - f(1,2,3))
   + (f(1)
        - (f(1,2) - f(1,2,3))
        - (f(3,1) - f(1,2,3))
        - f(1,2,3))            -- "topping up" branch 1
   + (f(2)
        - (f(1,2) - f(1,2,3))
        - (f(2,3) - f(1,2,3))
        - f(1,2,3))            -- "topping up" branch 2
   + (f(3)
        - (f(3,1) - f(1,2,3))
        - (f(2,3) - f(1,2,3))
        - f(1,2,3))            -- "topping up" branch 3

   = f(1) + f(2) + f(2) - f(1,2) - f(2,3) - f(3,1) + f(1,2,3)

   v}

   And taking this further leads to the regular inclusion-exclusion formulae:
   http://en.wikipedia.org/wiki/Inclusion%E2%80%93exclusion_principle
*)
