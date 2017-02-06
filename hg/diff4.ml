module Stable = struct
  open Core.Core_stable
  open Import_stable

  module V2 = struct
    type t =
      { diamond           : Attributed_file.Stable.V2.t Diamond.V1.t
      ; errors            : Error.V1.t list [@default []]
      ; num_lines_in_diff : int
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 3170dba6397eda7bb2b6635775a9c099 |}]
    ;;

    let hash t =
      Attributed_file.Ignoring_rev.hash t.diamond.b1
      lxor Attributed_file.Ignoring_rev.hash t.diamond.b2
      lxor Attributed_file.Ignoring_rev.hash t.diamond.f1
      lxor Attributed_file.Ignoring_rev.hash t.diamond.f2
    ;;
  end

  module And_output_num_lines = struct
    module V1 = struct
      type t =
        { diff4            : V2.t
        ; output_num_lines : int
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 30a4bf245013206f8aea714c5f2b65c7 |}]
      ;;
    end
  end
end

open! Core
open! Async
open! Import

let verbose = Verbose.knowledge

module T = Stable.V2

include T
include Hashable.Make (T)

(* let limit_memory_use =
 *   lazy (In_thread.run (fun () ->
 *     let open Core in
 *     let module R = Unix.RLimit in
 *     match R.virtual_memory with
 *     | Error _ -> ()
 *     | Ok virtual_mem ->
 *       let min v1 v2 =
 *         match v1, v2 with
 *         | R.Infinity, x -> x
 *         | x, R.Infinity -> x
 *         | R.Limit x, R.Limit y -> R.Limit (Int64.min x y)
 *       in
 *       let limit = R.get virtual_mem in
 *       let my_limit = R.Limit 2_000_000_000L in (\* 2GB in bytes *\)
 *       let limit : R.t =
 *         { cur = min limit.cur my_limit
 *         ; max = min limit.max my_limit }
 *       in
 *       R.set virtual_mem limit))
 * ;; *)

module Cache = struct
  module Key = struct
    module T = struct
      type t = File_contents_hash.t option Diamond.t sexp_opaque
      [@@deriving compare, sexp]
      let hash = Hashtbl.hash
    end
    include Hashable.Make(T)
  end
  type t = int Deferred.t Key.Table.t [@@deriving sexp_of]
  let create : unit -> t = Key.Table.create
end

let create_from_scratch_to_diff2 { Diff2. base; tip; num_lines_in_diff } =
  { diamond           = { b1 = base; f1 = base; b2 = base; f2 = tip }
  ; errors            = []
  ; num_lines_in_diff
  }
;;

let create_forget { Diff2. base; tip; num_lines_in_diff } =
  { diamond           = { b1 = base; f1 = tip; b2 = tip; f2 = tip }
  ; errors            = []
  ; num_lines_in_diff
  }
;;

let create_forget diff2 =
  let result = create_forget diff2 in
  if verbose
  then Debug.ams [%here] "create_forget" (diff2, result) [%sexp_of: Diff2.t * t];
  result
;;

let is_forget { diamond; _ } =
  Diamond.is_forget diamond ~equal:(fun f1 f2 ->
    Rev.equal_node_hash (Attributed_file.rev f1) (Attributed_file.rev f2))
;;

let as_from_scratch_to_diff2 { diamond; errors = _; num_lines_in_diff } =
  let { Diamond.b1 ; b2 ; f1 ; f2 = _ } = Diamond.map diamond ~f:Attributed_file.rev in
  if Rev.equal_node_hash b1 f1
  && Rev.equal_node_hash b1 b2
  then Some (Diff2.create ~base:diamond.b2 ~tip:diamond.f2 ~num_lines_in_diff)
  else None
;;

let diamond_is_rev_update { Diamond. b1; b2; f1; f2 } =
  Attributed_file.Ignoring_rev.equal b1 b2
  && Attributed_file.Ignoring_rev.equal f1 f2
;;

let is_rev_update t = t.num_lines_in_diff = 0 && diamond_is_rev_update t.diamond

let input t ~num_lines_in_diff =
  Diff2.create ~base:t.diamond.b1 ~tip:t.diamond.f1 ~num_lines_in_diff
;;

let output t ~num_lines_in_diff =
  Diff2.create ~base:t.diamond.b2 ~tip:t.diamond.f2 ~num_lines_in_diff
;;

let create_rev_update ~(from_ : Diff2.t) ~(to_ : Diff2.t) =
  let b1 = from_.base in
  let f1 = from_.tip  in
  let b2 = to_.base   in
  let f2 = to_.tip    in
  let diamond = { Diamond. b1; b2; f1; f2 } in
  if not (diamond_is_rev_update diamond)
  then error "Diff4.create_rev_update" (from_, to_) [%sexp_of: Diff2.t * Diff2.t]
  else Ok { diamond
          ; errors            = []
          ; num_lines_in_diff = 0
          }
;;

let normalize_paths ({ b1; f1; b2; f2 } : 'a Diamond.t) : 'a Diamond.t =
  let b1, f1 = Attributed_file.normalize_paths ~base:b1 ~tip:f1 in
  let b2, f2 = Attributed_file.normalize_paths ~base:b2 ~tip:f2 in
  match b2.attributes, f2.attributes with
  | `Absent, `Absent ->
    let with_b1 f = Attributed_file.with_path_in_repo f b1.path_in_repo in
    { b1; f1; b2 = with_b1 b2; f2 = with_b1 f2 }
  | `Present _, _ | _, `Present _ ->
    match b1.attributes, f1.attributes with
    | `Present _, _ | _, `Present _ -> { b1; f1; b2; f2 }
    | `Absent, `Absent ->
      let with_b2 f = Attributed_file.with_path_in_repo f b2.path_in_repo in
      { b1 = with_b2 b1; f1 = with_b2 f1; b2; f2 }
;;

let create ~file_by_path_by_rev ~cache ~errors ~lines_required_to_separate_ddiff_hunks diamond =
  let diamond = normalize_paths diamond in
  (* [hg status] is an overapproximation of the files that did change, so we filter out
     the files that didn't change.  However we must be careful to not remove diff4s that
     the server needs. *)
  match Diamond.classify diamond ~equal:Attributed_file.Ignoring_rev.equal with
  (* In these two cases, the reviewer shouldn't know b1->f1 (since b1=f1) and there is no
     edge in the goal (b2=f2), so the server doesn't need the diff4. *)
  | `b1_b2_f1_f2
  | `b1_f1__b2_f2
  (* In this case, the server can realize the diff2 in the goal is already in the
     knowledge, so it doesn't need the diff4. *)
  | `b1_b2__f1_f2
    when List.is_empty errors -> return `Equal
  (* In that case though, if we don't return a diamond, the server will understand it as
     meaning that the change in b1->f1 was dropped. This is not completely wrong, but it
     better to produce a diff4, because the diff4 doesn't have to be shown by [fe review].
     | `b2_f1_f2 *)
  | _ ->
    let%bind num_lines_in_diff =
      (* This optimization cannot be done by patdiff4 because we have the digests and it
         doesn't. *)
      match Diamond.classify diamond ~equal:Attributed_file.equal_file_contents with
      | `b1_b2_f1_f2
      | `b1_f1__b2_f2
      | `b1_b2__f1_f2
      | `b2_f1_f2 -> return 0
      | _ ->
        (* This caching avoids duplicate calls, but also avoid calls where the files
           being diffed are the same (same digest) but have different names (because
           they come from different revision). *)
        let key = Diamond.map diamond ~f:Attributed_file.file_digest in
        match Hashtbl.find cache key with
        | Some i -> i
        | None ->
          (* As it turns out, patdiff and patdiff4 don't tell the difference between a
             non existing file and an empty file.
             $ patdiff /dev/null <(:)
             $ patdiff4 manual -old-base /dev/null -new-base /dev/null -old-tip /dev/null -new-tip <(:)
             So in effect, the client has to be smarter and consider that even when there
             are no lines in the diff and no attribute changes, there may still be
             something to review. *)
          let file_file_exn af =
            let file_by_path = Map.find_exn file_by_path_by_rev af.Attributed_file.rev in
            match af.attributes with
            | `Absent ->
              assert (not (Map.mem file_by_path af.path_in_repo));
              "/dev/null"
            | `Present _ -> Abspath.to_string (Map.find_exn file_by_path af.path_in_repo)
          in
          let files = Diamond.map diamond ~f:file_file_exn in
          let def =
            let%map { num_lines_in_diff4 } =
              Process_num_lines_in_diff4.compute
                { files
                ; lines_required_to_separate_ddiff_hunks
                }
            in
            num_lines_in_diff4
          in
          Hashtbl.add_exn cache ~key ~data:def;
          def
    in
    return (`Unequal { diamond; errors; num_lines_in_diff })
;;

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~diamond:(check (Diamond.invariant Attributed_file.invariant))
      ~errors:ignore
      ~num_lines_in_diff:(check
                            ([%test_pred: int]
                               (fun num_lines_in_diff -> num_lines_in_diff >= 0)));
    if Diamond.for_all t.diamond ~f:(fun file -> not (Attributed_file.is_present file))
    && not (is_forget t)
    then failwith "at least one node of the diamond has to be present")
;;

module And_output_num_lines = struct
  include Stable.And_output_num_lines.V1

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~diff4:(check invariant)
        ~output_num_lines:(check (fun n -> assert (n >= 0))))
  ;;

  let output { diff4; output_num_lines } =
    output diff4 ~num_lines_in_diff:output_num_lines
  ;;
end

let present_paths_in_repo t =
  Attributed_file.present_paths_in_repo (Diamond.to_list t.diamond)
;;

let path_in_repo_at_f2 t = t.diamond.f2.path_in_repo

let should_review_file_followers_change t (user_name : User_name.t) =
  Diff4_class.is_shown
    (Diamond.classify t.diamond
       ~equal:(fun (attributed_file1 : Attributed_file.t) attributed_file2 ->
         match attributed_file1.attributes, attributed_file2.attributes with
         | `Absent, `Absent -> true
         | `Present attributes1, `Present attributes2 ->
           Bool.equal
             (Set.mem attributes1.followers user_name)
             (Set.mem attributes2.followers user_name)
         | (`Absent | `Present _), _ -> false))
  &&
  (Diamond.exists t.diamond ~f:(fun attributed_file ->
     match attributed_file.attributes with
     | `Absent -> false
     | `Present attributes -> Set.mem attributes.followers user_name))
;;

let may_follow t (reviewer : Reviewer.t) =
  match t.diamond.f2.attributes with
  | `Absent ->
    if reviewer.is_whole_feature_follower
    || should_review_file_followers_change t reviewer.user_name
    then `Follow_lines
    else `Nothing_to_follow
  | `Present attributes ->
    if Attributed_file.Attributes.may_follow attributes reviewer
    then `Follow_lines
    else if should_review_file_followers_change t reviewer.user_name
    then `Dropped_from_follow
    else `Nothing_to_follow
;;

let should_review_ownership_change t (reviewer : Reviewer.t) =
  Diff4_class.is_shown
    (Diamond.classify t.diamond
       ~equal:(fun (attributed_file1 : Attributed_file.t) attributed_file2 ->
         match attributed_file1.attributes, attributed_file2.attributes with
         | `Absent, `Absent -> true
         | `Present attributes1, `Present attributes2 ->
           User_name.equal attributes1.owner attributes2.owner
         | (`Absent | `Present _), _ -> false))
  &&
  (reviewer.is_whole_feature_reviewer
   || Diamond.exists t.diamond ~f:(fun attributed_file ->
     match attributed_file.attributes with
     | `Absent -> false
     | `Present attributes -> User_name.equal reviewer.user_name attributes.owner))
;;

let may_review_internal t reviewer =
  let is_present_with_value bool = function
    | `Absent -> false
    | `Present value -> Bool.equal bool value
  in
  let may_review =
    Diamond.map t.diamond ~f:(fun file ->
      match Attributed_file.attributes file with
      | `Absent -> `Absent
      | `Present attributes ->
        `Present (Attributed_file.Attributes.may_review attributes
                    ~include_may_follow:false reviewer))
  in
  if not (Diamond.exists may_review ~f:(is_present_with_value true))
  then
    if should_review_ownership_change t reviewer
    then `Review_ownership_change
    else `Nothing_to_review
  else if is_present_with_value false may_review.f2
  then
    (* If we're here, then:
       {[
         List.exists Diamond.([ b1 ; f1 ; b2 ]) ~f:(fun field ->
           is_present_with_value true (field may_review))
       ]}
    *)
    if not (is_present_with_value true may_review.f1)
    then `Nothing_to_review
    else `Dropped_from_review
  else `Review_lines
;;

let may_review t reviewer =
  match may_review_internal t reviewer with
  | `Dropped_from_review
  | `Review_ownership_change
  | `Review_lines as review -> review
  | `Nothing_to_review ->
    match may_follow t reviewer with
    | `Dropped_from_follow | `Follow_lines as result -> result
    | `Nothing_to_follow -> `Nothing_to_review_or_follow
;;

let num_lines ({ diamond ; errors ; num_lines_in_diff } as t) reviewer =
  if is_rev_update t
  then 0
  (* We test for forget diffs specially before looking at the obliged stuff below to avoid
     a bug in which a forget diff incorrectly has [num_lines = 0] when the reviewer
     is not obliged at any of the four points on the diamond. *)
  else if is_forget t
  then 1 + num_lines_in_diff
  else (
    match may_review t reviewer with
    | `Nothing_to_review_or_follow -> 0 (* no review *)
    | `Dropped_from_review | `Dropped_from_follow ->
      (* Not reviewed anymore; just count one line for reviewing the message saying you're
         dropped from this file. *)
      1
    | `Review_ownership_change
    | `Follow_lines
    | `Review_lines ->
      let raw_num_lines_in_diff =
        let src = Diamond.f1 diamond in
        let dst = Diamond.f2 diamond in
        if File_scrutiny.(>=)
             (Attributed_file.scrutiny src)
             (Attributed_file.scrutiny dst)
        then
          (* Regardless of whether [user] has read the src, he should only read the diff4
             to the [dst]. *)
          num_lines_in_diff
        else
          (* Review the whole file, and review the diff unless it was previously not
             reviewed. *)
          (Attributed_file.num_lines dst)
          + (if File_scrutiny.(=)
                  (Attributed_file.scrutiny src)
                  File_scrutiny.ignored
             then 0
             else num_lines_in_diff)
      in
      let review_client_wont_show_more_stuff =
        let equal_attributes_for_review attributes1 attributes2 =
          let check equal field =
            equal (Field.get field attributes1) (Field.get field attributes2)
          in
          let ignore _ = true in
          let followers f1 f2 =
            Bool.equal
              (Set.mem f1 reviewer.user_name)
              (Set.mem f2 reviewer.user_name)
          in
          Attributed_file.Attributes.Fields.for_all
            ~hash:ignore
            ~owner:(check User_name.equal)
            ~review_obligation:(check Review_obligation.equal)
            ~followers:(check followers)
            ~scrutiny:(check File_scrutiny.equal)
            ~is_read_by_whole_feature_reviewers:ignore
            ~num_lines:ignore
        in
        let equal_attributed_file_for_review
              (at1 : Attributed_file.t) (at2 : Attributed_file.t) =
          match at1.attributes, at2.attributes with
          | `Absent, `Absent -> true
          | `Present _, `Absent
          | `Absent, `Present _ -> false
          | `Present attributes1, `Present attributes2 ->
            Path_in_repo.equal at1.path_in_repo at2.path_in_repo
            && equal_attributes_for_review attributes1 attributes2
        in
        List.is_empty errors
        && not (Diff4_class.is_shown
                  (Diamond.classify diamond ~equal:equal_attributed_file_for_review))
      in
      let conservative_approach_waiting_for_proper_fix =
        if review_client_wont_show_more_stuff
        then 0
        else 1
      in
      conservative_approach_waiting_for_proper_fix + raw_num_lines_in_diff)
;;

let is_implicitly_reviewed t reviewer = num_lines t reviewer = 0

let summary ts ~sort =
  let ts =
    if not sort
    then ts
    else
      List.sort ts ~cmp:(fun d1 d2 ->
        Path_in_repo.default_review_compare
          (path_in_repo_at_f2 d1)
          (path_in_repo_at_f2 d2))
  in
  let op t =
    match Attributed_file.status ~src:t.diamond.b2 ~dst:t.diamond.f2 with
    | `Added _    -> "add"
    | `Removed _  -> "rem"
    | `Modified _ -> "mod"
  in
  let columns =
    Ascii_table.Column.(
      [ string ~header:"file"
          (cell (fun t -> Path_in_repo.to_string (path_in_repo_at_f2 t)))
      ; string ~header:"op" (cell op)
      ; num_lines (cell num_lines_in_diff)
      ])
  in
  Ascii_table.create ~columns ~rows:ts
;;
