module Stable = struct

  open Core.Core_stable

  module Rev = Rev.Stable

  module Is_ancestor = struct
    module V1 = struct
      type t =
        { ancestor   : Rev.V1.t
        ; descendant : Rev.V1.t
        ; status     : bool
        }
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 508e7a52ccbf756a27d9b057250f8b36 |}]
      ;;
    end
  end

  module Is_conflict_free = struct
    module V1 = struct
      type t =
        { rev    : Rev.V1.t
        ; status : bool
        }
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 28833e78f1d51ae94504cc1cfd4e8511 |}]
      ;;
    end
  end

  module Is_cr_clean = struct
    module V1 = struct
      type t =
        { rev    : Rev.V1.t
        ; status : bool
        }
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 28833e78f1d51ae94504cc1cfd4e8511 |}]
      ;;
    end
  end

  module Obligations_are_valid = struct
    module V1 = struct
      type t =
        { rev    : Rev.V1.t
        ; status : bool
        }
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 28833e78f1d51ae94504cc1cfd4e8511 |}]
      ;;
    end
  end

  module V1 = struct
    type t =
      { is_conflict_free      : Is_conflict_free.V1.t
      ; is_cr_clean           : Is_cr_clean.V1.t
      ; obligations_are_valid : Obligations_are_valid.V1.t
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| bcbda9e61ea119f9aac8830236338103 |}]
    ;;
  end
end

open Core
open Async
open! Import

let assert_working_copy_of ?repo_is_clean repo_root ~is:rev =
  let%bind parent = Hg.parent repo_root in
  if not (Rev.equal_node_hash parent rev)
  then raise_s [%sexp "unexpected working directory parent", (parent : Rev.t)];
  let%map result = Hg.status_cleanliness ?repo_is_clean repo_root in
  ok_exn result
;;

module Is_ancestor = struct

  include Stable.Is_ancestor.V1

  let create repo_root ~ancestor ~descendant =
    let%map status = Hg.is_ancestor repo_root ~ancestor ~descendant in
    { ancestor; descendant; status }
  ;;

  let reflexive rev =
    { ancestor = rev; descendant = rev; status = true }
  ;;

  let transitive t1 t2 =
    if Rev.equal_node_hash t1.descendant t2.ancestor
    then Ok { ancestor   = t1.ancestor
            ; descendant = t2.descendant
            ; status     = t1.status && t2.status
            }
    else error "Rev_fact.transitive" (t1, t2) [%sexp_of: t * t]
  ;;

  let check t ~ancestor ~descendant =
    if Rev.equal_node_hash t.ancestor ancestor
    && Rev.equal_node_hash t.descendant descendant
    then Ok t.status
    else error "mismatched revs" (t, `ancestor ancestor, `descendant descendant)
           [%sexp_of: t * [ `ancestor of Rev.t ] * [ `descendant of Rev.t ]]
  ;;
end

module Is_conflict_free = struct

  include Stable.Is_conflict_free.V1

  let create ?repo_is_clean repo_root rev =
    let%bind Repo_is_clean =
      assert_working_copy_of ?repo_is_clean repo_root ~is:rev
    in
    let%map is_conflict_free = Hg.is_conflict_free_exn repo_root in
    { rev; status = is_conflict_free }
  ;;

  let check t rev =
    if Rev.equal_node_hash t.rev rev
    then Ok t.status
    else error "Is_conflict_free.check mismatched rev" (t, rev) [%sexp_of: t * Rev.t]
  ;;

  let with_rev_exn t rev =
    ignore (check t rev |> ok_exn : bool);
    { t with rev }
  ;;
end

module Is_cr_clean = struct

  include Stable.Is_cr_clean.V1

  let create ?repo_is_clean repo_root format ~incremental_based_on rev ~file_owner =
    match format with
    | Error _ as e -> return ({ rev; status = false }, e)
    | Ok format ->
      let%bind Repo_is_clean =
        assert_working_copy_of ?repo_is_clean repo_root ~is:rev
      in
      let%map ({ due_now; due_soon = _ } as grepped_crs) =
        Cr_comment.maybe_incremental_grep repo_root format
          ~incremental_based_on
          ~file_owner
      in
      let status = List.is_empty due_now in
      { rev; status }, Ok grepped_crs
  ;;

  let check t rev =
    if Rev.equal_node_hash t.rev rev
    then Ok t.status
    else error "Is_cr_clean.check mismatched rev" (t, rev) [%sexp_of: t * Rev.t]
  ;;

  let with_rev_exn t rev =
    ignore (check t rev |> ok_exn : bool);
    { t with rev }
  ;;
end

module Obligations_are_valid = struct

  include Stable.Obligations_are_valid.V1

  let create_exn ?fake_for_testing ?repo_is_clean repo_root rev ~aliases =
    let%map (oblig_result, version_result) =
      match fake_for_testing with
      | Some attribute ->
        if not am_functional_testing
        then Rpc_to_server_prevention.disable_rpc_to_server Use_of_fake_obligations;
        let%map manifest = Hg.manifest repo_root (`Revset (Hg.Revset.of_rev rev)) in
        Ok (Obligations.fake ~manifest attribute), Ok Obligations_version.latest
      | None ->
        let%bind Repo_is_clean =
          assert_working_copy_of ?repo_is_clean repo_root ~is:rev
        in
        let%bind manifest = Hg.manifest repo_root (`Revset Hg.Revset.dot) in
        Obligations.create (module Hg) ~repo_root ~dirs:`All ~manifest ~aliases ()
    in
    let tag result = Or_error.tag result ~tag:(sprintf !"At rev %{Rev#12}" rev) in
    { rev; status = Result.is_ok oblig_result }, tag oblig_result, tag version_result
  ;;

  let check t rev =
    if Rev.equal_node_hash t.rev rev
    then Ok t.status
    else error "Obligations_are_valid.check mismatched rev" (t, rev)
           [%sexp_of: t * Rev.t]
  ;;

  let with_rev_exn t rev =
    ignore (check t rev |> ok_exn : bool);
    { t with rev }
  ;;
end

include Stable.V1

let rev t = t.is_conflict_free.rev

let revs_agree t =
  Rev.equal_node_hash    t.is_conflict_free.rev t.is_cr_clean.rev
  && Rev.equal_node_hash                        t.is_cr_clean.rev t.obligations_are_valid.rev
;;

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let _check f = Invariant.check_field t f in
    assert (revs_agree t);
    Fields.iter
      ~is_conflict_free:ignore
      ~is_cr_clean:ignore
      ~obligations_are_valid:ignore)
;;

let create is_conflict_free is_cr_clean obligations_are_valid =
  let t = { is_conflict_free; is_cr_clean; obligations_are_valid } in
  if revs_agree t
  then Ok t
  else error "Triple.create got mismatched revs"
         (is_conflict_free, is_cr_clean, obligations_are_valid)
         [%sexp_of: Is_conflict_free.t * Is_cr_clean.t * Obligations_are_valid.t]
;;

let check_gen
      ?(allow_non_cr_clean = false)
      ?(error_msg_if_not_cr_clean = "revision is not CR clean")
      t
      rev
      ~if_check_fails
  =
  let check msg f field =
    Or_error.bind (f (Field.get field t) rev) ~f:(function
      | true -> Ok true
      | false ->
        match if_check_fails with
        | `False -> Ok false
        | `Error -> Or_error.error_string msg)
  in
  match
    Or_error.combine_errors
      (Fields.to_list
         ~is_conflict_free:
           (check "revision has files with conflict markers" Is_conflict_free.check)
         ~is_cr_clean:(fun is_cr_clean ->
           if allow_non_cr_clean
           then Ok true
           else check error_msg_if_not_cr_clean Is_cr_clean.check is_cr_clean)
         ~obligations_are_valid:
           (check "obligations are invalid" Obligations_are_valid.check))
  with
  | Error _ as e -> e
  | Ok bools -> Ok (List.for_all ~f:Fn.id bools)
;;

let check ?allow_non_cr_clean t rev =
  check_gen t rev ?allow_non_cr_clean ~if_check_fails:`False
;;

let check_true ?allow_non_cr_clean ?error_msg_if_not_cr_clean t rev =
  match check_gen t rev ?allow_non_cr_clean ?error_msg_if_not_cr_clean
          ~if_check_fails:`Error
  with
  | Ok true -> Ok ()
  | Ok false -> assert false
  | Error _ as e -> e
;;

let with_rev_exn t rev =
  { is_conflict_free      = Is_conflict_free.with_rev_exn t.is_conflict_free rev
  ; is_cr_clean           = Is_cr_clean.with_rev_exn t.is_cr_clean rev
  ; obligations_are_valid = Obligations_are_valid.with_rev_exn t.obligations_are_valid rev
  }
;;
