open! Core
open! Import

module Attributed_files_analysis = struct
  type t =
    { completed_review                                       : User_name.Set.t
    ; completed_review_satisfies_non_wfr_obligations_in_base : bool
    ; completed_review_satisfies_non_wfr_obligations_in_tip  : bool
    }
  [@@deriving compare, fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      Fields.iter
        ~completed_review:ignore
        ~completed_review_satisfies_non_wfr_obligations_in_base:ignore
        ~completed_review_satisfies_non_wfr_obligations_in_tip:ignore)
  ;;

  let attributed_files_obligations_are_satisfied t =
    let self f = Field.get f t in
    Fields.for_all
      ~completed_review:(const true)
      ~completed_review_satisfies_non_wfr_obligations_in_base:self
      ~completed_review_satisfies_non_wfr_obligations_in_tip:self
  ;;
end

module Diff2_analysis = struct
  type t =
    { actually_reviewed                    : Attributed_files_analysis.t
    ; assuming_expected_users_are_finished : Attributed_files_analysis.t
    }
  [@@deriving compare, fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~actually_reviewed:(check Attributed_files_analysis.invariant)
        ~assuming_expected_users_are_finished:(check Attributed_files_analysis.invariant))
  ;;
end

module Whole_feature_reviewer_analysis = struct
  type t =
    { obligations_are_satisfied : bool
    }
  [@@deriving compare, fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      Fields.iter
        ~obligations_are_satisfied:ignore)
  ;;
end

type t =
  { diff2s : Diff2_analysis.t Diff2.Ignoring_rev.Map.t
  ; users_with_review_session_in_progress : User_name.Set.t
  ; whole_feature_reviewers : Whole_feature_reviewer_analysis.t User_name.Map.t
  }
[@@deriving compare, fields, sexp_of]

let equal t1 t2 = compare t1 t2 = 0

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~diff2s:(check (Map.iteri ~f:(fun ~key:diff2 ~data ->
        Diff2.invariant diff2;
        Diff2_analysis.invariant data)))
      ~users_with_review_session_in_progress:(check (Set.iter ~f:User_name.invariant))
      ~whole_feature_reviewers:(check (Map.iteri ~f:(fun ~key:user_name ~data ->
        User_name.invariant user_name;
        Whole_feature_reviewer_analysis.invariant data))))
;;

let obligations_are_satisfied t =
  let check f field = f (Field.get field t) in
  Fields.for_all
    ~diff2s:(check (Map.for_all ~f:(fun (diff2 : Diff2_analysis.t) ->
      Attributed_files_analysis.attributed_files_obligations_are_satisfied
        diff2.actually_reviewed)))
    ~users_with_review_session_in_progress:(const true)
    ~whole_feature_reviewers:
      (check (Map.for_all ~f:Whole_feature_reviewer_analysis.obligations_are_satisfied))
;;

let non_wfr_obligations_will_be_satisfied_once_expected_users_have_read t diff2 =
  match Map.find t.diff2s diff2 with
  | None -> false
  | Some analysis ->
    Attributed_files_analysis.attributed_files_obligations_are_satisfied
      analysis.assuming_expected_users_are_finished
;;
