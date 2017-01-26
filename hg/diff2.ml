module Stable = struct
  open! Core.Core_stable
  module Attributed_file = Attributed_file.Stable
  module V2 = struct
    type t =
      { base              : Attributed_file.V2.t
      ; tip               : Attributed_file.V2.t
      ; num_lines_in_diff : int
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| c081b09564f5c889887b1620dcf3addd |}]
    ;;
  end
end

open Core
open! Import

include Stable.V2
include Structurally_comparable.Make(Stable.V2)

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    (match t.base.attributes, t.tip.attributes with
     | `Present _, `Present _ -> ()
     | `Absent, _ | _, `Absent ->
       [%test_result: Path_in_repo.t] t.tip.path_in_repo ~expect:t.base.path_in_repo);
    Fields.iter
      ~base:(check Attributed_file.invariant)
      ~tip:(check Attributed_file.invariant)
      ~num_lines_in_diff:(check ([%test_pred: int] (fun n -> n >= 0))))
;;

let create ~base ~tip ~num_lines_in_diff =
  let base, tip = Attributed_file.normalize_paths ~base ~tip in
  let t = { base; tip; num_lines_in_diff } in
  invariant t;
  t
;;

let involved_in_ownership_change t =
  match t.base.attributes, t.tip.attributes with
  | `Absent, `Absent -> User_name.Set.empty
  | `Present attributes, `Absent | `Absent, `Present attributes ->
    User_name.Set.singleton attributes.owner
  | `Present attributes1, `Present attributes2 ->
    if User_name.equal attributes1.owner attributes2.owner
    then User_name.Set.empty
    else User_name.Set.of_list [ attributes1.owner; attributes2.owner ]
;;

let may_review t ~include_may_follow reviewer =
  let may_review file reviewer =
    match Attributed_file.attributes file with
    | `Absent -> false
    | `Present attributes ->
      Attributed_file.Attributes.may_review attributes ~include_may_follow reviewer
  in
  may_review t.base reviewer
  || may_review t.tip reviewer
  || (
    let involved_in_ownership_change = involved_in_ownership_change t in
    (* If someone is going to review the ownership change, then a whole-feature
       reviewer/follower should too. *)
    if reviewer.is_whole_feature_reviewer
    then not (Set.is_empty involved_in_ownership_change)
    else Set.mem involved_in_ownership_change reviewer.user_name)
;;

let may_reviewers t ~include_file_followers =
  let may_reviewers file =
    match Attributed_file.attributes file with
    | `Absent -> User_name.Set.empty
    | `Present attributes ->
      Attributed_file.Attributes.may_reviewers attributes ~include_file_followers
  in
  User_name.Set.union_list
    [ may_reviewers t.base
    ; may_reviewers t.tip
    ; involved_in_ownership_change t
    ]
;;

let review_edge t = { Review_edge. base = t.base.rev; tip = t.tip.rev }

let with_num_lines t num_lines_in_diff = { t with num_lines_in_diff }

let path_in_repo_at_tip t = Attributed_file.path_in_repo t.tip

module Ignoring_rev = struct
  module T = struct
    type nonrec t = t [@@deriving sexp]

    let hash { base; tip; num_lines_in_diff = _ } =
      Attributed_file.Ignoring_rev.hash base
      lxor Attributed_file.Ignoring_rev.hash tip
    ;;

    let compare t { base; tip; num_lines_in_diff = _ } =
      let c = Attributed_file.Ignoring_rev.compare t.base base in
      if c <> 0
      then c
      else Attributed_file.Ignoring_rev.compare t.tip tip
    ;;
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

let should_automatically_forget t =
  Attributed_file.Ignoring_rev.equal t.base t.tip
;;

let de_alias t user_name_by_alias =
  let { base
      ; tip
      ; num_lines_in_diff
      } = t in
  { base              = Attributed_file.de_alias base user_name_by_alias
  ; tip               = Attributed_file.de_alias tip  user_name_by_alias
  ; num_lines_in_diff
  }
;;
