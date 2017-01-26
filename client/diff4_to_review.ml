open! Core
open! Import

module Kind = struct
  type t =
    | Catch_up of Catch_up_kind.t
    | Review   of Review_kind.t
  [@@deriving compare, sexp_of]

  let equal t1 t2 = compare t1 t2 = 0

  (* This drives the order in which different section of diff4s are grouped by when
     showing a review or catch-up session.  As of now review and catch-up session are not
     mixed. *)
  let weight = function
    | Review Must_review                    -> 0
    | Review Ownership_change               -> 1
    | Review Follow                         -> 2
    | Review May_review                     -> 3
    | Catch_up (Reviewed_by_someone_else _) -> 4
    | Catch_up Unfinished_review            -> 5
    | Catch_up Follower                     -> 6
    | Catch_up Create_catch_up_for_me       -> 7
  ;;

  let compare_to_sort_by_decreasing_priority t1 t2 =
    let cmp = Int.compare (weight t1) (weight t2) in
    if cmp <> 0 then cmp else compare t1 t2
  ;;

  let to_string_hum = function
    | Catch_up catch_up_kind ->
      concat
        [ "Catch-up.  "
        ; Catch_up_kind.to_string_hum catch_up_kind
        ]
    | Review review_kind -> Review_kind.to_string_hum review_kind
  ;;
end

type t =
  { diff4_in_session : Diff4_in_session.t
  ; kind : Kind.t
  }
[@@deriving fields, sexp_of]

let review { Diff4_in_session.And_review_kind. diff4_in_session; review_kind } =
  { diff4_in_session
  ; kind = Review review_kind
  }
;;

let catch_up diff4_to_catch_up =
  { diff4_in_session = Diff4_to_catch_up.diff4_in_session diff4_to_catch_up
  ; kind = Catch_up (Diff4_to_catch_up.kind diff4_to_catch_up)
  }
;;

let diff4               t = Diff4_in_session.diff4              t.diff4_in_session
let id                  t = Diff4_in_session.id                 t.diff4_in_session
let is_reviewed         t = Diff4_in_session.is_reviewed        t.diff4_in_session
let path_in_repo_at_f2  t = Diff4_in_session.path_in_repo_at_f2 t.diff4_in_session

let compare_by_path_in_repo_at_f2_for_review t1 t2 =
  Diff4_in_session.compare_by_path_in_repo_at_f2_for_review
    t1.diff4_in_session t2.diff4_in_session
;;

let num_lines t reviewer =
  Diff4_in_session.num_lines t.diff4_in_session reviewer
;;

let compare_to_sort_by_decreasing_priority t1 t2 =
  Kind.compare_to_sort_by_decreasing_priority t1.kind t2.kind
;;

let by_kind_of_decreasing_priority ts =
  ts
  |> List.stable_sort ~cmp:compare_to_sort_by_decreasing_priority
  |> List.group ~break:(fun t1 t2 -> not (Kind.equal t1.kind t2.kind))
  |> List.map ~f:(fun group -> (List.hd_exn group).kind, group)
;;
