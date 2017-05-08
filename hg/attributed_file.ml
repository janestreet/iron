module Stable = struct

  open! Core.Core_stable
  open! Iron_common.Stable

  module Rev = Rev.Stable

  module Attributes = struct

    module V1 = struct
      type t =
        { hash                               : File_contents_hash.V1.t
        ; owner                              : User_name.V1.t
        ; file_reviewers                     : User_name.V1.t list
        ; followers                          : User_name.V1.t list
        ; scrutiny_level                     : Scrutiny_level.V1.t
        ; is_read_by_whole_feature_reviewers : bool
        ; num_lines                          : int
        }
      [@@deriving sexp]
    end

    (* [V2] writes the same sexp format as [V1], and reads both the [V1] and [V2] formats.
       When we are comfortable with this code in production, we will change [V2] to write
       the [V2] format.  At that point, we will not be able to roll back to the pre-[V2]
       code, because the old [V1] reader will not be able to read the new format.  But
       [V2] will still be able to read the [V1] format. *)
    module V2 = struct
      type t =
        { hash                               : File_contents_hash.V1.t
        ; owner                              : User_name.V1.t
        ; review_obligation                  : Review_obligation.V1.t
        ; followers                          : User_name.V1.Set.t
        ; scrutiny                           : File_scrutiny.V1.t
        ; is_read_by_whole_feature_reviewers : bool
        ; num_lines                          : int
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 22403e51c42e09e66dab7e49bac34b06 |}]
      ;;

      open Core
      open Import

      let of_v1 { V1. hash; owner; followers; scrutiny_level
                ; is_read_by_whole_feature_reviewers; num_lines; file_reviewers
                } =
        let scrutiny_name =
          Scrutiny_name.of_string
            (* The only obligations-global.sexp is the one in jane. *)
            (match Scrutiny_level.to_int scrutiny_level with
             | 0  -> "ignore"
             | 50 -> "normal"
             | 85 -> "critical_path"
             | value -> sprintf "level%d" value)
        in
        let scrutiny = File_scrutiny.create scrutiny_name scrutiny_level in
        let review_obligation =
          Review_obligation.all_of (User_name.Set.of_list file_reviewers)
        in
        { hash
        ; owner
        ; review_obligation
        ; followers                          = User_name.Set.of_list followers
        ; scrutiny
        ; is_read_by_whole_feature_reviewers
        ; num_lines
        }
      ;;

      let t_of_sexp sexp =
        try of_v1 (V1.t_of_sexp sexp)
        with
        | exn_v1 ->
          try t_of_sexp sexp
          with
          | exn_v2 ->
            raise_s
              [%sexp
                "Attributes.V2.t_of_sexp",
                { v1 = (exn_v1 : Exn.t)
                ; v2 = (exn_v2 : Exn.t)
                }
              ]
      ;;
    end
  end

  module V2 = struct
    type t =
      { path_in_repo   : Path_in_repo.V1.t
      ; rev            : Rev.V1.t
      ; attributes     : [ `Absent | `Present of Attributes.V2.t ]
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 9a11f8d3c900873f41f982658d7339cb |}]
    ;;
  end
end

open! Core
open! Async
open! Import

module Attributes = struct
  include Stable.Attributes.V2

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~hash:ignore
        ~owner:ignore
        ~review_obligation:ignore
        ~followers:ignore
        ~scrutiny:ignore
        ~is_read_by_whole_feature_reviewers:ignore
        ~num_lines:(check ([%test_pred: int] (fun num_lines -> num_lines >= 0))))
  ;;

  let create { Review_attributes.
               build_projections                  = _
             ; tags                               = _
             ; fewer_than_min_reviewers           = _
             ; followers
             ; is_read_by_whole_feature_reviewers
             ; more_than_max_reviewers            = _
             ; owner
             ; review_obligation
             ; scrutiny_level
             ; scrutiny_name
             }
        ~hash ~num_lines =
    let scrutiny = File_scrutiny.create scrutiny_name scrutiny_level in
    { hash
    ; owner
    ; review_obligation
    ; followers
    ; scrutiny
    ; is_read_by_whole_feature_reviewers
    ; num_lines
    }
  ;;

  let may_review_internal t (reviewer : Reviewer.t) =
    if reviewer.is_whole_feature_reviewer
    then
      t.is_read_by_whole_feature_reviewers
      || Review_obligation.has_a_may_reviewer t.review_obligation
    else
      Review_obligation.may_review t.review_obligation reviewer.user_name
  ;;

  let may_follow t (reviewer : Reviewer.t) =
    Set.mem t.followers reviewer.user_name
    || reviewer.is_whole_feature_follower
       && may_review_internal t { reviewer with is_whole_feature_reviewer = true }
  ;;

  let may_review t ~include_may_follow (reviewer : Reviewer.t) =
    may_review_internal t reviewer
    || ( include_may_follow && may_follow t reviewer )
  ;;

  let may_reviewers t ~include_file_followers =
    Set.union (Review_obligation.may_reviewers t.review_obligation)
      (if include_file_followers then t.followers else User_name.Set.empty)
  ;;

  let equal_file_contents t1 t2 = File_contents_hash.equal t1.hash t2.hash

  let de_alias t user_name_by_alias =
    let de_alias user_name =
      User_name_by_alternate_name.to_user_name user_name_by_alias
        (User_name.to_unresolved_name user_name)
    in
    let de_alias_set user_names = User_name.Set.map user_names ~f:de_alias in
    { hash                               = t.hash
    ; owner                              = de_alias t.owner
    ; review_obligation                  = Review_obligation.de_alias t.review_obligation
                                             user_name_by_alias
    ; followers                          = de_alias_set t.followers
    ; scrutiny                           = t.scrutiny
    ; is_read_by_whole_feature_reviewers = t.is_read_by_whole_feature_reviewers
    ; num_lines                          = t.num_lines
    }
  ;;
end

include Stable.V2

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~path_in_repo:ignore
      ~rev:ignore
      ~attributes:(check (function
        | `Absent -> ()
        | `Present attributes -> Attributes.invariant attributes))
  )
;;

let create ~path_in_repo ~rev ~file review_attributes =
  let%map contents = Reader.file_contents (Abspath.to_string file) in
  let hash = File_contents_hash.create contents in
  let num_lines = line_count contents in
  let attributes = Attributes.create review_attributes ~hash ~num_lines in
  { path_in_repo
  ; rev
  ; attributes   = `Present attributes
  }
;;

let with_path_in_repo t path_in_repo =
  match t.attributes with
  | `Present _ ->
    raise_s [%sexp "with_path_in_repo should only be used on absent files"
                 , (t : t), (path_in_repo : Path_in_repo.t)]
  | `Absent ->
    { t with path_in_repo }
;;

let absent ~path_in_repo ~rev = { path_in_repo; rev; attributes = `Absent }

module Ignoring_rev = struct
  module T = struct
    type nonrec t = t [@@deriving sexp]
    let compare t1 t2 = compare t1 { t2 with rev = t1.rev }
    let hash t =
      Path_in_repo.hash t.path_in_repo
      lxor (match t.attributes with
        | `Absent -> Hashtbl.hash attributes
        | `Present { Attributes. hash; _ } -> File_contents_hash.hash hash)
    ;;
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

let file_digest t =
  match t.attributes with
  | `Absent       -> None
  | `Present attr -> Some attr.hash
;;

let equal_file_contents t1 t2 =
  match t1.attributes, t2.attributes with
  | `Absent    , `Absent     -> true
  | `Present _ , `Absent
  | `Absent    , `Present _  -> false
  | `Present t1, `Present t2 -> Attributes.equal_file_contents t1 t2
;;

let scrutiny t =
  match t.attributes with
  | `Present t -> t.scrutiny
  | `Absent    -> File_scrutiny.ignored
;;

let num_lines t =
  match t.attributes with
  | `Absent    -> 0
  | `Present t -> t.num_lines
;;

let is_present t =
  match t.attributes with
  | `Present _ -> true
  | `Absent    -> false
;;

let status ~src:t1 ~dst:t2 =
  match t1.attributes, t2.attributes with
  | `Absent   , `Absent
  | `Present _, `Absent    -> `Removed t2
  | `Absent   , `Present _ -> `Added t1
  | `Present _, `Present _ -> `Modified (t1, t2)
;;

let present_paths_in_repo ts =
  List.filter_map ts ~f:(fun t ->
    match t.attributes with
    | `Absent -> None
    | `Present _ -> Some (t.rev, t.path_in_repo))
;;

let normalize_paths ~base ~tip =
  match base.attributes, tip.attributes with
  | `Present _, `Present _ -> base, tip
  | `Present _, `Absent    -> base, with_path_in_repo tip base.path_in_repo
  | `Absent   , `Present _
  | `Absent   , `Absent    -> with_path_in_repo base tip.path_in_repo, tip
;;

let de_alias t user_name_by_alias =
  let attributes =
    match t.attributes with
    | `Absent -> `Absent
    | `Present attributes -> `Present (Attributes.de_alias attributes user_name_by_alias)
  in
  { path_in_repo = t.path_in_repo
  ; rev          = t.rev
  ; attributes
  }
;;
