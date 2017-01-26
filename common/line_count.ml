module Stable = struct
  open! Core.Core_stable

  module Review_or_commit = Review_or_commit.Stable

  module Review = struct
    module V1 = struct
      type t =
        { must_review       : int
        ; follow            : int
        ; may_review        : int
        ; ownership_changes : int
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 5a2239b2680d1d46190cf770bd040374 |}]
      ;;
    end

    module Model = V1
  end

  module Catch_up = struct
    module V1 = struct
      type t =
        { create_catch_up_for_me   : int
        ; follower                 : int
        ; unfinished_review        : int
        ; reviewed_by_someone_else : int
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 29727e345cc05aee57269f3649a48f44 |}]
      ;;
    end

    module Model = V1
  end

  module V5 = struct
    type t =
      { review    : Review.V1.t
      ; catch_up  : Catch_up.V1.t
      ; completed : int
      ; have_potentially_blocking_review_session_in_progress : bool
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| eef97dbf91aa67597336a6a67a476c18 |}]
    ;;
  end

  module V4 = struct
    type t =
      { review    : Review.V1.t
      ; catch_up  : Catch_up.V1.t
      ; completed : int
      ; have_uncommitted_and_potentially_blocking_session : bool
      }
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 9048bea09e73944b1f04912460f0e852 |}]
    ;;

    open! Core
    open! Import

    let of_v5 { V5.
                review
              ; catch_up
              ; completed
              ; have_potentially_blocking_review_session_in_progress
              } =
      let have_uncommitted_and_potentially_blocking_session =
        have_potentially_blocking_review_session_in_progress
      in
      { review
      ; catch_up
      ; completed
      ; have_uncommitted_and_potentially_blocking_session
      }
    ;;

  end

  module V3 = struct
    type t =
      { review    : Review_or_commit.V1.t
      ; follow    : int
      ; catch_up  : int
      ; completed : int
      }
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 079195a277e05830df211eaaea8f7aa6 |}]
    ;;

    open! Core
    open! Import

    let of_v4 { V4.
                review =
                  { must_review
                  ; follow
                  ; may_review
                  ; ownership_changes
                  }
              ; catch_up
              ; completed
              ; have_uncommitted_and_potentially_blocking_session
              } =
      let review : Review_or_commit.V1.t =
        let review = must_review + ownership_changes in
        if review = 0
        then
          if have_uncommitted_and_potentially_blocking_session
          then Commit
          else Num 0
        else
          Num (review + may_review)
      in
      let catch_up =
        let { Catch_up.V1.
              create_catch_up_for_me
            ; follower
            ; unfinished_review
            ; reviewed_by_someone_else
            } = catch_up in
        create_catch_up_for_me
        + follower
        + unfinished_review
        + reviewed_by_someone_else
      in
      { review
      ; follow
      ; catch_up
      ; completed
      }
    ;;
  end

  module V2 = struct
    type t =
      { review    : int
      ; catch_up  : int
      ; completed : int
      }
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 2e9568ada259731ec62136f50a02d6ba |}]
    ;;

    open! Core
    open! Import

    let of_v3 { V3.
                review
              ; follow
              ; catch_up
              ; completed
              } =
      { review =
          (match review with
           | Commit -> 1
           | Num n -> n) + follow
      ; catch_up
      ; completed
      }
    ;;
  end

  module Model = V5
end

open! Core
open! Import

let update_field t field ~f = Field.fset field t (Field.get field t |> f)

module Review = struct
  include Stable.Review.Model

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      let non_negative t = assert (t >= 0) in
      Fields.iter
        ~must_review:      (check non_negative)
        ~follow:           (check non_negative)
        ~may_review:       (check non_negative)
        ~ownership_changes:(check non_negative)
    )
  ;;

  let zero =
    { must_review       = 0
    ; follow            = 0
    ; may_review        = 0
    ; ownership_changes = 0
    }
  ;;

  let total t =
    let f acc field = acc + Field.get field t in
    Fields.fold ~init:0
      ~must_review:f
      ~follow:f
      ~may_review:f
      ~ownership_changes:f
  ;;

  let add_count t review_kind count =
    let field =
      match (review_kind : Review_kind.t) with
      | Must_review      -> Fields.must_review
      | Follow           -> Fields.follow
      | May_review       -> Fields.may_review
      | Ownership_change -> Fields.ownership_changes
    in
    update_field t field ~f:(fun n -> n + count)
  ;;

  let (+) t1 t2 =
    let f field = Field.get field t1 + Field.get field t2 in
    Fields.map
      ~must_review:f
      ~follow:f
      ~may_review:f
      ~ownership_changes:f
  ;;

  let to_review_column_shown (review : t)
        ~have_potentially_blocking_review_session_in_progress =
    let lines = Int.(review.must_review + review.ownership_changes) in
    if lines = 0
    then
      if have_potentially_blocking_review_session_in_progress
      then Review_or_commit.Commit
      else Num 0
    else Num Int.(lines + review.may_review)
  ;;
end

module Catch_up = struct
  include Stable.Catch_up.Model

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      let non_negative t = assert (t >= 0) in
      Fields.iter
        ~create_catch_up_for_me:  (check non_negative)
        ~follower:                (check non_negative)
        ~unfinished_review:       (check non_negative)
        ~reviewed_by_someone_else:(check non_negative)
    )
  ;;

  let zero =
    { create_catch_up_for_me   = 0
    ; follower                 = 0
    ; unfinished_review        = 0
    ; reviewed_by_someone_else = 0
    }
  ;;

  let total t =
    let f acc field = acc + Field.get field t in
    Fields.fold ~init:0
      ~create_catch_up_for_me:  f
      ~follower:                f
      ~unfinished_review:       f
      ~reviewed_by_someone_else:f
  ;;

  let add_count t catch_up_kind count =
    let field =
      match (catch_up_kind : Catch_up_kind.t) with
      | Create_catch_up_for_me     -> Fields.create_catch_up_for_me
      | Follower                   -> Fields.follower
      | Unfinished_review          -> Fields.unfinished_review
      | Reviewed_by_someone_else _ -> Fields.reviewed_by_someone_else
    in
    update_field t field ~f:(fun n -> n + count)
  ;;

  let (+) t1 t2 =
    let f field = Field.get field t1 + Field.get field t2 in
    Fields.map
      ~create_catch_up_for_me:f
      ~follower:f
      ~unfinished_review:f
      ~reviewed_by_someone_else:f
  ;;
end

include Stable.Model

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    let non_negative t = assert (t >= 0) in
    Fields.iter
      ~review:   (check Review.invariant)
      ~catch_up: (check Catch_up.invariant)
      ~completed:(check non_negative)
      ~have_potentially_blocking_review_session_in_progress:(check (ignore : bool -> unit))
  )
;;

let zero =
  { review    = Review.zero
  ; catch_up  = Catch_up.zero
  ; completed = 0
  ; have_potentially_blocking_review_session_in_progress = false
  }
;;

let is_zero t = 0 = compare t zero

let (+) t1 t2 =
  let f f field = f (Field.get field t1) (Field.get field t2) in
  Fields.map
    ~review:(f Review.(+))
    ~catch_up:(f Catch_up.(+))
    ~completed:(f (+))
    ~have_potentially_blocking_review_session_in_progress:(f (||))
;;

let catch_up_only catch_up =
  { review    = Review.zero
  ; catch_up
  ; completed = 0
  ; have_potentially_blocking_review_session_in_progress = false
  }
;;

let _must_review_or_commit t =
  let must_review = t.review.must_review in
  if must_review = 0
  && t.have_potentially_blocking_review_session_in_progress
  then Review_or_commit.Commit
  else Num must_review
;;

let to_review_column_shown t =
  Review.to_review_column_shown t.review
    ~have_potentially_blocking_review_session_in_progress:
      t.have_potentially_blocking_review_session_in_progress
;;

let is_shown { review
             ; catch_up
             ; completed
             ; have_potentially_blocking_review_session_in_progress
             } ~show_completed_review =
  Review_or_commit.count
    (Review.to_review_column_shown review
       ~have_potentially_blocking_review_session_in_progress) > 0
  || review.follow > 0
  || Catch_up.total catch_up > 0
  || show_completed_review && completed > 0
;;

module Increasing_review = struct
  let compare =
    (if false
     then
       [ (fun t -> t.review.must_review)
       ; (fun t ->
            if t.have_potentially_blocking_review_session_in_progress
            then 1
            else 0)
       ; (fun t -> t.review.follow)
       ; (fun t -> Int.(+) t.review.may_review t.review.ownership_changes)
       ; (fun t -> Catch_up.total t.catch_up)
       ; (fun t -> t.completed)
       ]
     else
       [ (fun t -> t |> to_review_column_shown |> Review_or_commit.count)
       ; (fun t -> t.review.follow)
       ; (fun t -> Catch_up.total t.catch_up)
       ; (fun t -> t.completed)
       ])
    |> List.map ~f:(fun f t1 t2 -> Int.compare (f t1) (f t2))
    |> Comparable.lexicographic
  ;;
end

let sort_decreasing_review us =
  List.sort us ~cmp:(fun (u1, c1) (u2, c2) ->
    let res = Increasing_review.compare c2 c1 in
    if res <> 0 then res else User_name.compare u1 u2)
;;

module Cached_in_feature = struct
  module T = Stable.Model

  type t =
    { review    : Review.t To_goal_via_session.t
    ; completed : int
    ; have_potentially_blocking_review_session_in_progress : bool
    }
  [@@deriving compare, fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      let non_negative t = assert (t >= 0) in
      Fields.iter
        ~review:   (check (To_goal_via_session.invariant Review.invariant))
        ~completed:(check non_negative)
        ~have_potentially_blocking_review_session_in_progress:
          (check (ignore : bool -> unit))
    )
  ;;

  module By_user = struct
    type nonrec t = t User_name.Map.t [@@deriving compare, sexp_of]
    let equal t1 t2 = 0 = compare t1 t2

    let invariant t =
      Map.iteri t ~f:(fun ~key:user_name ~data ->
        User_name.invariant user_name;
        Invariant.invariant [%here] user_name [%sexp_of: User_name.t] (fun () ->
          invariant data));
    ;;
  end
end
