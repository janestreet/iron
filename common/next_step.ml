module Stable = struct
  open! Core.Core_stable

  module Lock_name = struct
    module V2 = struct
      type t =
        | Rebase
        | Release
        | Release_into
        | Second
      [@@deriving bin_io, compare, enumerate, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 4e45e65024da5c32032aace9f5224252 |}]
      ;;
    end
    module V1 = struct
      type t =
        | Rebase
        | Release
        | Release_into
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 87cdeca4e3128fc2c80f2225d290c51b |}]
      ;;

      let to_v2 : t -> V2.t = function
        | Rebase       -> Rebase
        | Release      -> Release
        | Release_into -> Release_into
      ;;
    end
    module Model = V2
  end

  module V6 = struct
    type t =
      | Add_code
      | Add_whole_feature_reviewer
      | Archive
      | Ask_seconder
      | Compress
      | CRs
      | Enable_review
      | Fix_build
      | Fix_problems
      | In_parent of t
      | Rebase
      | Release
      | Report_iron_bug
      | Restore_base
      | Restore_bookmark
      | Review
      | Unlock of Lock_name.V2.t
      | Wait_for_continuous_release
      | Wait_for_hydra
      | Widen_reviewing
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 6d9520df5039fa99848267b5d70dde7c |}]
    ;;
  end

  module V5 = struct
    type t =
      | Add_code
      | Add_whole_feature_reviewer
      | Ask_seconder
      | CRs
      | Enable_review
      | Fix_problems
      | In_parent of t
      | Rebase
      | Release
      | Report_iron_bug
      | Restore_bookmark
      | Review
      | Unlock of Lock_name.V1.t
      | Wait_for_hydra
      | Widen_reviewing
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| ca9bc57a0d113d2d47f9c3246ff1fcfe |}]
    ;;

    open! Core
    open! Import

    let rec to_v6 : t -> V6.t = function
      | Add_code                   -> Add_code
      | Add_whole_feature_reviewer -> Add_whole_feature_reviewer
      | Ask_seconder               -> Ask_seconder
      | CRs                        -> CRs
      | Enable_review              -> Enable_review
      | Fix_problems               -> Fix_problems
      | In_parent m                -> In_parent (to_v6 m)
      | Rebase                     -> Rebase
      | Release                    -> Release
      | Report_iron_bug            -> Report_iron_bug
      | Restore_bookmark           -> Restore_bookmark
      | Review                     -> Review
      | Unlock lock_name           -> Unlock (Lock_name.V1.to_v2 lock_name)
      | Wait_for_hydra             -> Wait_for_hydra
      | Widen_reviewing            -> Widen_reviewing
    ;;

    let rec of_v6 : V6.t -> t = function
      | Add_code                    -> Add_code
      | Add_whole_feature_reviewer  -> Add_whole_feature_reviewer
      | Archive | Compress          -> Add_code
      | Ask_seconder                -> Ask_seconder
      | CRs                         -> CRs
      | Enable_review               -> Enable_review
      | Fix_build | Fix_problems    -> Fix_problems
      | In_parent m                 -> In_parent (of_v6 m)
      | Rebase                     -> Rebase
      | Release                    -> Release
      | Report_iron_bug             -> Report_iron_bug
      | Restore_base                -> Fix_problems
      | Restore_bookmark            -> Restore_bookmark
      | Review                      -> Review
      | Unlock l -> (match l with
        | Rebase                    -> Unlock Rebase
        | Release                   -> Unlock Release
        | Release_into              -> Unlock Release_into
        | Second                    -> Ask_seconder)
      | Wait_for_hydra              -> Wait_for_hydra
      | Wait_for_continuous_release -> Release
      | Widen_reviewing             -> Widen_reviewing
    ;;
  end

  module Model = V6
end

open! Core
open! Import

module Lock_name = struct
  include Stable.Lock_name.Model

  let to_lock_name = function
    | Rebase       -> Lock_name.Rebase
    | Release      -> Release
    | Release_into -> Release_into
    | Second       -> Second
  ;;

  let to_string_hum t = Lock_name.to_string_hum (to_lock_name t)

  let of_lock_name : Lock_name.t -> t option = function
    | Create_child -> None
    | Rebase       -> Some Rebase
    | Release      -> Some Release
    | Release_into -> Some Release_into
    | Rename       -> None
    | Second       -> Some Second
  ;;

  let%test "round trip via Lock_name.t" =
    List.for_all all ~f:(fun t ->
      [%compare.equal:t] t (t |> to_lock_name |> of_lock_name |> Option.value_exn))
  ;;

  let%test "we need two types" =
    let ts = List.map Lock_name.all ~f:of_lock_name in
    List.exists ts ~f:Option.is_some
    && List.exists ts ~f:Option.is_none
  ;;
end

include Stable.Model

module For_command_line = struct
  type nonrec t = t [@@deriving sexp]
end

let equal a b = compare a b = 0

let rec to_string_hum = function
  | Add_code                    -> "add code"
  | Add_whole_feature_reviewer  -> "add w-f-reviewer"
  | Archive                     -> "archive"
  | Ask_seconder                -> "ask seconder"
  | Compress                    -> "compress"
  | CRs                         -> "CRs"
  | Enable_review               -> "enable-review"
  | Fix_build                   -> "fix build"
  | Fix_problems                -> "fix problems"
  | In_parent t                 -> to_string_hum t ^ " in parent"
  | Rebase                      -> "rebase"
  | Release                     -> "release"
  | Report_iron_bug             -> "report Iron bug"
  | Restore_base                -> "restore base"
  | Restore_bookmark            -> "restore bookmark"
  | Review                      -> "review"
  | Unlock lock_name            -> concat [ "unlock "
                                          ; Lock_name.to_string_hum lock_name
                                          ]
  | Wait_for_continuous_release -> "wait for continuous release"
  | Wait_for_hydra              -> "wait for hydra"
  | Widen_reviewing             -> "widen reviewing"
;;

let should_be_red = function
  | Fix_build
  | Fix_problems
  | Report_iron_bug
  | Restore_base
  | Restore_bookmark
    -> true
  | Add_code
  | Add_whole_feature_reviewer
  | Archive
  | Ask_seconder
  | Compress
  | CRs
  | Enable_review
  | In_parent _
  | Rebase
  | Release
  | Review
  | Unlock _
  | Wait_for_continuous_release
  | Wait_for_hydra
  | Widen_reviewing
    -> false
;;

let should_be_green = function
  | Release
  | Wait_for_continuous_release -> true
  | Add_code
  | Add_whole_feature_reviewer
  | Archive
  | Ask_seconder
  | Compress
  | CRs
  | Enable_review
  | Fix_build
  | Fix_problems
  | In_parent _
  | Rebase
  | Report_iron_bug
  | Restore_base
  | Restore_bookmark
  | Review
  | Unlock _
  | Wait_for_hydra
  | Widen_reviewing
    -> false
;;

let to_attrs ts ~review_is_enabled =
  if List.exists ts ~f:should_be_red
  then [ `Red ]
  else if List.exists ts ~f:should_be_green
  then [ `Green ]
  else if review_is_enabled
  then [ `Yellow ]
  else []
;;

let to_attrs_and_string ts ~review_is_enabled =
  let attrs = to_attrs ts ~review_is_enabled in
  let text = concat ~sep:", " (List.map ts ~f:to_string_hum) in
  attrs, text
;;

module Assigned = struct
  type nonrec t = t list [@@deriving sexp_of]

  let t_to_string_hum = to_string_hum

  let rec to_string_hum t =
    match t with
    | In_parent t               -> to_string_hum t ^ " in parent"
    | Ask_seconder              -> "second"
    | Add_code
    | Add_whole_feature_reviewer
    | Archive
    | Compress
    | CRs
    | Enable_review
    | Fix_build
    | Fix_problems
    | Rebase
    | Release
    | Report_iron_bug
    | Restore_base
    | Restore_bookmark
    | Review
    | Unlock _
    | Wait_for_continuous_release
    | Wait_for_hydra
    | Widen_reviewing
      -> t_to_string_hum t
  ;;

  let to_attrs_and_string ts ~review_is_enabled =
    let attrs = to_attrs ts ~review_is_enabled in
    let text = concat ~sep:", " (List.map ts ~f:to_string_hum) in
    attrs, text
  ;;

  let%expect_test _ =
    print_endline (to_string_hum (In_parent Report_iron_bug));
    [%expect {| report Iron bug in parent |}]
  ;;
end
