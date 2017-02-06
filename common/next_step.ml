module Stable = struct
  module Lock_name = struct
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
    end

    module Model = V1
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

    let of_model m = m
  end

  module V4 = struct
    type t =
      | Add_code
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
      [%expect {| e8702181bb86ae1a93c239949568ac9d |}]
    ;;

    let rec of_v5 (v5 : V5.t) : t =
      match v5 with
      | Add_code                   -> Add_code
      | Add_whole_feature_reviewer -> Ask_seconder
      | Ask_seconder               -> Ask_seconder
      | CRs                        -> CRs
      | Enable_review              -> Enable_review
      | Fix_problems               -> Fix_problems
      | In_parent m                -> In_parent (of_v5 m)
      | Rebase                     -> Rebase
      | Release                    -> Release
      | Report_iron_bug            -> Report_iron_bug
      | Restore_bookmark           -> Fix_problems
      | Review                     -> Review
      | Unlock l                   -> Unlock l
      | Wait_for_hydra             -> Wait_for_hydra
      | Widen_reviewing            -> Widen_reviewing
    ;;

    let of_model m = m |> V5.of_model |> of_v5
  end

  module V3 = struct
    type t =
      | Add_code
      | Ask_seconder
      | CRs
      | Enable_review
      | Fix_problems
      | In_parent of t
      | Rebase
      | Release
      | Report_iron_bug
      | Review
      | Unlock of Lock_name.V1.t
      | Wait_for_hydra
      | Widen_reviewing
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| a609dc93b67521da20ae0e4bc3f2249d |}]
    ;;

    let rec of_v4 : V4.t -> t = function
      | Add_code         -> Add_code
      | Ask_seconder     -> Ask_seconder
      | CRs              -> CRs
      | Enable_review    -> Enable_review
      | Fix_problems     -> Fix_problems
      | In_parent m      -> In_parent (of_v4 m)
      | Rebase           -> Rebase
      | Release          -> Release
      | Report_iron_bug  -> Report_iron_bug
      | Restore_bookmark -> Fix_problems
      | Review           -> Review
      | Unlock l         -> Unlock l
      | Wait_for_hydra   -> Wait_for_hydra
      | Widen_reviewing  -> Widen_reviewing
    ;;

    let of_model m = m |> V4.of_model |> of_v4
  end

  module Model = V5
end

open! Core
open! Import

module Lock_name = struct
  include Stable.Lock_name.Model

  let to_lock_name = function
    | Rebase       -> Lock_name.Rebase
    | Release      -> Release
    | Release_into -> Release_into
  ;;

  let to_string_hum t = Lock_name.to_string_hum (to_lock_name t)
end

include Stable.Model

module For_command_line = struct
  type nonrec t = t [@@deriving sexp]
end

let equal a b = compare a b = 0

let rec to_string_hum = function
  | Add_code                   -> "add code"
  | Add_whole_feature_reviewer -> "add w-f-reviewer"
  | Ask_seconder               -> "ask seconder"
  | CRs                        -> "CRs"
  | Enable_review              -> "enable-review"
  | Fix_problems               -> "fix problems"
  | In_parent t                -> to_string_hum t ^ " in parent"
  | Rebase                     -> "rebase"
  | Release                    -> "release"
  | Report_iron_bug            -> "report Iron bug"
  | Restore_bookmark           -> "restore bookmark"
  | Review                     -> "review"
  | Unlock lock_name           -> concat [ "unlock "
                                         ; Lock_name.to_string_hum lock_name
                                         ]
  | Wait_for_hydra             -> "wait for hydra"
  | Widen_reviewing            -> "widen reviewing"
;;

let should_be_red = function
  | Fix_problems
  | Report_iron_bug
  | Restore_bookmark
    -> true
  | Add_code
  | Add_whole_feature_reviewer
  | Ask_seconder
  | CRs
  | Enable_review
  | In_parent _
  | Rebase
  | Release
  | Review
  | Unlock _
  | Wait_for_hydra
  | Widen_reviewing
    -> false
;;

let to_attrs_and_string ts ~review_is_enabled =
  let color =
    if List.exists ts ~f:should_be_red
    then [ `Red ]
    else if List.mem ts Release ~equal
    then [ `Green ]
    else if review_is_enabled
    then [ `Yellow ]
    else []
  in
  let text = concat ~sep:", " (List.map ts ~f:to_string_hum) in
  color, text
;;
