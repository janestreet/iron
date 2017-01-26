module Stable = struct

  open! Import_stable

  module Action = struct
    module V4 = struct
      type t =
        { feature_path      : Feature_path.V1.t
        ; even_though_empty : bool
        ; even_though_owner : bool
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 7ff04ba3148a46af4f8d6c86ab5c1742 |}]
      ;;

      let to_model t = t
    end

    module V3 = struct
      type t =
        { feature_path      : Feature_path.V1.t
        ; for_              : User_name.V1.t
        ; even_though_empty : bool
        ; even_though_owner : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 5cb55e299a244f539a71ba80e59b483f |}]
      ;;

      let to_model { feature_path; even_though_empty; even_though_owner; _ } =
        V4.to_model { V4. feature_path; even_though_empty; even_though_owner }
      ;;
    end

    module Model = V4
  end

  module Reaction = struct
    module V4 = struct
      type t =
        { whole_feature_review_remaining : (User_name.V1.t * Line_count.V5.t) list
        ; cr_summary                     : Cr_comment.Summary.V1.t
        }
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| fbce2789574a1bb8c9eb2f6213da66f4 |}]
      ;;

      let of_model (t : t) = t
    end

    module V3 = struct
      type t =
        { whole_feature_review_remaining : (User_name.V1.t * Line_count.V4.t) list
        ; cr_summary                     : Cr_comment.Summary.V1.t
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 301ff6cc77fea9fb782240f09389519b |}]
      ;;

      let of_model m =
        let { V4.
              whole_feature_review_remaining
            ; cr_summary
            } = V4.of_model m in
        let whole_feature_review_remaining =
          List.map whole_feature_review_remaining ~f:(fun (user, line_count) ->
            user, Line_count.V4.of_v5 line_count
          )
        in
        { whole_feature_review_remaining
        ; cr_summary
        }
      ;;
    end

    module V2 = struct
      type t =
        { whole_feature_review_remaining : (User_name.V1.t * Line_count.V3.t) list
        ; cr_summary                     : Cr_comment.Summary.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 3b54b028bf5a34ef17bd0a9e3e2c0ec2 |}]
      ;;

      let of_model m =
        let { V3.
              whole_feature_review_remaining
            ; cr_summary
            } = V3.of_model m in
        let whole_feature_review_remaining =
          List.map whole_feature_review_remaining ~f:(fun (user, line_count) ->
            user, Line_count.V3.of_v4 line_count
          )
        in
        { whole_feature_review_remaining
        ; cr_summary
        }
      ;;
    end

    module V1 = struct
      type t =
        { whole_feature_review_remaining : (User_name.V1.t * int) list
        ; cr_summary                     : Cr_comment.Summary.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| c4143193b95d3b2606f68fed69d04a41 |}]
      ;;

      open! Core
      open! Import

      let of_model m =
        let { V2.
              whole_feature_review_remaining
            ; cr_summary
            } = V2.of_model m in
        let whole_feature_review_remaining =
          List.map whole_feature_review_remaining ~f:(fun (user, line_count) ->
            user, Review_or_commit.count line_count.review
          )
        in
        { whole_feature_review_remaining
        ; cr_summary
        }
      ;;
    end

    module Model = V4
  end

end

include Iron_versioned_rpc.Make
    (struct let name = "prepare-to-second" end)
    (struct let version = 7 end)
    (Stable.Action.V4)
    (Stable.Reaction.V4)

include Register_old_rpc
    (struct let version = 6 end)
    (Stable.Action.V4)
    (Stable.Reaction.V3)

include Register_old_rpc
    (struct let version = 5 end)
    (Stable.Action.V4)
    (Stable.Reaction.V2)

include Register_old_rpc
    (struct let version = 4 end)
    (Stable.Action.V4)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 3 end)
    (Stable.Action.V3)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
