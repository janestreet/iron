module Stable = struct
  open! Import_stable

  module Clear = struct
    module Action = struct
      module V1 = struct
        type t =
          { metric_name    : Metric_name.V1.t
          ; which_features : Which_features.V1.t
          }
        [@@deriving bin_io, fields, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 1ae7dd7979fd084ccc47f380f89c3c3b |}]
        ;;

        let to_model m = m
      end
      module Model = V1
    end

    module Reaction = struct
      module V1 = Unit
      module Model = V1
    end
  end

  module Get = struct
    module Action = struct
      module V1 = struct
        type t = { descendants_of : Which_ancestor.V1.t }
        [@@deriving bin_io, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 8d5fd68329245acaeecf05871af92405 |}]
        ;;

        let to_model m = m
      end

      module Model = V1
    end

    module Reaction = struct
      module V2 = struct
        type t = Metric.Data_point.V1.t list Metric_name.V1.Map.t Feature_path.V1.Map.t
        [@@deriving bin_io, sexp_of]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 1343bc0d4bc5b16aac96b0791d4d82c5 |}]
        ;;

        let of_model m = m
      end

      module Model = V2
    end
  end

  module Add_values = struct
    module Action = struct
      module V1 = struct
        type t =
          { feature_path : Feature_path.V1.t
          ; metric_name  : Metric_name.V1.t
          ; values       : float list
          }
        [@@deriving bin_io, fields, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 5d76fbbf0c26a48aa357862930d99eee |}]
        ;;

        let to_model m = m
      end

      module Model = V1
    end

    module Reaction = struct
      module V1 = Unit
      module Model = V1
    end
  end
end

open! Core
open! Import

module Clear = struct
  module Stable = Stable.Clear

  include Iron_versioned_rpc.Make
      (struct let name = "metrics-clear" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)

  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end

module Get = struct
  module Stable = Stable.Get

  include Iron_versioned_rpc.Make
      (struct let name = "metrics-get" end)
      (struct let version = 2 end)
      (Stable.Action.V1)
      (Stable.Reaction.V2)

  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end

module Add_values = struct
  module Stable = Stable.Add_values

  include Iron_versioned_rpc.Make
      (struct let name = "metrics-add-values" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)

  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end
