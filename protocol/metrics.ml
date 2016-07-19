module Stable = struct
  open! Import_stable

  module Which_features = Which_features.Stable

  module Clear = struct
    module Action = struct
      module V1 = struct
        type t =
          { metric_name    : Metric_name.V1.t
          ; which_features : Which_features.V1.t
          }
        [@@deriving bin_io, fields, sexp]

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

        let to_model m = m
      end

      module Model = V1
    end

    module Reaction = struct
      module V1 = struct
        type t = Metric.Snapshot.V1.t Metric_name.V1.Map.t Feature_path.V1.Map.t
        [@@deriving bin_io, sexp]

        let of_model m = m
      end

      module Model = V1
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

open! Core.Std
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
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)

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
