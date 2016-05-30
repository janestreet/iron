module Stable = struct
  open Import_stable

  module Row = struct
    module V1 = struct
      type 'a t =
        { feature_path  : 'a
        ; other_columns : string array
        }
      [@@deriving bin_io, fields, sexp]
    end

    module Model = V1
  end

  module Action = struct
    module V1 = struct
      type t =
        { headers                 : string Row.V1.t
        ; data                    : Feature_path.V1.t Row.V1.t list
        ; preserve_input_ordering : bool
        ; display_ascii           : bool
        ; max_output_columns      : int
        }
      [@@deriving bin_io, fields, sexp]

      let to_model m = m
    end

    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t = string [@@deriving bin_io, sexp]

      let of_model m = m
    end

    module Model = V1
  end
end

include Iron_command_rpc.Make
          (struct let name = "feature-table-of-csv" end)
          (struct let version = 1 end)
          (Stable.Action.V1)
          (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
module Row      = Stable.Row.      Model
