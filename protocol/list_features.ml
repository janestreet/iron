module Stable = struct

  open Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t option
        ; depth        : int
        ; use_archived : bool
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end

    module Model = V1
  end

  module Reaction = struct
    module V8 = struct
      type one =
        { feature_path      : Feature_path.V1.t
        ; feature_id        : Feature_id.V1.t
        ; owners            : User_name.V1.t list
        ; review_is_enabled : bool
        ; num_lines         : int Or_error.V1.t Or_pending.V1.t
        ; next_steps        : Next_step.V5.t list
        ; status            : [ `Existing
                              | `Was_archived_at of Time.V1_round_trippable.t
                              ]
        }
      [@@deriving bin_io, sexp]

      type t = one list
      [@@deriving bin_io, sexp]

      let of_model m = m
    end

    module V7 = struct
      type one =
        { feature_path      : Feature_path.V1.t
        ; review_is_enabled : bool
        ; num_lines         : int Or_error.V1.t Or_pending.V1.t
        ; next_steps        : Next_step.V5.t list
        }
      [@@deriving bin_io]

      type t = one list
      [@@deriving bin_io]

      let of_model m =
        List.map m ~f:(fun m ->
          let { V8.
                feature_path
              ; review_is_enabled
              ; num_lines
              ; next_steps
              ; _
              } = V8.of_model m in
          { feature_path
          ; review_is_enabled
          ; num_lines
          ; next_steps
          })
      ;;
    end

    module Model = V8
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "list-features" end)
    (struct let version = 8 end)
    (Stable.Action.V1)
    (Stable.Reaction.V8)

include Register_old_rpc
    (struct let version = 7 end)
    (Stable.Action.V1)
    (Stable.Reaction.V7)

module Action   = Stable.Action.   Model
module Reaction = struct
  module Stable = Stable.Reaction
  include Stable.Model
end
