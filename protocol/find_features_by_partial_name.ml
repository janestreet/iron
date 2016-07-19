module Stable = struct

  open Import_stable

  module Namespace = struct
    module V4 = Maybe_archived_feature_spec.Namespace.V4

    module V3 = Maybe_archived_feature_spec.Namespace.V2

    module V2 = struct
      type t =
        [ `All
        | `Archived
        | `Existing
        ]
      [@@deriving bin_io]

      let to_v3 (t : t) = (t :> V3.t)
    end

    module V1 = struct
      type t =
        [ `Archived
        | `Existing
        | `Catch_up
        ]
      [@@deriving bin_io]

      let to_v2 = function
        | `Catch_up -> `All
        | ( `Archived | `Existing ) as t -> t
      ;;
    end

    module Model = V4
  end

  module Action = struct
    module V5 = struct
      type t =
        { partial_name_prefix : string
        ; namespace           : Namespace.V4.t
        }
      [@@deriving bin_io, fields, sexp]

      let to_model (t : t) = t
    end

    module V4 = struct
      type t =
        { partial_name_prefix : string
        ; namespace           : Namespace.V3.t
        }
      [@@deriving bin_io]

      let to_model { partial_name_prefix
                   ; namespace
                   } =
        V5.to_model
          { V5.
            partial_name_prefix
          ; namespace = Namespace.V3.to_v4 namespace
          }
      ;;
    end

    module V3 = struct
      type t =
        { partial_name_prefix : string
        ; namespace           : Namespace.V2.t
        }
      [@@deriving bin_io]

      let to_model { partial_name_prefix
                   ; namespace
                   } =
        V4.to_model
          { V4.partial_name_prefix
          ; namespace = Namespace.V2.to_v3 namespace
          }
      ;;
    end

    module V2 = struct
      type t =
        { partial_name_prefix : string
        ; namespace           : Namespace.V1.t
        }
      [@@deriving bin_io]

      let to_model { partial_name_prefix; namespace } =
        V3.to_model
          { V3.partial_name_prefix
          ; namespace = Namespace.V1.to_v2 namespace
          }
      ;;
    end

    module Model = V5
  end


  module Reaction = struct
    module V4 = struct
      type t = Feature_path.V1.t list [@@deriving bin_io, sexp]

      let of_model (m : t) = m
    end

    module V3 = struct
      type t =
        { matching_features : Feature_path.V1.t list
        ; rev_zero_roots    : (Feature_name.V1.t * Rev.V1.t) list
        }
      [@@deriving bin_io]

      let of_model m =
        let matching_features = V4.of_model m in
        { matching_features
        ; rev_zero_roots = []
        }
      ;;
    end

    module V2 = struct
      type t = Feature_path.V1.t list
      [@@deriving bin_io]

      let of_model m =
        let { V3. matching_features; _ } = V3.of_model m in
        matching_features
      ;;
    end

    module Model = V4
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "find-features-by-partial-name" end)
    (struct let version = 6 end)
    (Stable.Action.V5)
    (Stable.Reaction.V4)

include Register_old_rpc
    (struct let version = 5 end)
    (Stable.Action.V4)
    (Stable.Reaction.V3)

include Register_old_rpc
    (struct let version = 4 end)
    (Stable.Action.V3)
    (Stable.Reaction.V3)

include Register_old_rpc
    (struct let version = 3 end)
    (Stable.Action.V2)
    (Stable.Reaction.V3)

include Register_old_rpc
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V2)

module Action    = Stable.Action.   Model
module Namespace = Stable.Namespace.Model
module Reaction  = Stable.Reaction. Model
