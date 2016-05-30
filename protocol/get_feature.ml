module Stable = struct

  open Import_stable

  module Feature = Feature.Stable

  module By_id = struct
    module Action = struct
      module V2 = struct
        type t =
          { feature_id       : Feature_id.V1.t
          ; even_if_archived : bool
          }
        [@@deriving bin_io, sexp, fields]

        let to_model t = t
      end

      module Model = V2

      module V1 = struct
        type t = Feature_id.V1.t
        [@@deriving bin_io]

        let to_model feature_id =
          V2.to_model
            { V2.
              feature_id
            ; even_if_archived = false
            }
        ;;
      end
    end

    module Reaction = Feature
  end

  module Maybe_archived = struct
    module Action = struct
      module V2 = struct
        type t =
          { what_feature : Maybe_archived_feature_spec.V2.t
          ; what_diff    : What_diff.V2.t
          }
        [@@deriving bin_io, compare, fields, sexp]

        let to_model t = t
      end

      module V1 = struct
        type t =
          { what_feature : Maybe_archived_feature_spec.V1.t
          ; what_diff    : What_diff.V2.t
          }
        [@@deriving bin_io]

        let to_model { what_feature
                     ; what_diff
                     } =
          V2.to_model
            { V2.
              what_feature = Maybe_archived_feature_spec.V1.to_v2 what_feature
            ; what_diff
            }
        ;;
      end

      module Model = V2
    end

    module Reaction = Feature
  end

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t option
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end
    module Model = V1
  end

  module Reaction = Feature
end

open! Core.Std
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "get-feature" end)
    (struct let version = 19 end)
    (Stable.Action.V1)
    (Stable.Reaction.V19)

include Register_old_rpc
    (struct let version = 18 end)
    (Stable.Action.V1)
    (Stable.Reaction.V18)

include Register_old_rpc
    (struct let version = 17 end)
    (Stable.Action.V1)
    (Stable.Reaction.V17)

include Register_old_rpc
    (struct let version = 16 end)
    (Stable.Action.V1)
    (Stable.Reaction.V16)

include Register_old_rpc
    (struct let version = 15 end)
    (Stable.Action.V1)
    (Stable.Reaction.V15)

include Register_old_rpc
    (struct let version = 14 end)
    (Stable.Action.V1)
    (Stable.Reaction.V14)

include Register_old_rpc
    (struct let version = 13 end)
    (Stable.Action.V1)
    (Stable.Reaction.V13)

include Register_old_rpc
    (struct let version = 12 end)
    (Stable.Action.V1)
    (Stable.Reaction.V12)

include Register_old_rpc
    (struct let version = 11 end)
    (Stable.Action.V1)
    (Stable.Reaction.V11)

include Register_old_rpc
    (struct let version = 10 end)
    (Stable.Action.V1)
    (Stable.Reaction.V10)

module Action    = Stable.Action.Model
module Reaction  = Feature

module By_id = struct
  include Iron_versioned_rpc.Make
      (struct let name = "get-feature-by-id" end)
      (struct let version = 19 end)
      (Stable.By_id.Action.V2)
      (Stable.By_id.Reaction.V19)

  include Register_old_rpc
      (struct let version = 18 end)
      (Stable.By_id.Action.V2)
      (Stable.By_id.Reaction.V18)

  include Register_old_rpc
      (struct let version = 17 end)
      (Stable.By_id.Action.V2)
      (Stable.By_id.Reaction.V17)

  include Register_old_rpc
      (struct let version = 16 end)
      (Stable.By_id.Action.V2)
      (Stable.By_id.Reaction.V16)

  include Register_old_rpc
      (struct let version = 15 end)
      (Stable.By_id.Action.V2)
      (Stable.By_id.Reaction.V15)

  include Register_old_rpc
      (struct let version = 14 end)
      (Stable.By_id.Action.V1)
      (Stable.By_id.Reaction.V14)

  include Register_old_rpc
      (struct let version = 13 end)
      (Stable.By_id.Action.V1)
      (Stable.By_id.Reaction.V13)

  module Action    = Stable.By_id.Action.Model
  module Reaction  = Feature
end

module Maybe_archived = struct
  include Iron_versioned_rpc.Make
      (struct let name = "get-feature-maybe-archived" end)
      (struct let version = 6 end)
      (Stable.Maybe_archived.Action.V2)
      (Stable.Maybe_archived.Reaction.V19)

  include Register_old_rpc
      (struct let version = 5 end)
      (Stable.Maybe_archived.Action.V2)
      (Stable.Maybe_archived.Reaction.V18)

  include Register_old_rpc
      (struct let version = 4 end)
      (Stable.Maybe_archived.Action.V2)
      (Stable.Maybe_archived.Reaction.V17)

  include Register_old_rpc
      (struct let version = 3 end)
      (Stable.Maybe_archived.Action.V1)
      (Stable.Maybe_archived.Reaction.V17)

  include Register_old_rpc
      (struct let version = 2 end)
      (Stable.Maybe_archived.Action.V1)
      (Stable.Maybe_archived.Reaction.V16)

  include Register_old_rpc
      (struct let version = 1 end)
      (Stable.Maybe_archived.Action.V1)
      (Stable.Maybe_archived.Reaction.V15)

  module Action    = Stable.Maybe_archived.Action.Model
  module Reaction  = Feature
end
