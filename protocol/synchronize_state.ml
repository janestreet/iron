open! Core.Std.No_polymorphic_compare

module Stable = struct

  open Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { remote_repo_path : Remote_repo_path.V1.t
        ; bookmarks        : Hydra_state_for_bookmark.Stable.V1.t list
        }
      [@@deriving bin_io, compare, fields, sexp]

      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = struct
      type t =
        { bookmarks_to_rerun : string list
        }
      [@@deriving bin_io, sexp, compare]

      let of_model t = t
    end
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "synchronize-state" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1
