module Stable = struct

  open Import_stable

  module Action = struct
    module V1 = Unit
  end

  module Reaction = struct
    module V1 = struct
      type one =
        { root_feature     : Feature_name.V1.t
        ; remote_repo_path : Remote_repo_path.V1.t
        ; tip              : Rev.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      type t = one list
      [@@deriving bin_io, sexp]

      let of_model t = t
    end
  end
end

open! Core.Std
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "list-root-features" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1

include Register_map_reaction_in_client
    (struct
      (* The sort is done client side to avoid unnecessary work in Iron server. *)
      let of_server_reaction (_ : action) (reaction : reaction) =
        List.sort reaction ~cmp:(fun (t1 : Reaction.one) t2 ->
          Feature_name.compare t1.root_feature t2.root_feature)
      ;;
    end)
