module Stable = struct

  open! Import_stable

  module Get = struct
    module Action = struct
      module V1 = Unit
      module Model = V1
    end

    module Reaction = struct
      module V1 = struct
        type t = User_name.V1.Set.t
        [@@deriving bin_io, sexp]

        let of_model m = m
      end
      module Model = V1
    end
  end

  module Change_user = struct
    module Action = struct
      module V1 = struct
        type t =
          { user_name : User_name.V1.t
          ; change    : [ `Add | `Remove ]
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

module Get = struct
  module Stable = Stable.Get

  include Iron_versioned_rpc.Make
      (struct let name = "users-using-locked-sessions-get" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)

  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end

module Change_user = struct
  module Stable = Stable.Change_user

  include Iron_versioned_rpc.Make
      (struct let name = "users-using-locked-sessions-change-user" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)

  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end
