module Stable = struct

  open! Import_stable

  module V2 = struct
    type t =
      [ `Admins
      | `Feeding_metrics
      | `Using_locked_sessions
      ]
    [@@deriving bin_io, enumerate, sexp]
  end

  module V1 = struct
    type t =
      | Admins
      | Using_locked_sessions
    [@@deriving bin_io]

    let to_v2 = function
      | Admins                -> `Admins
      | Using_locked_sessions -> `Using_locked_sessions
    ;;
  end

  module Model = V2

  module Get = struct
    module Action = struct
      module V2 = struct
        type t = V2.t
        [@@deriving bin_io, sexp]

        let to_model m = m
      end

      module V1 = struct
        type t = V1.t
        [@@deriving bin_io]

        let to_model t = V2.to_model (V1.to_v2 t)
      end

      module Model = V2
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
      module V2 = struct
        type t =
          { user_set   : V2.t
          ; user_names : User_name.V1.Set.t
          ; change     : [ `Add | `Remove ]
          ; idempotent : bool
          }
        [@@deriving bin_io, fields, sexp]

        let to_model m = m
      end

      module V1 = struct
        type t =
          { user_set   : V1.t
          ; user_names : User_name.V1.Set.t
          ; change     : [ `Add | `Remove ]
          ; idempotent : bool
          }
        [@@deriving bin_io]

        let to_model { user_set
                     ; user_names
                     ; change
                     ; idempotent
                     } =
          V2.to_model
            { V2.
              user_set   = V1.to_v2 user_set
            ; user_names
            ; change
            ; idempotent
            }
        ;;
      end

      module Model = V2
    end

    module Reaction = struct
      module V1 = Unit
      module Model = V1
    end
  end
end

open! Core.Std
open! Import

include Stable.Model

module Get = struct
  module Stable = Stable.Get

  include Iron_versioned_rpc.Make
      (struct let name = "user-set-get" end)
      (struct let version = 2 end)
      (Stable.Action.V2)
      (Stable.Reaction.V1)

  include Register_old_rpc
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)

  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end

module Change = struct
  module Stable = Stable.Change_user

  include Iron_versioned_rpc.Make
      (struct let name = "user-set-change" end)
      (struct let version = 2 end)
      (Stable.Action.V2)
      (Stable.Reaction.V1)

  include Register_old_rpc
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)

  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end
