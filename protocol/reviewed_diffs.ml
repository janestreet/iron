module Stable = struct

  open Import_stable

  module Action = struct
    module V3 = struct
      type t =
        { feature_path           : Feature_path.V1.t
        ; for_                   : User_name.V1.t
        ; reason                 : string
        ; create_catch_up_for_me : bool
        ; review_session_id      : Session_id.V1.t
        ; diff4_in_session_ids   : Diff4_in_session.Id.V1.t list
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end

    module Model = V3

    module V2 = struct
      type t =
        { feature_path         : Feature_path.V1.t
        ; for_                 : User_name.V1.t
        ; reason               : string
        ; review_session_id    : Session_id.V1.t
        ; diff4_in_session_ids : Diff4_in_session.Id.V1.t list
        }
      [@@deriving bin_io]

      let to_model { feature_path
                   ; for_
                   ; reason
                   ; review_session_id
                   ; diff4_in_session_ids
                   } =
        V3.to_model
          { V3.
            feature_path
          ; for_
          ; reason
          ; create_catch_up_for_me = false
          ; review_session_id
          ; diff4_in_session_ids
          }
    end
  end

  module Reaction = struct
    module V1 = Unit
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "reviewed-diff4" end)
    (struct let version = 3 end)
    (Stable.Action.V3)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

module Action   = Stable.Action.  V3
module Reaction = Stable.Reaction.V1
