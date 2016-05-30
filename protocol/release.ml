module Stable = struct

  open Import_stable

  module Action = struct
    module V4 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t
        ; tagged_tip   : Rev.V1.t option
        ; for_         : User_name.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end

    module Model = V4

    module V3 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t
        ; tagged_tip   : Rev.V1.t option
        ; for_         : User_name.V1.t
        ; may_archive  : bool
        }
      [@@deriving bin_io]

      let to_model { feature_path; rev_zero; tagged_tip; for_; may_archive = _ } =
        V4.to_model { V4. feature_path; rev_zero; tagged_tip; for_; }
      ;;
    end

    module V2 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t
        ; for_         : User_name.V1.t
        ; may_archive  : bool
        }
      [@@deriving bin_io]

      let to_model { feature_path; rev_zero; for_; may_archive } =
        V3.to_model { V3. feature_path; rev_zero; tagged_tip = None; for_; may_archive }
      ;;
    end
  end

  module Reasons_for_not_archiving = struct
    module V1 = struct
      type t = string [@@deriving bin_io, sexp]
    end
  end

  module Disposition = struct
    module V3 = struct
      type t =
        [ `Released_and_archived
        | `Released_and_cleared of Reasons_for_not_archiving.V1.t
        | `Not_released__push_to_hydra
        ]
      [@@deriving bin_io, sexp]

      let of_model m = m
    end

    module Model = V3

    module V2 = struct
      type t =
        [ `Released_and_archived
        | `Released_and_cleared
        | `Not_released__push_to_hydra
        ]
      [@@deriving bin_io]

      let of_v3 : V3.t -> t = function
        | `Released_and_archived | `Not_released__push_to_hydra as x -> x
        | `Released_and_cleared _ -> `Released_and_cleared
      ;;

      let of_model m = of_v3 (V3.of_model m)
    end
  end

  module Reaction = struct
    module V4 = struct
      type t =
        { disposition           : Disposition.V3.t
        ; send_release_email_to : Email_address.V1.Set.t
        }
      [@@deriving bin_io, sexp]

      let of_model m = m
    end

    module Model = V4

    module V3 = struct
      type t =
        { disposition           : Disposition.V2.t
        ; send_release_email_to : Email_address.V1.Set.t
        }
      [@@deriving bin_io]

      let of_model m =
        let { V4. disposition; send_release_email_to } = V4.of_model m in
        { disposition = Disposition.V2.of_model disposition
        ; send_release_email_to
        }
      ;;
    end
  end
end

open! Core.Std
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "release" end)
    (struct let version = 5 end)
    (Stable.Action.V4)
    (Stable.Reaction.V4)

include Register_old_rpc
    (struct let version = 4 end)
    (Stable.Action.V3)
    (Stable.Reaction.V4)

include Register_old_rpc
    (struct let version = 3 end)
    (Stable.Action.V2)
    (Stable.Reaction.V3)

module Action      = Stable.Action.     Model
module Reaction    = Stable.Reaction.   Model
module Disposition = Stable.Disposition.Model

module Reasons_for_not_archiving = struct
  include Stable.Reasons_for_not_archiving.V1

  let create reasons =
    concat ~sep:", "
      (List.map reasons ~f:(function
         | `Feature_is_permanent -> "feature is permanent"
         | `Feature_has_children -> "feature has children"))
  ;;

  let to_string_hum t = t
end
