module Stable = struct

  open Import_stable

  module Action = struct
    module V3 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; for_         : User_name.V1.t
        ; lock_names   : Lock_name.V2.t list
        ; reason       : string
        ; is_permanent : bool
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end

    module V2 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; for_         : User_name.V1.t
        ; lock_names   : Lock_name.V1.t list
        ; reason       : string
        ; is_permanent : bool
        }
      [@@deriving bin_io]

      let to_model { feature_path
                   ; for_
                   ; lock_names
                   ; reason
                   ; is_permanent
                   } =
        V3.to_model
          { V3.
            feature_path
          ; for_
          ; lock_names = List.map lock_names ~f:Lock_name.V1.to_v2
          ; reason
          ; is_permanent
          }
      ;;
    end

    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; for_         : User_name.V1.t
        ; lock_names   : Lock_name.V1.t list
        ; reason       : string
        }
      [@@deriving bin_io]

      let to_model { feature_path
                   ; for_
                   ; lock_names
                   ; reason
                   } =
        V2.to_model
          { V2.
            feature_path
          ; for_
          ; lock_names
          ; reason
          ; is_permanent = false
          }
      ;;
    end

    module Model = V3
  end

  module Reaction = struct
    module V2 = struct
      type t = (Lock_name.V2.t * unit Or_error.V1.t) list
      [@@deriving bin_io, sexp]

      let of_model t = t
    end

    module V1 = struct
      type t = (Lock_name.V1.t * unit Or_error.V1.t) list
      [@@deriving bin_io]

      open! Core.Std
      open! Import

      let of_model m =
        List.map (V2.of_model m) ~f:(fun (lock_name, result) ->
          let lock_name =
            match Lock_name.Stable.V1.of_v2 lock_name with
            | Some lock_name -> lock_name
            | None -> assert false (* reaction inconsistent with action *)
          in
          lock_name, result)
      ;;
    end

    module Model = V2
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "lock-feature" end)
    (struct let version = 3 end)
    (Stable.Action.V3)
    (Stable.Reaction.V2)

include Register_old_rpc
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
