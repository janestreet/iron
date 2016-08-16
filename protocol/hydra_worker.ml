module Stable = struct

  open Import_stable

  module Action = struct
    module V2 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t
        ; tip          : Rev.V1.t option
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
      let of_model m = m
    end

    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let to_model { feature_path; rev_zero } =
        V2.to_model { V2.feature_path; rev_zero; tip = None }
      ;;

      let of_model m =
        let { V2.
              feature_path
            ; rev_zero
            ; tip = _
            } = V2.of_model m in
        { feature_path
        ; rev_zero
        }
      ;;
    end

    module Model = V2
  end

  module Reaction = struct
    module V6 = struct
      type t =
        { base                       : Rev.V1.t
        ; feature_id                 : Feature_id.V1.t
        ; need_diff4s_starting_from  : (Review_edge.V1.t * User_name.V1.Set.t) list
        ; aliases                    : User_name_by_alternate_name.V1.t
        ; worker_cache               : Worker_cache.From_server_to_worker.V5.t
        }
      [@@deriving bin_io, sexp]

      let to_model (m : t) = m
      let of_model t = t
    end

    (* Note about version scheme: we expect to keep V1 always as a way to preserve a
       common version for roll transitions.  We do not implement projection between
       different versions of the cache: keep V1 <-> Vn, delete V(n-1) *)
    module V1 = struct
      type t =
        { base                       : Rev.V1.t
        ; feature_id                 : Feature_id.V1.t
        ; need_diff4s_starting_from  : (Review_edge.V1.t * User_name.V1.Set.t) list
        ; aliases                    : User_name_by_alternate_name.V1.t
        }
      [@@deriving bin_io, sexp]

      let of_model m =
        let { V6.
              base
            ; feature_id
            ; need_diff4s_starting_from
            ; aliases
            ; _
            } = V6.of_model m in
        { base
        ; feature_id
        ; need_diff4s_starting_from
        ; aliases
        }
      ;;

      let to_model { base
                   ; feature_id
                   ; need_diff4s_starting_from
                   ; aliases
                   } =
        V6.to_model
          { base
          ; feature_id
          ; need_diff4s_starting_from
          ; aliases
          ; worker_cache = Worker_cache.From_server_to_worker.V5.empty
          }
      ;;
    end

    module Model = V6
  end
end

open! Core.Std
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "hydra-worker" end)
    (struct let version = 6 end)
    (Stable.Action.V2)
    (Stable.Reaction.V6)

(* Intent is to keep [1] always, and only the latest version including the worker cache *)
include Register_old_rpc_converting_both_ways
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.  Model

module Reaction = struct
  include Stable.Reaction.Model

  module Concise = struct
    type nonrec t = t =
      { base                       : Rev.t
      ; feature_id                 : Feature_id.t
      ; need_diff4s_starting_from  : (Review_edge.t * User_name.Set.t) list
      ; aliases                    : User_name_by_alternate_name.t
      ; worker_cache               : Worker_cache.From_server_to_worker.Concise.t
      }
    [@@deriving sexp_of]
  end
end
