module Stable = struct

  open Import_stable

  module Info = struct
    module V5 = struct
      type t =
        { crs_at_tip              : Cr_comment.V1.t list Or_error.V1.t
        ; base_facts              : Rev_facts.V1.t
        ; tip_facts               : Rev_facts.V1.t
        ; base_allow_review_for   : Allow_review_for.V1.t Or_error.V1.t
        ; base_is_ancestor_of_tip : Rev_facts.Is_ancestor.V1.t
        ; diff_from_base_to_tip   : Diff2s.V2.t Or_error.V1.t
        ; diff4s                  : Diff4.V2.t list Or_error.V1.t
        ; cr_soons                : Cr_soons.In_feature.V1.t Or_error.V1.t
        }
      [@@deriving bin_io, sexp]
    end

    module Model = V5
  end

  module Action = struct
    module V8 = struct
      type t =
        { feature_path         : Feature_path.V1.t
        ; feature_id           : Feature_id.V1.t
        ; info                 : Info.V5.t Or_error.V1.t
        ; augment_worker_cache : Worker_cache.From_worker_back_to_server.V3.t
        }
      [@@deriving bin_io, fields, sexp]

      let of_model m = m
      let to_model t = t
    end

    module V5 = struct
      type t =
        { feature_path         : Feature_path.V1.t
        ; feature_id           : Feature_id.V1.t
        ; info                 : Info.V5.t Or_error.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let to_model { feature_path
                   ; feature_id
                   ; info
                   } =
        V8.to_model
          { V8.feature_path
          ; feature_id
          ; info
          ; augment_worker_cache = Worker_cache.From_worker_back_to_server.V3.empty
          }
      ;;

      let of_model m =
        let { V8.
              feature_path
            ; feature_id
            ; info
            ; augment_worker_cache = _
            } = V8.of_model m in
        { feature_path
        ; feature_id
        ; info
        }
      ;;
    end

    module Model = V8
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

open Core.Std
open Import

include Iron_versioned_rpc.Make
    (struct let name = "update-bookmark" end)
    (struct let version = 8 end)
    (Stable.Action.V8)
    (Stable.Reaction.V1)

(* Intent is to keep [5] always, and only the latest version including the worker_cache *)
include Register_old_rpc_converting_both_ways
    (struct let version = 5 end)
    (Stable.Action.V5)
    (Stable.Reaction.V1)

module Info = struct
  include Stable.Info.Model

  module Concise = struct
    type diff4s = Diff4.t list

    let sexp_of_diff4s diff4s =
      (* The cutoff of 5_000 comes from looking at messages.log, in which typical bookmark
         updates are <1_000, and jane/qf-rpaths was >80_000. *)
      diff4s
      |> (if List.length diff4s > 5_000
          then [%sexp_of: _]
          else [%sexp_of: Diff4.t list])
    ;;

    type nonrec t = t =
      { crs_at_tip              : Cr_comment.t list Or_error.t
      ; base_facts              : Rev_facts.t
      ; tip_facts               : Rev_facts.t
      ; base_allow_review_for   : Allow_review_for.t Or_error.t
      ; base_is_ancestor_of_tip : Rev_facts.Is_ancestor.t
      ; diff_from_base_to_tip   : Diff2s.t Or_error.t
      ; diff4s                  : diff4s Or_error.t
      ; cr_soons                : Cr_soons.In_feature.t Or_error.t
      }
    [@@deriving sexp_of]
  end
end

module Action = struct
  include Stable.Action.Model

  module Concise = struct
    type nonrec t = t =
      { feature_path         : Feature_path.t
      ; feature_id           : Feature_id.t
      ; info                 : Info.Concise.t Or_error.t
      ; augment_worker_cache : Worker_cache.From_worker_back_to_server.Concise.t
      }
    [@@deriving sexp_of]
  end
end

module Reaction = Stable.Reaction.Model
