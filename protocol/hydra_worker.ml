module Stable = struct

  open! Import_stable

  module Action = struct
    module V2 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t
        ; tip          : Rev.V1.t option
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 854beac2488a28587f05f39b0c29e82b |}]
      ;;

      let to_model t = t
    end

    module Model = V2

    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 25e91f85b75ca8aeb1afa63bb9ba7795 |}]
      ;;

      let to_model { feature_path; rev_zero } =
        { Model.feature_path; rev_zero; tip = None }
      ;;

      let of_model { Model.
                     feature_path
                   ; rev_zero
                   ; tip = _
                   } =
        { feature_path
        ; rev_zero
        }
      ;;
    end
  end

  module Reaction = struct
    module V8 = struct
      type t =
        { base                             : Rev.V1.t
        ; feature_id                       : Feature_id.V1.t
        ; need_diff4s_starting_from        : (Review_edge.V1.t * User_name.V1.Set.t) list
        ; aliases                          : User_name_by_alternate_name.V1.t
        ; lines_required_to_separate_ddiff_hunks : int
        ; worker_cache                     : Worker_cache.From_server_to_worker.V6.t
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| a7de797edde444ef4b7f47d163c5da6d |}]
      ;;

      let of_model m = m
    end

    module Model = V8

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

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| cca82b512f83b713d1f9f20dadeae847 |}]
      ;;

      let of_model { Model.
                     base
                   ; feature_id
                   ; need_diff4s_starting_from
                   ; aliases
                   ; _
                   } =
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
        { Model.
          base
        ; feature_id
        ; need_diff4s_starting_from
        ; aliases
        ; lines_required_to_separate_ddiff_hunks
          = Constants.lines_required_to_separate_ddiff_hunks_default
        ; worker_cache = Worker_cache.From_server_to_worker.V6.empty
        }
      ;;
    end
  end
end

open! Core
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "hydra-worker" end)
    (struct let version = 8 end)
    (Stable.Action.V2)
    (Stable.Reaction.V8)

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
      { base                             : Rev.t
      ; feature_id                       : Feature_id.t
      ; need_diff4s_starting_from        : (Review_edge.t * User_name.Set.t) list
      ; aliases                          : User_name_by_alternate_name.t
      ; lines_required_to_separate_ddiff_hunks : int
      ; worker_cache                     : Worker_cache.From_server_to_worker.Concise.t
      }
    [@@deriving sexp_of]
  end
end
