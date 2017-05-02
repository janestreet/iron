module Stable = struct

  open! Import_stable

  module Feature = Feature.Stable

  module By_id = struct
    module Action = struct
      module V2 = struct
        type t =
          { feature_id       : Feature_id.V1.t
          ; even_if_archived : bool
          }
        [@@deriving bin_io, fields, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| f7ab3e222aa7d24abb573b430bdc14ae |}]
        ;;

        let to_model t = t
      end

      module V1 = struct
        type t = Feature_id.V1.t
        [@@deriving bin_io]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
        ;;

        let to_model feature_id =
          V2.to_model
            { V2.
              feature_id
            ; even_if_archived = false
            }
        ;;
      end

      module Model = V2
    end

    module Reaction = Feature
  end

  module Maybe_archived = struct
    module Action = struct
      module V3 = struct
        type t =
          { what_feature : Maybe_archived_feature_spec.V3.t
          ; what_diff    : What_diff.V2.t
          }
        [@@deriving bin_io, compare, fields, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| fab5a2cdaf75e3324c227147f7ba2644 |}]
        ;;

        let to_model (t : t) = t
      end

      module V2 = struct
        type t =
          { what_feature : Maybe_archived_feature_spec.V2.t
          ; what_diff    : What_diff.V2.t
          }
        [@@deriving bin_io]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| ae25b80d385e7de37d53926d609be34e |}]
        ;;

        let to_model { what_feature
                     ; what_diff
                     } =
          V3.to_model
            { V3.
              what_feature = Maybe_archived_feature_spec.V2.to_v3 what_feature
            ; what_diff
            }
        ;;
      end

      module V1 = struct
        type t =
          { what_feature : Maybe_archived_feature_spec.V1.t
          ; what_diff    : What_diff.V2.t
          }
        [@@deriving bin_io]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 91def6327f48a5796145b47efb6e5eda |}]
        ;;

        let to_model { what_feature
                     ; what_diff
                     } =
          V2.to_model
            { V2.
              what_feature = Maybe_archived_feature_spec.V1.to_v2 what_feature
            ; what_diff
            }
        ;;
      end

      module Model = V3
    end

    module Reaction = Feature
  end

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t option
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b2b386963682bcb97c2bb87f82e9e4e4 |}]
      ;;

      let to_model t = t
    end
    module Model = V1
  end

  module Reaction = Feature
end

open! Core
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "get-feature" end)
    (struct let version = 23 end)
    (Stable.Action.V1)
    (Stable.Reaction.V23)

include Register_old_rpc
    (struct let version = 22 end)
    (Stable.Action.V1)
    (Stable.Reaction.V22)

include Register_old_rpc
    (struct let version = 21 end)
    (Stable.Action.V1)
    (Stable.Reaction.V21)

include Register_old_rpc
    (struct let version = 20 end)
    (Stable.Action.V1)
    (Stable.Reaction.V20)

include Register_old_rpc
    (struct let version = 19 end)
    (Stable.Action.V1)
    (Stable.Reaction.V19)

include Register_old_rpc
    (struct let version = 18 end)
    (Stable.Action.V1)
    (Stable.Reaction.V18)

include Register_old_rpc
    (struct let version = 17 end)
    (Stable.Action.V1)
    (Stable.Reaction.V17)

include Register_old_rpc
    (struct let version = 16 end)
    (Stable.Action.V1)
    (Stable.Reaction.V16)

include Register_old_rpc
    (struct let version = 15 end)
    (Stable.Action.V1)
    (Stable.Reaction.V15)

include Register_old_rpc
    (struct let version = 14 end)
    (Stable.Action.V1)
    (Stable.Reaction.V14)

include Register_old_rpc
    (struct let version = 13 end)
    (Stable.Action.V1)
    (Stable.Reaction.V13)

include Register_old_rpc
    (struct let version = 12 end)
    (Stable.Action.V1)
    (Stable.Reaction.V12)

include Register_old_rpc
    (struct let version = 11 end)
    (Stable.Action.V1)
    (Stable.Reaction.V11)

include Register_old_rpc
    (struct let version = 10 end)
    (Stable.Action.V1)
    (Stable.Reaction.V10)

module Action    = Stable.Action.Model
module Reaction  = Feature

module By_id = struct
  include Iron_versioned_rpc.Make
      (struct let name = "get-feature-by-id" end)
      (struct let version = 23 end)
      (Stable.By_id.Action.V2)
      (Stable.By_id.Reaction.V23)

  include Register_old_rpc
      (struct let version = 22 end)
      (Stable.By_id.Action.V2)
      (Stable.By_id.Reaction.V22)

  include Register_old_rpc
      (struct let version = 21 end)
      (Stable.By_id.Action.V2)
      (Stable.By_id.Reaction.V21)

  include Register_old_rpc
      (struct let version = 20 end)
      (Stable.By_id.Action.V2)
      (Stable.By_id.Reaction.V20)

  include Register_old_rpc
      (struct let version = 19 end)
      (Stable.By_id.Action.V2)
      (Stable.By_id.Reaction.V19)

  include Register_old_rpc
      (struct let version = 18 end)
      (Stable.By_id.Action.V2)
      (Stable.By_id.Reaction.V18)

  include Register_old_rpc
      (struct let version = 17 end)
      (Stable.By_id.Action.V2)
      (Stable.By_id.Reaction.V17)

  include Register_old_rpc
      (struct let version = 16 end)
      (Stable.By_id.Action.V2)
      (Stable.By_id.Reaction.V16)

  include Register_old_rpc
      (struct let version = 15 end)
      (Stable.By_id.Action.V2)
      (Stable.By_id.Reaction.V15)

  include Register_old_rpc
      (struct let version = 14 end)
      (Stable.By_id.Action.V1)
      (Stable.By_id.Reaction.V14)

  include Register_old_rpc
      (struct let version = 13 end)
      (Stable.By_id.Action.V1)
      (Stable.By_id.Reaction.V13)

  module Action    = Stable.By_id.Action.Model
  module Reaction  = Feature
end

module Maybe_archived = struct
  include Iron_versioned_rpc.Make
      (struct let name = "get-feature-maybe-archived" end)
      (struct let version = 11 end)
      (Stable.Maybe_archived.Action.V3)
      (Stable.Maybe_archived.Reaction.V23)

  include Register_old_rpc
      (struct let version = 10 end)
      (Stable.Maybe_archived.Action.V3)
      (Stable.Maybe_archived.Reaction.V22)

  include Register_old_rpc
      (struct let version = 9 end)
      (Stable.Maybe_archived.Action.V3)
      (Stable.Maybe_archived.Reaction.V21)

  include Register_old_rpc
      (struct let version = 8 end)
      (Stable.Maybe_archived.Action.V3)
      (Stable.Maybe_archived.Reaction.V20)

  include Register_old_rpc
      (struct let version = 7 end)
      (Stable.Maybe_archived.Action.V2)
      (Stable.Maybe_archived.Reaction.V20)

  include Register_old_rpc
      (struct let version = 6 end)
      (Stable.Maybe_archived.Action.V2)
      (Stable.Maybe_archived.Reaction.V19)

  include Register_old_rpc
      (struct let version = 5 end)
      (Stable.Maybe_archived.Action.V2)
      (Stable.Maybe_archived.Reaction.V18)

  include Register_old_rpc
      (struct let version = 4 end)
      (Stable.Maybe_archived.Action.V2)
      (Stable.Maybe_archived.Reaction.V17)

  include Register_old_rpc
      (struct let version = 3 end)
      (Stable.Maybe_archived.Action.V1)
      (Stable.Maybe_archived.Reaction.V17)

  include Register_old_rpc
      (struct let version = 2 end)
      (Stable.Maybe_archived.Action.V1)
      (Stable.Maybe_archived.Reaction.V16)

  include Register_old_rpc
      (struct let version = 1 end)
      (Stable.Maybe_archived.Action.V1)
      (Stable.Maybe_archived.Reaction.V15)

  module Action    = Stable.Maybe_archived.Action.Model
  module Reaction  = Feature
end
