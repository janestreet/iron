module Stable = struct

  open! Import_stable

  module Which_obligations = struct
    module V1 = struct
      type t =
        { repo_root          : Abspath.V1.t
        ; file_tree_of       : [ `Working_copy | `Rev of Raw_rev.V1.t ]
        ; aliases_resolution : [ `None
                               | `Using_latest_aliases_from_iron_server
                               ]
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 2c6d0604c1b6f76ca46f0193d884c0b6 |}]
      ;;

      let to_model t = t
    end

    module Model = V1
  end

  module List_users = struct
    module Action = struct
      module V1 = Which_obligations.V1
      module Model = V1
    end
    module Reaction = struct
      module V1 = struct
        type t = Unresolved_name.V1.Set.t
        [@@deriving bin_io, sexp_of]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 050cfc58de0961a044c2d045ebb6936c |}]
        ;;

        let of_model t = t
      end
      module Model = V1
    end
  end

  module List_groups = struct
    module Action = struct
      module V1 = Which_obligations.V1
      module Model = V1
    end
    module Reaction = struct
      module V1 = struct
        type t = Unresolved_name.V1.Set.t Group_name.V1.Map.t
        [@@deriving bin_io, sexp_of]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 784896b81c01cfe4fee3ea9dfd807fdf |}]
        ;;

        let of_model t = t
      end
      module Model = V1
    end
  end
end

open! Core
open! Import

module Which_obligations = Stable.Which_obligations.Model

module List_users = struct
  module Stable = Stable.List_users

  include Iron_command_rpc.Make
      (struct let name = "obligations-list-users" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)

  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end

module List_groups = struct
  module Stable = Stable.List_groups

  include Iron_command_rpc.Make
      (struct let name = "obligations-list-groups" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)

  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end
