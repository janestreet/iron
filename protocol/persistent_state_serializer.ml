module Stable = struct

  open! Import_stable

  module Pause = struct
    module Action = struct
      module V1 = struct
        type t =
          { with_timeout : Span.V1.t
          }
        [@@deriving bin_io, fields, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 6dccc8739200b80a3912ebdb42428fdf |}]
        ;;

        let to_model t = t
      end
      module Model = V1
    end

    module Reaction = struct
      module V1 = Unit
      module Model = V1
    end
  end

  module Prior_changes_synced_to_file_system = struct
    module Action = struct
      module V1 = Unit
      module Model = V1
    end

    module Reaction = struct
      module V1 = Unit
      module Model = V1
    end
  end

  module Resume = struct
    module Action = struct
      module V1 = Unit
      module Model = V1
    end

    module Reaction = struct
      module V1 = Unit
      module Model = V1
    end
  end

  module Status = struct
    module Action = struct
      module V1 = Unit
      module Model = V1
    end

    module Reaction = struct
      module V1 = struct
        type t = Sexp.t
        [@@deriving bin_io, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 832b40ae394f2851da8ba67b3339b429 |}]
        ;;

        let of_model t = t
      end
      module Model = V1
    end
  end
end

module Pause = struct
  module Stable = Stable.Pause
  include Iron_versioned_rpc.Make
      (struct let name = "pause-serializer" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)
  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end

module Prior_changes_synced_to_file_system = struct
  module Stable = Stable.Prior_changes_synced_to_file_system
  include Iron_versioned_rpc.Make
      (struct let name = "prior-changes-synced-to-file-system" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)
  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end

module Resume = struct
  module Stable = Stable.Resume
  include Iron_versioned_rpc.Make
      (struct let name = "resume-serializer" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)
  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end

module Status = struct
  module Stable = Stable.Status
  include Iron_versioned_rpc.Make
      (struct let name = "serializer-status" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)
  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end
