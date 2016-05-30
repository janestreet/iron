module Stable = struct

  open Import_stable

  module Pause = struct
    module Action = struct
      module V1 = struct
        type t =
          { with_timeout : Span.V1.t
          }
        [@@deriving bin_io, fields, sexp]

        let to_model t = t
      end
    end

    module Reaction = struct
      module V1 = Unit
    end
  end

  module Prior_changes_synced_to_file_system = struct
    module Action = struct
      module V1 = Unit
    end

    module Reaction = struct
      module V1 = Unit
    end
  end

  module Resume = struct
    module Action = struct
      module V1 = Unit
    end

    module Reaction = struct
      module V1 = Unit
    end
  end

  module Status = struct
    module Action = struct
      module V1 = Unit
    end

    module Reaction = struct
      module V1 = struct
        type t = Sexp.t
        [@@deriving bin_io, sexp]

        let of_model t = t
      end
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
  module Action   = Stable.Action.V1
  module Reaction = Stable.Reaction.V1
end

module Prior_changes_synced_to_file_system = struct
  module Stable = Stable.Prior_changes_synced_to_file_system
  include Iron_versioned_rpc.Make
      (struct let name = "prior-changes-synced-to-file-system" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)
  module Action   = Stable.Action.V1
  module Reaction = Stable.Reaction.V1
end

module Resume = struct
  module Stable = Stable.Resume
  include Iron_versioned_rpc.Make
      (struct let name = "resume-serializer" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)
  module Action   = Stable.Action.V1
  module Reaction = Stable.Reaction.V1
end

module Status = struct
  module Stable = Stable.Status
  include Iron_versioned_rpc.Make
      (struct let name = "serializer-status" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)
  module Action   = Stable.Action.V1
  module Reaction = Stable.Reaction.V1
end
