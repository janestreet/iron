module Stable = struct

  open Import_stable

  module Kind = struct
    module V1 = struct
      type t = Gc_stat | Gc_quick_stat | Process_times
      [@@deriving bin_io, enumerate, sexp]
    end
  end

  module Action = struct
    module V1 = struct
      type t =
        { kind : Kind.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = struct
      type t = Sexp.t
      [@@deriving bin_io, sexp]

      let of_model t = t
    end
  end

end

open! Core.Std
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "stat" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

include Register_map_reaction_in_client (struct
    let of_server_reaction (_ : action) (reaction : reaction) =
      let rec rewrite_ints = function
        | Sexp.List sexps -> Sexp.List (List.map sexps ~f:rewrite_ints)
        | Sexp.Atom str as atom ->
          match Int.of_string str with
          | exception _ -> atom
          | int         -> Sexp.Atom (Int.to_string_hum int)
      in
      rewrite_ints reaction
    ;;
  end)

module Action   = Stable.Action.V1
module Kind     = Stable.Kind.V1
module Reaction = Stable.Reaction.V1
