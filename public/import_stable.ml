include (Core.Stable : module type of Core.Stable
         with module String := Core.Stable.String
         with module Time   := Core.Stable.Time
         with module Uuid   := Core.Stable.Uuid)
include Iron_common.Stable
include Iron_hg.Stable

module Fn = Core.Std.Fn
module List = Core.Std.List

module Unit = struct
  type t = unit
  [@@deriving bin_io, sexp]

  let of_model t = t
  let to_model t = t
end
