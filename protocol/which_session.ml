module Stable = struct

  open Import_stable

  module V1 = struct
    type t =
      | Current_session
      | This_session    of Session_id.V1.t
    [@@deriving bin_io, compare, sexp]
  end
end

include Stable.V1
