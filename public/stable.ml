include Iron_common.Stable
include Iron_obligations.Stable
include Iron_hg.Stable
include (struct
  (* non RPC modules from Iron_protocol *)
  open Iron_protocol
  module Feature          = Feature          .Stable
  module Which_features   = Which_features   .Stable
end)
