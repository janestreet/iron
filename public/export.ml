(** The intended usage of the [Iron] library is that calling code should not have to open
    anything else than [Iron.Std].  Thus this library needs to export some modules -- at
    the very least the one that are mentioned in RPCs offered by this lib.  This module
    is used to collect those aliases.  It is done in a file here rather than via [std.ml]
    so that RPCs mli can depend on it and mention the types using the same module paths
    that the user should be using. (Example: Iron.User_name). *)
module Iron = struct
  include Iron_common.Std
  include Iron_obligations.Std
  include Iron_hg.Std
  include (struct
    (* non RPC modules from Iron_protocol *)
    open Iron_protocol
    module Feature        = Feature
  end)
end
