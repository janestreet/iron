open! Core
open! Import

include Iron_uuid_intf.S

val check : actual:t -> supplied:t -> unit Or_error.t

val no_session_error : supplied:t -> Error.t
