open! Core.Std
open! Import

module Rev_info : sig
  type t =
    { first_12_of_rev     : Node_hash.First_12.t
    ; rev_author_or_error : User_name.t Or_error.t
    }
end

module Broken_info : sig
  type t =
    { last_working : Rev_info.t option
    ; first_broken : Rev_info.t
    ; last_broken  : Rev_info.t
    }
end

module Finished_compilation : sig
  type t =
    | Working of Rev_info.t
    | Broken  of Broken_info.t
end

module Compilation_status : sig
  type t =
    { finished : Finished_compilation.t option
    ; pending  : Rev_info.t list
    }
end

type t =
  { bookmark                  : string
  ; rev_info                  : Rev_info.t
  ; status                    : [ `Done              | `Pending_or_working_on_it ]
  ; continuous_release_status : [ `Not_working_on_it | `Pending_or_working_on_it ]
  ; compilation_status        : (string * Compilation_status.t) list
  }

module Stable : sig
  module V2 : sig
    type nonrec t = t [@@deriving bin_io, sexp]
  end
  module V1 : sig
    type t [@@deriving bin_io]
    val to_v2 : t -> V2.t
    val of_v2 : V2.t -> t
  end
end
