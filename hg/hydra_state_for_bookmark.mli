open! Core
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

module Hydra_compilation_status : sig
  type one =
    { finished : Finished_compilation.t option
    ; pending  : Rev_info.t list
    }
  and t = one Repo_controller_name.Map.t
end

module Compilation_status : sig
  module Pending_rev : sig
    type t =
      { first_12_of_rev     : Node_hash.First_12.t
      ; rev_author_or_error : User_name.t Or_error.t
      ; pending_since       : Time.t
      }
    [@@deriving sexp_of]
  end

  (** [pending] is not a rev map to preserve the ordering supplied by Hydra. *)
  type one =
    { finished : Finished_compilation.t option
    ; pending  : Pending_rev.t list
    }
  and t = one Repo_controller_name.Map.t
  [@@deriving sexp_of]

  include Invariant.S with type t := one
  include Equal.S     with type t := one
end

type t =
  { bookmark                  : string
  ; rev_info                  : Rev_info.t
  ; status                    : [ `Done              | `Pending_or_working_on_it ]
  ; continuous_release_status : [ `Not_working_on_it | `Pending_or_working_on_it ]
  ; compilation_status        : Hydra_compilation_status.t
  }
[@@deriving sexp_of]

module Stable : sig
  module Compilation_status : sig
    module V1 : sig
      type t = Compilation_status.t [@@deriving bin_io, sexp]
    end
  end

  module V3 : sig
    type nonrec t = t [@@deriving bin_io, sexp]
  end
  module V2 : sig
    type t [@@deriving bin_io]
    val to_v3 : t -> V3.t
    val of_v3 : V3.t -> t
  end
  module V1 : sig
    type t [@@deriving bin_io]
    val to_v2 : t -> V2.t
    val of_v2 : V2.t -> t
  end
end
