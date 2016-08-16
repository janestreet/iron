open! Core.Std.No_polymorphic_compare

module Stable = struct

  open! Import_stable

  module Rev_info = struct
    module V1 = struct
      type t =
        { first_12_of_rev     : Node_hash.First_12.V1.t
        ; rev_author_or_error : User_name.V1.t Or_error.V2.t
        }
      [@@deriving bin_io, sexp]
    end
    module Model = V1
  end

  module Broken_info = struct
    module V1 = struct
      type t =
        { last_working : Rev_info.V1.t option
        ; first_broken : Rev_info.V1.t
        ; last_broken  : Rev_info.V1.t
        }
      [@@deriving bin_io, sexp]
    end
    module Model = V1
  end

  module Finished_compilation = struct
    module V1 = struct
      type t =
        | Working of Rev_info.V1.t
        | Broken  of Broken_info.V1.t
      [@@deriving bin_io, sexp]
    end
    module Model = V1
  end

  module Compilation_status = struct
    module V1 = struct
      type t =
        { finished : Finished_compilation.V1.t option
        ; pending  : Rev_info.V1.t list
        }
      [@@deriving bin_io, sexp]
    end
    module Model = V1
  end

  module V2 = struct
    type t =
      { bookmark                  : string
      ; rev_info                  : Rev_info.V1.t
      ; status                    : [ `Done              | `Pending_or_working_on_it ]
      ; continuous_release_status : [ `Not_working_on_it | `Pending_or_working_on_it ]
      ; compilation_status        : (string * Compilation_status.V1.t) list
      }
    [@@deriving bin_io, sexp]
  end

  module V1 = struct
    type t =
      { bookmark            : string
      ; first_12_of_rev     : Node_hash.First_12.V1.t
      ; rev_author_or_error : User_name.V1.t Or_error.V1.t
      ; status              : [ `Done | `Pending_or_working_on_it ]
      }
    [@@deriving bin_io]

    let to_v2 { bookmark
              ; first_12_of_rev
              ; rev_author_or_error
              ; status
              } =
      { V2.
        bookmark
      ; rev_info                  = { first_12_of_rev; rev_author_or_error }
      ; status
      ; continuous_release_status = `Not_working_on_it
      ; compilation_status        = []
      }
    ;;

    let of_v2 { V2.
                bookmark
              ; rev_info = { first_12_of_rev; rev_author_or_error }
              ; status
              ; continuous_release_status = _
              ; compilation_status        = _
              } =
      { bookmark
      ; first_12_of_rev
      ; rev_author_or_error
      ; status
      }
    ;;
  end

  module Model = V2
end

open! Core.Std
open! Import

module Broken_info          = Stable.Broken_info          .Model
module Compilation_status   = Stable.Compilation_status   .Model
module Finished_compilation = Stable.Finished_compilation .Model
module Rev_info             = Stable.Rev_info             .Model

include Stable.Model
