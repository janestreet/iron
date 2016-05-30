module Stable = struct
  open! Core.Stable
  open! Import_stable

  module Value = struct
    module V1 = struct
      module Unshared = struct
        type t =
          { min_file_reviewers              : int
          ; max_file_reviewers              : int
          ; read_by_whole_feature_reviewers : bool
          ; level                           : Scrutiny_level.V1.t
          ; description                     : string
          ; color                           : string option
          }
        [@@deriving bin_io, compare, fields, sexp]

        let module_name = "Iron_obligations.Scrutiny_value"
        let hash (t : t) = Hashtbl.hash t
      end
      include Unshared
      include Hash_consing.Make_stable_public (Unshared) ()
    end

    module Model = V1
  end

  module V1 = struct
    module Unshared = struct
      type t =
        { name  : Scrutiny_name.V1.t
        ; value : Value.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let module_name = "Iron_obligations.Scrutiny"
      let hash (t : t) = Hashtbl.hash t
    end
    include Unshared
    include Hash_consing.Make_stable_public (Unshared) ()
  end

  module Model = V1

end

open! Core.Std
open! Import

module Syntax = struct
  type t = Stable.Value.Model.t =
    { min_file_reviewers              : int  [@default 0 ]
    ; max_file_reviewers              : int  [@default Int.max_value ]
    ; read_by_whole_feature_reviewers : bool [@default true ]
    ; level                           : Scrutiny_level.Syntax.t
    ; description                     : string
    ; color                           : string sexp_option
    }
  [@@deriving compare, sexp]

  let shared_t = Stable.Value.Model.shared_t

  let t_of_sexp sexp =
    let t = t_of_sexp sexp in
    if t.min_file_reviewers < 0
    then of_sexp_error "negative min_file_reviewers is not allowed" sexp;
    if t.max_file_reviewers < 0
    then of_sexp_error "negative max_file_reviewers is not allowed" sexp;
    if t.min_file_reviewers > t.max_file_reviewers
    then of_sexp_error "cannot have min_file_reviewers > max_file_reviewers" sexp;
    shared_t t
  ;;

  let for_testing =
    shared_t
      { min_file_reviewers              = 1
      ; max_file_reviewers              = 2
      ; read_by_whole_feature_reviewers = true
      ; level                           = Scrutiny_level.ignored
      ; description                     = "description"
      ; color                           = None
      }
  ;;
end

type t = Stable.Model.t =
  { name  : Scrutiny_name.t
  ; value : Syntax.t
  }
[@@deriving compare, fields, sexp_of]

let shared_t = Stable.Model.shared_t

let create name value = shared_t { name; value }

let for_testing =
  shared_t
    { name  = Scrutiny_name.of_string "scrutiny"
    ; value = Syntax.for_testing
    }
;;
