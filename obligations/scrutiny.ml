module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module V2 = struct
    module Unshared = struct
      type t =
        { name                            : Scrutiny_name.V1.t
        ; level                           : Scrutiny_level.V1.t
        ; min_file_reviewers              : int
        ; max_file_reviewers              : int
        ; read_by_whole_feature_reviewers : bool
        ; obligations_read_by             : Review_obligation.V1.t
        ; description                     : string
        ; color                           : string option
        }
      [@@deriving bin_io, compare, fields, sexp]

      let module_name = "Iron_obligations.Scrutiny"
      let hash (t : t) = Hashtbl.hash t
    end
    include Unshared
    include Hash_consing.Make_stable_public (Unshared) ()

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| fef28cd19d3e492e935f0730f77c6ed4 |}]
    ;;
  end
  module Model = V2
end

open! Core
open! Import

type t = Stable.Model.t =
  { name                            : Scrutiny_name.t
  ; level                           : Scrutiny_level.t
  ; min_file_reviewers              : int
  ; max_file_reviewers              : int
  ; read_by_whole_feature_reviewers : bool
  ; obligations_read_by             : Review_obligation.t
  ; description                     : string
  ; color                           : string sexp_option
  }
[@@deriving compare, fields, sexp_of]

let shared_t = Stable.Model.shared_t

let for_testing =
  shared_t
    { name                            = Scrutiny_name.of_string "scrutiny"
    ; level                           = Scrutiny_level.ignored
    ; min_file_reviewers              = 1
    ; max_file_reviewers              = 2
    ; read_by_whole_feature_reviewers = true
    ; obligations_read_by             = Review_obligation.none
    ; description                     = "description"
    ; color                           = None
    }
;;

module Syntax = struct
  type t =
    { level                           : Scrutiny_level.Syntax.t
    ; min_file_reviewers              : int  [@default 0 ]
    ; max_file_reviewers              : int  [@default Int.max_value ]
    ; read_by_whole_feature_reviewers : bool [@default true ]
    ; obligations_read_by             : Reviewed_by.t sexp_option
    ; description                     : string
    ; color                           : string sexp_option
    }
  [@@deriving sexp]

  let t_of_sexp sexp =
    let t = t_of_sexp sexp in
    if t.min_file_reviewers < 0
    then of_sexp_error "negative min_file_reviewers is not allowed" sexp;
    if t.max_file_reviewers < 0
    then of_sexp_error "negative max_file_reviewers is not allowed" sexp;
    if t.min_file_reviewers > t.max_file_reviewers
    then of_sexp_error "cannot have min_file_reviewers > max_file_reviewers" sexp;
    t
  ;;
end

let eval name { Syntax.
                level
              ; min_file_reviewers
              ; max_file_reviewers
              ; read_by_whole_feature_reviewers
              ; obligations_read_by
              ; description
              ; color
              }
      error_context ~aliases ~allowed_users ~known_groups =
  let obligations_read_by =
    match obligations_read_by with
    | None -> Review_obligation.none
    | Some reviewed_by ->
      Reviewed_by.eval reviewed_by error_context
        ~aliases ~allowed_users ~known_groups
  in
  shared_t
    { name
    ; level
    ; min_file_reviewers
    ; max_file_reviewers
    ; read_by_whole_feature_reviewers
    ; obligations_read_by
    ; description
    ; color
    }
;;
