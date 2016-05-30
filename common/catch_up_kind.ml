module Stable = struct
  open Core.Stable
  module User_name = User_name.Stable
  module Reviewed_by_someone_else = struct
    module V1 = struct
      type t =
        { reviewed_by : User_name.V1.t
        ; reason      : string
        }
      [@@deriving bin_io, compare, fields, sexp]
    end
    module Model = V1
  end
  module V1 = struct
    type t =
      | Create_catch_up_for_me
      | Follower
      | Unfinished_review
      | Reviewed_by_someone_else of Reviewed_by_someone_else.V1.t
    [@@deriving bin_io, compare, sexp]
    let to_model m = m
    let of_model m = m
  end
  module Model = V1
end

open! Core.Std
open! Import

module Reviewed_by_someone_else = struct
  include Stable.Reviewed_by_someone_else.Model

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~reviewed_by:(check User_name.invariant)
        ~reason:(check ignore))
  ;;
end

include Stable.Model

let invariant = function
  | Create_catch_up_for_me
  | Follower
  | Unfinished_review -> ()
  | Reviewed_by_someone_else reviewed -> Reviewed_by_someone_else.invariant reviewed
;;

let to_string_hum = function
  | Create_catch_up_for_me ->
    concat [ "You provided " ; Switch.create_catch_up_for_me ; " during review." ]
  | Follower -> "The feature was released and you were a follower."
  | Unfinished_review ->
    "The feature was released and you had partial review done on these files."
  | Reviewed_by_someone_else { reviewed_by ; reason } ->
    concat [ User_name.to_string reviewed_by
           ; " reviewed this for you, giving the reason as:\n"
           ; String.strip reason
           ]
;;
