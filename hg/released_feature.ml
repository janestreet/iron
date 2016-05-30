module Stable = struct
  open! Core.Stable
  open Import_stable

  module Rev = Rev.Stable

  module V3 = struct
    type t =
      { feature_id              : Feature_id.V1.t
      ; feature_path            : Feature_path.V1.t
      ; description             : string
      ; owners                  : User_name.V1.t list
      ; whole_feature_followers : User_name.V1.Set.t
      ; whole_feature_reviewers : User_name.V1.Set.t
      ; seconder                : User_name.V1.t
      ; base                    : Rev.V1.t
      ; tip                     : Rev.V1.t
      ; properties              : Properties.V1.t
      ; includes                : t list
      ; release_cause           : unit Query.V1.t
      }
    [@@deriving bin_io, compare, fields, sexp]

    let of_model m = m
    let to_model m = m
  end

  module Model = V3

  module V2 = struct
    type t =
      { feature_id              : Feature_id.V1.t
      ; feature_path            : Feature_path.V1.t
      ; description             : string
      ; owners                  : User_name.V1.t list
      ; whole_feature_reviewers : User_name.V1.Set.t
      ; seconder                : User_name.V1.t
      ; base                    : Rev.V1.t
      ; tip                     : Rev.V1.t
      ; properties              : Properties.V1.t
      ; includes                : t list
      ; release_cause           : unit Query.V1.t
      }
    [@@deriving sexp, bin_io, compare]

    open! Core.Std
    open! Import

    let rec to_v3
              { feature_id
              ; feature_path
              ; description
              ; owners
              ; whole_feature_reviewers
              ; seconder
              ; base
              ; tip
              ; properties
              ; includes
              ; release_cause
              } =
      { V3.
        feature_id
      ; feature_path
      ; description
      ; owners
      ; whole_feature_followers = User_name.Set.empty
      ; whole_feature_reviewers
      ; seconder
      ; base
      ; tip
      ; properties
      ; includes                = List.map includes ~f:to_v3
      ; release_cause
      }
    ;;

    let to_model t = V3.to_model (to_v3 t)

    let rec of_v3 { V3.
                    feature_id
                  ; feature_path
                  ; description
                  ; owners
                  ; whole_feature_reviewers
                  ; seconder
                  ; base
                  ; tip
                  ; properties
                  ; includes
                  ; release_cause
                  ; _
                  } =
      { feature_id
      ; feature_path
      ; description
      ; owners
      ; whole_feature_reviewers
      ; seconder
      ; base
      ; tip
      ; properties
      ; includes                = List.map includes ~f:of_v3
      ; release_cause
      }
    ;;

    let of_model m = of_v3 (V3.of_model m)

  end

  module V1 = struct
    type t =
      { feature_id              : Feature_id.V1.t
      ; feature_path            : Feature_path.V1.t
      ; description             : string
      ; owners                  : User_name.V1.t list
      ; whole_feature_reviewers : User_name.V1.Set.t
      ; seconder                : User_name.V1.t
      ; base                    : Rev.V1.t
      ; tip                     : Rev.V1.t
      ; includes                : t list
      ; release_cause           : unit Query.V1.t
      }
    [@@deriving sexp, bin_io, compare]

    open! Core.Std
    open! Import

    let rec to_v2
              { feature_id
              ; feature_path
              ; description
              ; owners
              ; whole_feature_reviewers
              ; seconder
              ; base
              ; tip
              ; includes
              ; release_cause
              } =
      { V2.
        feature_id
      ; feature_path
      ; description
      ; owners
      ; whole_feature_reviewers
      ; seconder
      ; base
      ; tip
      ; properties              = Properties.create ()
      ; includes                = List.map includes ~f:to_v2
      ; release_cause
      }
    ;;

    let to_model t = V2.to_model (to_v2 t)

    let rec of_v2 { V2.
                    feature_id
                  ; feature_path
                  ; description
                  ; owners
                  ; whole_feature_reviewers
                  ; seconder
                  ; base
                  ; tip
                  ; includes
                  ; release_cause
                  ; _
                  } =
      { feature_id
      ; feature_path
      ; description
      ; owners
      ; whole_feature_reviewers
      ; seconder
      ; base
      ; tip
      ; includes                = List.map includes ~f:of_v2
      ; release_cause
      }
    ;;

    let of_model m = of_v2 (V2.of_model m)

  end
end

open! Core.Std
open! Async.Std
open! Import

include Stable.Model

let rec invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~feature_id:(check Feature_id.invariant)
      ~feature_path:(check Feature_path.invariant)
      ~description:ignore
      ~owners:(check (List.iter ~f:User_name.invariant))
      ~whole_feature_followers:(check (Set.iter ~f:User_name.invariant))
      ~whole_feature_reviewers:(check (Set.iter ~f:User_name.invariant))
      ~seconder:(check User_name.invariant)
      ~base:(check Rev.invariant)
      ~tip:(check Rev.invariant)
      ~properties:(check Properties.invariant)
      ~includes:(check (List.iter ~f:invariant))
      ~release_cause:(check (Query.invariant (ignore:unit->unit)))
  )
;;

let released_at t = Query.at t.release_cause
let released_by t = Query.by t.release_cause

let user_names_to_string user_names =
  concat ~sep:", " (List.map user_names ~f:User_name.to_string)
;;

let user_list name users =
  match users with
  | [ user ] -> (name, User_name.to_string user)
  | users -> (concat [ name; "s"], user_names_to_string users)
;;

let attribute_table t =
  let rows =
    [ "id", Feature_id.to_string t.feature_id
    ; user_list "owner" t.owners
    ; user_list "whole-feature reviewer" (Set.to_list t.whole_feature_reviewers)
    ; "seconder", User_name.to_string t.seconder
    ; "tip" , Rev.to_string_hum t.tip
    ; "base", Rev.to_string_hum t.base
    ]
    @ Properties.to_rows t.properties
  in
  let columns =
    Ascii_table.Column.(
      [ string ~header:"attribute" (cell fst)
      ; string ~header:"value"     (cell snd)
      ])
  in
  Ascii_table.create ~columns ~rows
;;
