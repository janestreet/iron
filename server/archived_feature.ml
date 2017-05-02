open! Core
open! Import

type t =
  { feature_id   : Feature_id.t
  ; feature_path : Feature_path.t
  ; rev_zero     : Rev.t
  ; owners       : User_name.t list
  ; archived_at  : Time.t
  ; reason_for_archiving : string
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~feature_id:(check Feature_id.invariant)
      ~feature_path:(check Feature_path.invariant)
      ~rev_zero:(check Rev.invariant)
      ~owners:(check (fun owners -> List.iter owners ~f:User_name.invariant))
      ~archived_at:ignore
      ~reason_for_archiving:ignore)
;;

let create feature ~archived_at ~reason_for_archiving =
  { feature_id   = Feature.feature_id feature
  ; feature_path = Feature.feature_path feature
  ; rev_zero     = Feature.rev_zero feature
  ; owners       = Feature.owners feature
  ; archived_at
  ; reason_for_archiving
  }
;;

let to_list_protocol { feature_id
                     ; feature_path
                     ; owners
                     ; archived_at
                     ; reason_for_archiving
                     ; _
                     } =
  { Iron_protocol.List_features.Reaction.
    feature_path
  ; feature_id
  ; owners
  ; review_is_enabled = false
  ; num_lines         = Known (error_string "no num lines for archived features")
  ; next_steps        = []
  ; status            = Archived { archived_at; reason_for_archiving }
  }
;;

let to_is_archived t = Is_archived.Yes { reason_for_archiving = t.reason_for_archiving }
