module Stable = struct
  open Core.Core_stable
  open Import_stable

  module Diff4_in_session = Review_session.Stable.Diff4_in_session
  module Rev = Rev.Stable

  module Diff4_to_catch_up = struct
    module V3 = struct
      type t =
        { diff4_in_session : Diff4_in_session.V2.t
        ; reason           : Catch_up_kind.V1.t
        }
      [@@deriving bin_io, compare, sexp, fields]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 41e1c896b88d70348038ccc7ddbff128 |}]
      ;;
    end
    module Model = V3
    module V2 = struct
      type t =
        { diff4_in_session : Diff4_in_session.V2.t
        ; reviewed_by      : User_name.V1.t
        ; reason           : string
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8f3cc47f57a124a4143720230d174839 |}]
      ;;

      open! Core
      open! Import

      let of_v3 { V3.diff4_in_session; reason } =
        match reason with
        | Reviewed_by_someone_else { reviewed_by; reason } ->
          { diff4_in_session
          ; reviewed_by
          ; reason
          }
        | Create_catch_up_for_me
        | Follower
        | Unfinished_review ->
          { diff4_in_session
          ; reviewed_by = User_name.of_string "iron"
          ; reason      = Catch_up_kind.to_string_hum reason
          }
      ;;

      let to_v3 { diff4_in_session
                ; reviewed_by
                ; reason
                } =
        { V3.
          diff4_in_session
        ; reason = Reviewed_by_someone_else { reviewed_by; reason }
        }
      ;;
    end
  end

  module Creation = struct

    module Context = struct
      open! Core
      open! Import

      type t =
        { user_name                        : User_name.t
        }
      [@@deriving sexp_of]
    end

    (* Most of the attributes fields (description, base, tip, seconder, etc.) reflects the
       state of the feature as of when the catch_up_session is created.  However, we need
       to keep in addition to that the [reviewer_in_session] value so that the review
       client would show the same during catch-up review than shown during the review.  In
       practice [reviewer_in_session] may be inconsistent with [whole_feature_reviewers]
       if this latter set has changed in the feature since the moment the review session
       was created. *)
    module V3 = struct
      type t =
        { session_id                           : Session_id.V1.t
        ; session_tip                          : Rev.V1.t
        ; creation_time                        : Time.V1_round_trippable.t
        ; reviewer_in_session                  : Reviewer.V2.t
        ; diff4s_in_session                    : Diff4_in_session.V2.t array
        ; remote_rev_zero                      : Rev.V1.t
        ; remote_repo_path                     : Remote_repo_path.V1.t
        ; feature_path                         : Feature_path.V1.t
        ; feature_id                           : Feature_id.V1.t
        ; whole_feature_reviewers              : User_name.V1.Set.t
        ; owners                               : User_name.V1.t list
        ; base                                 : Rev.V1.t
        ; tip                                  : Rev.V1.t
        ; description                          : string
        ; is_permanent                         : bool
        ; seconder                             : User_name.V1.t option
        }
      [@@deriving sexp, fields]

      let to_model _context m = m
    end
    module Model = V3
    module V2 = struct
      type t =
        { session_id                : Session_id.V1.t
        ; session_tip               : Rev.V1.t
        ; creation_time             : Time.V1_round_trippable.t
        ; diff4s_in_session         : Diff4_in_session.V2.t array
        ; remote_rev_zero           : Rev.V1.t
        ; remote_repo_path          : Remote_repo_path.V1.t
        ; feature_path              : Feature_path.V1.t
        ; feature_id                : Feature_id.V1.t
        ; whole_feature_reviewers   : User_name.V1.Set.t
        ; owners                    : User_name.V1.t list
        ; base                      : Rev.V1.t
        ; tip                       : Rev.V1.t
        ; description               : string
        ; is_permanent              : bool
        ; seconder                  : User_name.V1.t option
        }
      [@@deriving sexp, fields]

      open! Core
      open! Import

      let to_v3 (context : Context.t)
            { session_id
            ; session_tip
            ; creation_time
            ; diff4s_in_session
            ; remote_rev_zero
            ; remote_repo_path
            ; feature_path
            ; feature_id
            ; whole_feature_reviewers
            ; owners
            ; base
            ; tip
            ; description
            ; is_permanent
            ; seconder
            } =
        { V3.
          session_id
        ; session_tip
        ; creation_time
        ; reviewer_in_session
          = { user_name = context.user_name
            ; is_whole_feature_follower = false
            ; is_whole_feature_reviewer =
                (* When Iron was using V2 the logic was such that review client would
                   always show both the attributes and the contents for any diff, so this
                   has the desired effect *)
                true
            }
        ; diff4s_in_session
        ; remote_rev_zero
        ; remote_repo_path
        ; feature_path
        ; feature_id
        ; whole_feature_reviewers
        ; owners
        ; base
        ; tip
        ; description
        ; is_permanent
        ; seconder
        }
      ;;

      let to_model context t = V3.to_model context (to_v3 context t)
    end
  end

  module Action = struct
    module V3 = struct
      type t =
        [ `Reviewed of Diff4_to_catch_up.V3.t list
        | `Catch_up of Diff4_in_session.Id.V1.t list
        | `Set_feature_path of Feature_path.V1.t
        ]
      [@@deriving sexp]
    end
    module Model = V3
    module V2 = struct
      type t =
        [ `Reviewed of Diff4_to_catch_up.V2.t list
        | `Catch_up of Diff4_in_session.Id.V1.t list
        | `Set_feature_path of Feature_path.V1.t
        ]
      [@@deriving sexp]

      open! Core
      open! Import

      let to_v3 : t -> V3.t = function
        | (`Catch_up _ | `Set_feature_path _) as t -> t
        | `Reviewed v2 -> `Reviewed (List.map v2 ~f:Diff4_to_catch_up.V2.to_v3)
      ;;
    end
  end

  module Action_query = struct
    module V3 = struct
      type t = Action.V3.t Query.V1.t
      [@@deriving sexp]

      let to_model m = m
    end
    module Model = V3
    module V2 = struct
      type t = Action.V2.t Query.V1.t
      [@@deriving sexp]

      let to_model t = V3.to_model (Query.V1.map t ~f:Action.V2.to_v3)
    end
  end
end

open Core
open Import

module Creation = struct
  module Context = Stable.Creation.Context

  include Stable.Creation.Model

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~session_id:                ignore
        ~session_tip:               (check Rev.invariant)
        ~creation_time:             ignore
        ~reviewer_in_session:       (check Reviewer.invariant)
        ~diff4s_in_session:         ignore
        ~remote_rev_zero:           (check Rev.invariant)
        ~remote_repo_path:          ignore
        ~feature_path:              ignore
        ~feature_id:                ignore
        ~whole_feature_reviewers:   (check (Set.iter ~f:User_name.invariant))
        ~owners:                    (check (List.iter ~f:User_name.invariant))
        ~base:                      (check Rev.invariant)
        ~tip:                       (check Rev.invariant)
        ~description:               ignore
        ~is_permanent:              ignore
        ~seconder:                  ignore
    )
  ;;
end

module Action       = Stable.Action.       Model
module Action_query = Stable.Action_query. Model

module Persist = struct
  module Creation = struct
    include Persistent.Make_with_context
        (Stable.Creation.Context)
        (struct let version = 3 end)
        (Stable.Creation.V3)
    include Register_read_old_version
        (struct let version = 1 end)
        (Stable.Creation.V2)
  end
  module Action_query = struct
    include Persistent.Make
        (struct let version = 3 end)
        (Stable.Action_query.V3)
    include Register_read_old_version
        (struct let version = 1 end)
        (Stable.Action_query.V2)
  end
end

module Id_and_kind = struct
  type t =
    { id   : Diff4_in_session.Id.t
    ; kind : Catch_up_kind.t
    }
  [@@deriving sexp_of]
end

module Diff4_to_catch_up = struct
  include Stable.Diff4_to_catch_up.Model
  let id t = Diff4_in_session.id t.diff4_in_session
  let kind = reason
  let num_lines t reviewer = Diff4_in_session.num_lines t.diff4_in_session reviewer
  let path_in_repo_at_f2 t = Diff4_in_session.path_in_repo_at_f2 t.diff4_in_session
end

type t =
  { creation                  : Creation.t
  ; mutable feature_path      : Feature_path.t
  ; diff4s_in_session_by_id   : Diff4_in_session.t Diff4_in_session.Id.Table.t
  ; diff4s_to_catch_up        : Diff4_to_catch_up.t Diff4_in_session.Id.Table.t
  ; mutable serializer        : Serializer.t option
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    let ids_in_session =
      Array.map t.creation.diff4s_in_session ~f:Diff4_in_session.id
      |> Array.to_list
      |> Diff4_in_session.Id.Hash_set.of_list
    in
    Fields.iter
      ~creation:(check Creation.invariant)
      ~feature_path:(check Feature_path.invariant)
      ~diff4s_in_session_by_id:(check (fun diff4s_in_session_by_id ->
        let ids_in_table =
          Hashtbl.keys diff4s_in_session_by_id
          |> Diff4_in_session.Id.Hash_set.of_list
        in
        assert (Hash_set.equal ids_in_session ids_in_table);))
      ~diff4s_to_catch_up:(check (fun diff4s_to_catch_up ->
        Hashtbl.iteri diff4s_to_catch_up ~f:(fun ~key:id ~data:diff4_to_catch_up ->
          if not (Hash_set.mem ids_in_session id)
          then raise_s [%sexp "unknown id in to catch up", [%here]
                              , (diff4_to_catch_up : Diff4_to_catch_up.t)];
          [%test_result: Diff4_in_session.Id.t] ~expect:id
            (Diff4_to_catch_up.id diff4_to_catch_up))))
      ~serializer:(check (Option.iter ~f:Serializer.invariant));
  )
;;

let creation_file = Relpath.of_string "creation"
let queries_file  = Relpath.of_string "queries"

let reviewer t = t.creation.reviewer_in_session

let diff4s_in_session_by_id diff4s_in_session =
  let table = Diff4_in_session.Id.Table.create () in
  Array.iter diff4s_in_session ~f:(fun diff4_in_session ->
    let id = Diff4_in_session.id diff4_in_session in
    let diff4_in_session = Diff4_in_session.unreviewed_copy diff4_in_session in
    Hashtbl.set table ~key:id ~data:diff4_in_session);
  table
;;

let serializer_exn t =
  match t.serializer with
  | Some s -> s
  | None -> raise_s [%sexp "serializer isn't defined", [%here], (t : t)]
;;

let record t query action =
  Serializer.append_to (serializer_exn t) ~file:queries_file
    (Query.with_action query action) (module Persist.Action_query);
;;

let reviewed_internal t diff4s_to_catch_up =
  List.iter diff4s_to_catch_up ~f:(fun diff4_to_catch_up ->
    let { Diff4_to_catch_up. diff4_in_session; _ } = diff4_to_catch_up in
    let id = Diff4_in_session.id diff4_in_session in
    Hashtbl.set t.diff4s_to_catch_up ~key:id ~data:diff4_to_catch_up);
;;

let reviewed t query ids_and_kinds =
  let diff4s_to_catch_up, invalid_ids =
    List.partition_map ids_and_kinds ~f:(fun { Id_and_kind. id; kind } ->
      match Hashtbl.find t.diff4s_in_session_by_id id with
      | Some diff4_in_session ->
        `Fst { Diff4_to_catch_up.
               diff4_in_session
             ; reason = kind
             }
      | None -> `Snd id)
  in
  record t query (`Reviewed diff4s_to_catch_up);
  reviewed_internal t diff4s_to_catch_up;
  if List.is_empty invalid_ids
  then Ok ()
  else Or_error.error "invalid ids" invalid_ids [%sexp_of: Diff4_in_session.Id.t list]
;;

let catch_up_internal t valid_ids =
  List.iter valid_ids ~f:(fun id -> Hashtbl.remove t.diff4s_to_catch_up id);
;;

let catch_up t query ids =
  let valid_ids, invalid_ids =
    List.partition_tf ids ~f:(fun id -> Hashtbl.mem t.diff4s_to_catch_up id)
  in
  record t query (`Catch_up valid_ids);
  catch_up_internal t valid_ids;
  if List.is_empty invalid_ids
  then Ok ()
  else Or_error.error "invalid ids" invalid_ids [%sexp_of: Diff4_in_session.Id.t list]
;;

let set_feature_path_internal t feature_path =
  t.feature_path <- feature_path;
;;

let set_feature_path t query feature_path =
  set_feature_path_internal t feature_path;
  record t query (`Set_feature_path feature_path);
;;

let apply_query_internal t (query : Action.t Query.t) =
  match Query.action query with
  | `Reviewed ids                  -> reviewed_internal         t ids
  | `Catch_up ids                  -> catch_up_internal         t ids
  | `Set_feature_path feature_path -> set_feature_path_internal t feature_path
;;

let t_of_creation creation ~serializer =
  { creation
  ; feature_path            = Creation.feature_path creation
  ; diff4s_to_catch_up      = Diff4_in_session.Id.Table.create ()
  ; diff4s_in_session_by_id = diff4s_in_session_by_id creation.diff4s_in_session
  ; serializer
  }
;;

let create
      ~review_session
      ~remote_rev_zero
      ~remote_repo_path
      ~feature_path
      ~feature_id
      ~whole_feature_reviewers
      ~owners
      ~base
      ~tip
      ~description
      ~is_permanent
      ~seconder
      serializer
  =
  let creation_time = Time.now () in
  let diff4s_in_session =
    Review_session.diff4s_in_session_not_implicitly_reviewed review_session
  in
  let reviewer = Review_session.reviewer review_session in
  let creation =
    { Creation.
      session_id          = Review_session.id  review_session
    ; session_tip         = Review_session.tip review_session
    ; creation_time
    ; reviewer_in_session = reviewer
    ; diff4s_in_session
    ; remote_rev_zero
    ; remote_repo_path
    ; feature_path
    ; feature_id
    ; whole_feature_reviewers
    ; owners
    ; base
    ; tip
    ; description
    ; is_permanent
    ; seconder
    }
  in
  let context =
    { Creation.Context. user_name = reviewer.user_name }
  in
  let t = t_of_creation creation ~serializer:(Some serializer) in
  Serializer.set_contents serializer ~file:creation_file (context, creation)
    (module Persist.Creation.Writer);
  t
;;

let deserializer = Deserializer.with_serializer (fun serializer ->
  let open Deserializer.Let_syntax in
  let%map_open () = return ()
  and creation = one         (module Persist.Creation.Reader) ~in_file:creation_file
  and queries  = sequence_of (module Persist.Action_query)    ~in_file:queries_file
  in
  fun ~user_name ->
    let creation = creation { Creation.Context. user_name } in
    let t = t_of_creation creation ~serializer:None in
    List.iter queries ~f:(fun query -> apply_query_internal t query);
    t.serializer <- Some serializer;
    t
)
;;

let diff4s_to_catch_up t =
  Hashtbl.data t.diff4s_to_catch_up
;;

let line_count_remaining_to_catch_up t =
  let actual_line_count =
    let reviewer = reviewer t in
    diff4s_to_catch_up t
    |> List.fold ~init:Line_count.Catch_up.zero ~f:(fun acc diff4_to_catch_up ->
      Line_count.Catch_up.add_count acc diff4_to_catch_up.reason
        (Diff4_to_catch_up.num_lines diff4_to_catch_up reviewer))
  in
  if Line_count.Catch_up.total actual_line_count > 0
  then actual_line_count
  else
    (* For historical reasons, old sessions might have been created at a time where the
       line count computation had a slightly different implementation, and since we use
       that number to generate the users's todo, if we do not return at least 1, the
       sessions with count=0 will never get a chance to be cleaned up. *)
    { actual_line_count with reviewed_by_someone_else = 1 }
;;

let all_diff4s_are_caught_up t = Hashtbl.is_empty t.diff4s_to_catch_up

let creation_time t = t.creation.creation_time
let feature_id    t = t.creation.feature_id
let id            t = t.creation.session_id
