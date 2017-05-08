module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module Unstable_diff4 = Diff4
  module Diff2 = Diff2. Stable
  module Diff4 = Diff4. Stable
  module Rev   = Rev.   Stable

  module Diff4_in_session = struct
    module Id = struct
      module V1 = struct
        type t = int [@@deriving bin_io, compare, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 698cfa4093fe5e51523842d37b92aeac |}]
        ;;
      end

      module Model = V1

      module And_review_kind = struct
        module V1 = struct
          type t =
            { diff4_id    : V1.t
            ; review_kind : Review_kind.V1.t
            }
          [@@deriving fields, sexp]
        end

        module Model = V1
      end
    end

    module V2 = struct
      type t =
        { index       : int
        ; diff4       : Diff4.V2.t
        ; is_reviewed : bool
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 4f7e7ef41d2ce7c682648bdbdd1168db |}]
      ;;
    end

    module Model = V2

    module And_review_kind = struct
      module V2 = struct
        type t =
          { diff4_in_session : V2.t
          ; review_kind      : Review_kind.V1.t
          }
        [@@deriving bin_io, compare, fields, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| de839cfcf3f9d0bcb86d11fe0ae8ed1c |}]
        ;;
      end

      module Model = V2
    end
  end

  module Creation = struct

    module Context = struct
      open! Core
      open! Import

      type t =
        { user_name : User_name.t
        }
      [@@deriving sexp_of]
    end

    module V5 = struct
      type t =
        { reviewer    : Reviewer.V2.t
        ; id          : Session_id.V1.t
        ; diff4s      : Diff4.And_output_num_lines.V1.t list
        ; sort_diff4s : bool
        ; tip         : Rev.V1.t
        ; base        : Rev.V1.t
        }
      [@@deriving sexp]

      let to_model _context m = m
    end

    module Model = V5

    module V4 = struct
      type t =
        { reviewer    : Reviewer.V1.t
        ; id          : Session_id.V1.t
        ; diff4s      : Diff4.And_output_num_lines.V1.t list
        ; sort_diff4s : bool
        ; tip         : Rev.V1.t
        ; base        : Rev.V1.t
        }
      [@@deriving sexp]

      open! Core
      open! Import

      let to_model (context : Context.t)
            { reviewer; id; diff4s; sort_diff4s; tip; base } =
        V5.to_model context
          { V5.
            reviewer =
              { user_name = context.user_name
              ; is_whole_feature_follower = false
              ; is_whole_feature_reviewer =
                  (match reviewer with
                   | Whole_feature_reviewer | Whole_feature_reviewer_plus_ignored -> true
                   | Normal_reviewer _ -> false)
              }
          ; id
          ; diff4s
          ; sort_diff4s
          ; tip
          ; base
          }
      ;;
    end

    module V3 = struct
      type t =
        { reviewer    : Reviewer.V1.t
        ; id          : Session_id.V1.t
        ; diff4s      : Diff4.And_output_num_lines.V1.t list
        ; sort_diff4s : bool
        ; tip         : Rev.V1.t
        }
      [@@deriving sexp]

      let to_model context { reviewer; id; diff4s; sort_diff4s; tip } =
        let base =
          let b2_if_not_forget (d : Diff4.And_output_num_lines.V1.t) =
            if Unstable_diff4.is_forget d.diff4
            then None
            else Some (Attributed_file.rev d.diff4.diamond.b2)
          in
          (* If the session is empty or only contains forgets, we cannot determine the
             base of the review goal, so we take the tip.  Given that we only use this
             field to avoid recreating a new session, the worst that can happen is
             recreating empty sessions. *)
          match Core.List.find_map diff4s ~f:b2_if_not_forget with
          | None -> tip
          | Some b2 -> b2
        in
        V4.to_model context { V4. reviewer; id; diff4s; sort_diff4s; tip; base }
      ;;
    end

    module V2 = struct
      type t =
        { reviewer    : Reviewer.V1.t
        ; id          : Session_id.V1.t
        ; diff4s      : Diff4.V2.t list
        ; sort_diff4s : bool
        ; tip         : Rev.V1.t
        }
      [@@deriving sexp]

      open! Core

      let to_model context { reviewer; id; diff4s; sort_diff4s; tip } =
        let diff4s =
          List.map diff4s ~f:(fun diff4 ->
            ({ diff4 ; output_num_lines = 1 } : Diff4.And_output_num_lines.V1.t))
        in
        V3.to_model context
          { V3.
            reviewer
          ; id
          ; diff4s
          ; sort_diff4s
          ; tip
          }
      ;;
    end

    module V1 = struct
      type t =
        { id     : Session_id.V1.t
        ; diff4s : Diff4.V2.t list
        ; tip    : Rev.V1.t
        }
      [@@deriving sexp]

      open! Core

      let to_model context { id; diff4s; tip } =
        V2.to_model context
          { V2.
            reviewer    = Whole_feature_reviewer
          ; id
          ; diff4s
          ; sort_diff4s = true
          ; tip
          }
      ;;
    end
  end

  module Action = struct

    module V2 = struct
      type t =
        [ `Set_is_locked           of bool
        | `Reviewed_v1_compatible  of Diff4_in_session.Id.V1.t list
        | `Reviewed                of Diff4_in_session.Id.And_review_kind.V1.t list
        | `Set_to_nothing_reviewed
        | `Unreviewed              of Diff4_in_session.Id.V1.t list
        ]
      [@@deriving sexp]
    end

    module V1 = struct
      type t =
        [ `Reviewed                of Diff4_in_session.Id.V1.t list
        | `Set_to_nothing_reviewed
        | `Unreviewed              of Diff4_in_session.Id.V1.t list
        ]
      [@@deriving sexp]

      open! Core
      open! Import

      let to_v2 : t -> V2.t = function
        | ( `Set_to_nothing_reviewed | `Unreviewed _ ) as compatible -> compatible
        | `Reviewed ids -> `Reviewed_v1_compatible ids
      ;;
    end

    module Model = V2
  end

  module Action_query = struct
    module V2 = struct
      type t = Action.V2.t Query.V1.t [@@deriving sexp]

      let to_model m = m
    end

    module V1 = struct
      type t = Action.V1.t Query.V1.t [@@deriving sexp]

      let to_v2 = Query.V1.map ~f:Action.V1.to_v2

      let to_model t = V2.to_model (to_v2 t)
    end

    module Model = V2
  end

end

open! Core
open! Import

module Diff4_in_session = struct
  module Stable = Stable.Diff4_in_session
  include Stable.Model

  module Id = struct
    module Stable = Stable.Id
    include Stable.Model
    include (Int : Hashable.S with type t := t)
    let to_string = Int.to_string
    let arg_type = Command.Spec.Arg_type.create Int.of_string
    module And_review_kind = Stable.And_review_kind.Model
  end

  let id t = t.index

  let num_lines t reviewer = Diff4.num_lines t.diff4 reviewer

  let path_in_repo_at_f2 t = Diff4.path_in_repo_at_f2 t.diff4

  let compare_by_path_in_repo_at_f2_for_review t1 t2 =
    Path_in_repo.default_review_compare
      (path_in_repo_at_f2 t1)
      (path_in_repo_at_f2 t2)
  ;;

  let unreviewed_copy t = { t with is_reviewed = false }

  module And_review_kind = Stable.And_review_kind.Model
end

module Reviewable_diff4 = struct

  module Review_status = struct
    module Maybe_frozen_review_kind = struct
      (* [frozen_review_kind] has to be an option because of features existing before the
         field existed, as well as implicitly reviewed diff4s. *)
      type t = { mutable frozen_review_kind : Review_kind.t option }
      [@@deriving sexp_of]
    end

    type t =
      | Not_reviewed
      | Reviewed of Maybe_frozen_review_kind.t
    [@@deriving sexp_of]

    let is_reviewed = function
      | Not_reviewed -> false
      | Reviewed { frozen_review_kind = _ } -> true
    ;;
  end

  type t =
    { index                 : int
    ; diff4                 : Diff4.t
    ; mutable review_status : Review_status.t
    }
  [@@deriving fields, sexp_of]

  let is_reviewed t = Review_status.is_reviewed t.review_status

  let diff4_in_session { index; diff4; review_status } =
    { Diff4_in_session.
      index
    ; diff4
    ; is_reviewed = Review_status.is_reviewed review_status
    }
  ;;

  let is_implicitly_reviewed t reviewer =
    Diff4.is_implicitly_reviewed t.diff4 reviewer
  ;;

  let invariant t reviewer =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~index:(check (fun index -> assert (index >= 0)))
        ~diff4:(check Diff4.invariant)
        ~review_status:(check (fun review_status ->
          if is_implicitly_reviewed t reviewer
          then assert (Review_status.is_reviewed review_status))))
  ;;

  let reviewed t ~frozen_review_kind =
    t.review_status <- Reviewed { frozen_review_kind }
  ;;


  let unreviewed t = t.review_status <- Not_reviewed
  ;;

  let check_unreviewed t =
    if is_reviewed t
    then Ok ()
    else error "not reviewed yet" t.diff4 [%sexp_of: Diff4.t]
  ;;

  let mark_implicitly_as_reviewed t reviewer =
    if not (is_reviewed t) && is_implicitly_reviewed t reviewer
    then t.review_status <- Reviewed { frozen_review_kind = None };
  ;;

  let change_to_not_reviewed t reviewer =
    if is_reviewed t && not (is_implicitly_reviewed t reviewer)
    then t.review_status <- Not_reviewed;
  ;;

  let num_lines_completed t reviewer =
    if is_reviewed t
    then Diff4.num_lines t.diff4 reviewer
    else 0
  ;;
end

module Reviewable_diff4s = struct

  type t = Reviewable_diff4.t array
  [@@deriving sexp_of]

  let mark_implicitly_as_reviewed (t : t) reviewer =
    Array.iter t ~f:(fun reviewable_diff4 ->
      Reviewable_diff4.mark_implicitly_as_reviewed reviewable_diff4 reviewer)
  ;;

  let create diff4s reviewer ~sort_diff4s =
    let diff4s = List.map diff4s ~f:Diff4.And_output_num_lines.diff4 in
    let diff4s =
      if not sort_diff4s
      then diff4s
      else
        diff4s
        |> List.sort ~cmp:(fun d1 d2 ->
          String.alphabetic_compare
            (Path_in_repo.to_string (Diff4.path_in_repo_at_f2 d1))
            (Path_in_repo.to_string (Diff4.path_in_repo_at_f2 d2)))
    in
    let t =
      diff4s
      |> List.mapi ~f:(fun index diff4 ->
        { Reviewable_diff4.
          index
        ; diff4
        ; review_status = Not_reviewed
        }
      )
      |> Array.of_list
    in
    mark_implicitly_as_reviewed t reviewer;
    t
  ;;

  let invariant t reviewer =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      Array.iter t ~f:(fun d -> Reviewable_diff4.invariant d reviewer);
      for i = 0 to pred (Array.length t) do
        let index = t.(i).index in
        if i <> index
        then raise_s [%sexp "invalid index", { index : int; expected = (i : int)}]
      done)
  ;;

  let set_to_nothing_reviewed (t : t) reviewer =
    Array.iter t ~f:(fun reviewable_diff4 ->
      Reviewable_diff4.change_to_not_reviewed reviewable_diff4 reviewer);
  ;;
end

module Action = Stable.Action.Model
module Creation = struct
  module Context = Stable.Creation.Context
  include Stable.Creation.Model
end

module Persist = struct
  module Creation = struct
    include Persistent.Make_with_context
        (Stable.Creation.Context)
        (struct let version = 5 end)
        (Stable.Creation.V5)
    include Register_read_old_version
        (struct let version = 4 end)
        (Stable.Creation.V4)
    include Register_read_old_version
        (struct let version = 3 end)
        (Stable.Creation.V3)
    include Register_read_old_version
        (struct let version = 2 end)
        (Stable.Creation.V2)
    include Register_read_old_version
        (struct let version = 1 end)
        (Stable.Creation.V1)
  end
  module Action_query = struct
    include Persistent.Make
        (struct let version = 2 end)
        (Stable.Action_query.V2)
    include Register_read_old_version
        (struct let version = 1 end)
        (Stable.Action_query.V1)
  end
end

type t =
  { reviewer              : Reviewer.t
  ; id                    : Session_id.t
  ; diff4s                : Diff4s.t
  ; tip                   : Rev.t
  ; base                  : Rev.t
  ; reviewable_diff4s     : Reviewable_diff4s.t
  ; mutable is_locked     : bool
  ; dynamic_upgrade_state : Dynamic_upgrade.State.t
  ; mutable serializer    : Serializer.t option
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~reviewer:(check Reviewer.invariant)
      ~id:(check Session_id.invariant)
      ~diff4s:(check Diff4s.invariant)
      ~tip:(check Rev.invariant)
      ~base:(check Rev.invariant)
      ~reviewable_diff4s:(check (fun reviewable_diff4s ->
        Reviewable_diff4s.invariant reviewable_diff4s t.reviewer))
      ~is_locked:(check (ignore : bool -> unit))
      ~dynamic_upgrade_state:(check Dynamic_upgrade.State.Reference.invariant)
      ~serializer:(check (Option.iter ~f:Serializer.invariant)))
;;

(* We never want to show rev-updates to people, so we filter them out. *)
let map_diff4s_not_implicitly_reviewed t ~f =
  Array.filter_map t.reviewable_diff4s ~f:(fun reviewable_diff4 ->
    if Reviewable_diff4.is_implicitly_reviewed reviewable_diff4 t.reviewer
    then None
    else Some (f reviewable_diff4))
;;

let diff4s_in_session_not_implicitly_reviewed t =
  map_diff4s_not_implicitly_reviewed t ~f:Reviewable_diff4.diff4_in_session
;;

let diff4s_and_review_kind_in_session_not_implicitly_reviewed t ~compute_review_kind =
  map_diff4s_not_implicitly_reviewed t ~f:(fun reviewable_diff4 ->
    let review_kind =
      let computed_review_kind = lazy (compute_review_kind reviewable_diff4.diff4) in
      match reviewable_diff4.review_status with
      | Not_reviewed -> force computed_review_kind
      | Reviewed ({ frozen_review_kind = Some review_kind }) -> review_kind
      | Reviewed ({ frozen_review_kind = None } as r) ->
        (* This applies to old features only.  We've deserialized old
           [`Reviewed_v1_compatible] queries, thus we do not have a frozen review kind
           even though the diff is reviewed.  Freezing it here on the spot avoids the
           flickering. *)
        let review_kind = force computed_review_kind in
        r.frozen_review_kind <- Some review_kind;
        review_kind
    in
    { Diff4_in_session.And_review_kind.
      diff4_in_session = Reviewable_diff4.diff4_in_session reviewable_diff4
    ; review_kind
    })
;;

let filter_diff4s t ~f:predicate =
  let by_diff4 =
    Diff4.Table.of_alist_exn
      (List.map t.diff4s ~f:(fun ({ diff4; _ } as z) -> (diff4, z)))
  in
  Array.filter_map t.reviewable_diff4s ~f:(fun reviewable_diff4 ->
    if predicate reviewable_diff4
    then Some (Hashtbl.find_exn by_diff4 reviewable_diff4.diff4)
    else None)
  |> Array.to_list
;;

let reviewed_diff4s t =
  filter_diff4s t ~f:Reviewable_diff4.is_reviewed
;;

let not_reviewed_diff4s t =
  filter_diff4s t ~f:(fun d -> not (Reviewable_diff4.is_reviewed d))
;;

let creation_file = Relpath.of_string "creation"
let queries_file  = Relpath.of_string "queries"

let serializer_exn t =
  match t.serializer with
  | Some s -> s
  | None ->
    raise_s [%sexp "serializer isn't defined", (t : t), (Backtrace.get () : Backtrace.t)]
;;

let record t query action =
  let allowed_from =
    match (action : Action.t) with
    (* Note to devs: please keep at least this line so that it is easy to add/remove
       lines there *)
    | #Action.t -> Dynamic_upgrade.U1
  in
  let serializer = serializer_exn t in
  match Dynamic_upgrade.commit_to_upgrade t.dynamic_upgrade_state ~allowed_from with
  | `Not_allowed_yet ->
    (* Make sure to invalidate the cache in the case where the event are not recorded.
       This is handled by the serializer directly in the other case. *)
    Serializer.invalidate_dependents serializer
  | `Ok ->
    Serializer.append_to serializer ~file:queries_file
      (Query.with_action query action) (module Persist.Action_query);
;;

let t_of_creation creation ~dynamic_upgrade_state ~serializer =
  let { Creation.
        reviewer
      ; id
      ; diff4s
      ; sort_diff4s
      ; tip
      ; base
      } = creation
  in
  { reviewer
  ; id
  ; diff4s
  ; tip
  ; base
  ; reviewable_diff4s = Reviewable_diff4s.create diff4s reviewer ~sort_diff4s
  ; is_locked = false
  ; dynamic_upgrade_state
  ; serializer
  }
;;

let create ~serializer_dir_of_id ~reviewer
      ~diff4s ~tip ~base ~feature_cache_invalidator
      ~dynamic_upgrade_state
      serializer =
  let id = Session_id.create () in
  let serializer = Serializer.relativize serializer ~dir:(serializer_dir_of_id id) in
  Serializer.add_cache_invalidator serializer feature_cache_invalidator;
  let sort_diff4s = false in
  let creation =
    { Creation.
      reviewer
    ; id
    ; diff4s
    ; sort_diff4s
    ; tip
    ; base
    }
  in
  let context =
    { Creation.Context.
      user_name = reviewer.user_name
    }
  in
  let t =
    t_of_creation creation ~dynamic_upgrade_state
      ~serializer:(Some serializer)
  in
  Serializer.set_contents serializer
    ~file:creation_file (context, creation)
    (module Persist.Creation.Writer);
  t
;;

let check_session_id t session_id =
  Session_id.check ~actual:t.id ~supplied:session_id
;;

let is_locked t = t.is_locked

let have_done_some_review t =
  Array.exists (diff4s_in_session_not_implicitly_reviewed t)
    ~f:Diff4_in_session.is_reviewed
;;

let all_diff4s_are_reviewed t =
  Array.for_all t.reviewable_diff4s ~f:Reviewable_diff4.is_reviewed
;;

let set_is_locked_internal t is_locked =
  t.is_locked <- is_locked
;;

let set_is_locked t query is_locked =
  if not (Bool.equal t.is_locked is_locked)
  then (
    record t query (`Set_is_locked is_locked);
    set_is_locked_internal t is_locked)
;;

let reviewed_v1_compatible_internal t diff4_ids =
  List.iter diff4_ids ~f:(fun diff4_id ->
    Reviewable_diff4.reviewed t.reviewable_diff4s.( diff4_id ) ~frozen_review_kind:None);
;;

let reviewed_internal t (diff4_ids : Diff4_in_session.Id.And_review_kind.t list) =
  List.iter diff4_ids ~f:(fun { diff4_id; review_kind } ->
    Reviewable_diff4.reviewed
      t.reviewable_diff4s.( diff4_id ) ~frozen_review_kind:(Some review_kind))
;;

let reviewed t query session_id diff4_ids ~compute_review_kind ~is_using_locked_sessions
      ~even_if_some_files_are_already_reviewed =
  match check_session_id t session_id with
  | Error _ as e -> e
  | Ok () ->
    let already_reviewed_files, diff4_id_and_review_kinds =
      List.partition_map diff4_ids ~f:(fun diff4_id ->
        let diff4_in_session = t.reviewable_diff4s.(diff4_id) in
        if Reviewable_diff4.is_reviewed diff4_in_session
        then
          `Fst (Diff4.path_in_repo_at_f2 diff4_in_session.diff4)
        else (
          let review_kind = compute_review_kind diff4_in_session.diff4 in
          `Snd { Diff4_in_session.Id.And_review_kind. diff4_id; review_kind }))
    in
    if not (List.is_empty already_reviewed_files)
    && not even_if_some_files_are_already_reviewed
    then (
      let already_reviewed_files = Path_in_repo.Set.of_list already_reviewed_files in
      let msg =
        (if Set.length already_reviewed_files > 1
         then "These files are"
         else "This file is"
        ) ^ " already reviewed"
      in
      error_s [%sexp (msg : string), (already_reviewed_files : Path_in_repo.Set.t)])
    else (
      if not (List.is_empty diff4_id_and_review_kinds)
      then (
        record t query (`Reviewed diff4_id_and_review_kinds);
        reviewed_internal t diff4_id_and_review_kinds;
        if is_using_locked_sessions then set_is_locked t query true);
      Ok ())
;;

let unreviewed_internal t diff4_ids =
  List.iter diff4_ids ~f:(fun diff4_id ->
    Reviewable_diff4.unreviewed t.reviewable_diff4s.( diff4_id ));
;;

let unreviewed t query session_id diff4_ids =
  match check_session_id t session_id with
  | Error _ as e -> e
  | Ok () ->
    List.map diff4_ids ~f:(fun diff4_id ->
      match Reviewable_diff4.check_unreviewed t.reviewable_diff4s.( diff4_id ) with
      | Error _ as err -> err
      | Ok () -> Ok diff4_id)
    |> Or_error.combine_errors
    |> Or_error.map ~f:(fun diff4_ids ->
      if not (List.is_empty diff4_ids)
      then (
        record t query (`Unreviewed diff4_ids);
        unreviewed_internal t diff4_ids));
;;

let set_to_nothing_reviewed_internal t =
  Reviewable_diff4s.set_to_nothing_reviewed t.reviewable_diff4s t.reviewer;
;;

let set_to_nothing_reviewed t query session_id =
  match check_session_id t session_id with
  | Error _ as e -> e
  | Ok () ->
    record t query `Set_to_nothing_reviewed;
    set_to_nothing_reviewed_internal t;
    Ok ()
;;

let apply_query_internal t (query : Action.t Query.t) =
  match Query.action query with
  | `Reviewed                diff4_ids -> reviewed_internal                t diff4_ids
  | `Unreviewed              diff4_ids -> unreviewed_internal              t diff4_ids
  | `Set_is_locked           is_locked -> set_is_locked_internal           t is_locked
  | `Set_to_nothing_reviewed           -> set_to_nothing_reviewed_internal t
  | `Reviewed_v1_compatible  diff4_ids -> reviewed_v1_compatible_internal  t diff4_ids
;;

let num_lines_completed t =
  Array.sum (module Int) t.reviewable_diff4s ~f:(fun d ->
    Reviewable_diff4.num_lines_completed d t.reviewer)
;;

let deserializer =
  Deserializer.with_serializer (fun serializer ->
    let open Deserializer.Let_syntax in
    let%map_open () = return ()
    and creation = one         (module Persist.Creation.Reader) ~in_file:creation_file
    and queries  = sequence_of (module Persist.Action_query)    ~in_file:queries_file
    in
    fun ~user_name ~feature_cache_invalidator ~dynamic_upgrade_state ->
      let creation = creation { Creation.Context. user_name } in
      let t = t_of_creation creation ~dynamic_upgrade_state ~serializer:None in
      List.iter queries ~f:(fun query -> apply_query_internal t query);
      t.serializer <- Some serializer;
      Serializer.add_cache_invalidator serializer feature_cache_invalidator;
      t
  )
;;
