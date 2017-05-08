module Stable = struct
  open! Core.Core_stable
  open! Import_stable
  module Diff2 = Diff2.Stable

  module Mark_kind = struct
    module V1 = struct
      type t =
        | User
        | Internal__fully_reviewed
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 1255753ecd11060f9d0f070921d74d8f |}]
      ;;
    end

    module Model = V1
  end

  module Marked_diff2 = struct
    module V1 = struct
      type t =
        { diff2  : Diff2.V2.t
        ; mark_kind : Mark_kind.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 355e7ada3f767df34ae033645416e885 |}]
      ;;
    end

    module Model = V1
  end

  module V3 = struct
    type t = Marked_diff2.V1.t list
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 6bfb1860a65a9a4e7c400f76dbd751b5 |}]
    ;;

    let of_model m = m
    let to_model m = m
  end

  module Model = V3

  module V2 = struct
    type t = Diff2.V2.t list
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 6adaad1ab18aad7d705bef2041b3b5c5 |}]
    ;;

    open! Core

    let to_model diff2s : Model.t =
      V3.to_model
        (List.map diff2s ~f:(fun diff2 ->
           (* We use [Internal__fully_reviewed] here because this is going to yield
              potentially less catch-up for feature created before V3. *)
           { Marked_diff2.V1. diff2; mark_kind = Internal__fully_reviewed }))
    ;;

    let of_model (m : Model.t) =
      List.map (V3.of_model m) ~f:(fun (d : Marked_diff2.V1.t) -> d.diff2)
    ;;

  end
end

open! Core
open! Import

let verbose = Verbose.knowledge

module Mark_kind = struct
  include Stable.Mark_kind.Model

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      match t with
      | User
      | Internal__fully_reviewed -> ())
  ;;
end

module Marked_diff2 = struct
  include Stable.Marked_diff2.Model

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~diff2:(check Diff2.invariant)
        ~mark_kind:(check Mark_kind.invariant))
  ;;

  let de_alias { diff2; mark_kind } user_name_by_alias =
    { diff2 = Diff2.de_alias diff2 user_name_by_alias
    ; mark_kind
    }

end

include Stable.Model

let empty : t = []

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    List.iter t ~f:Marked_diff2.invariant)
;;

let review_edges t =
  List.map t ~f:(fun (marked_diff2 : Marked_diff2.t) ->
    Diff2.review_edge marked_diff2.diff2)
  |> Review_edge.Set.of_list
;;

module By_diff2 = Diff2.Ignoring_rev.Table

let by_diff2 (t : t) ~f =
  Hashtbl.map
    (By_diff2.of_alist_multi
       (List.map t ~f:(fun a -> (a.diff2, f a))))
    ~f:List.hd_exn
;;

let diff4s_needed_to_extend_brain (brain : t)
      ~reviewer ~goal ~(could_use : Indexed_diff4s.t) =
  try
    let brain = by_diff2 brain ~f:(fun a -> (a, ref false)) in
    let needed : Diff4s.t ref = ref [] in
    let need diff4 ~output_num_lines =
      needed := { Diff4.And_output_num_lines. diff4; output_num_lines } :: !needed
    in
    (* Get every diff2 in the goal into the reviewer's brain. *)
    List.iter goal ~f:(fun goal_diff2 ->
      let need diff4 =
        need diff4 ~output_num_lines:(Diff2.num_lines_in_diff goal_diff2)
      in
      let already_in_brain =
        match Hashtbl.find brain goal_diff2 with
        | None -> false
        | Some ({ diff2 = brain_diff2; mark_kind = _ }, used) ->
          if Rev.Compare_by_hash.equal brain_diff2.base.rev goal_diff2.base.rev
          && Rev.Compare_by_hash.equal brain_diff2.tip.rev  goal_diff2.tip.rev
          then
            (* This is the case where what we want to know is already what we know (the
               only potential difference between [brain_diff2] and [goal_diff2] is
               [num_lines_in_diff], if the computation of line counts has changed).  So we
               don't need even a rev update, and creating one anyway would result in
               spurious implicitly reviewed diff in the session, and maybe spurious
               sessions if a session would contain nothing else. *)
            (used := true; true)
          else (
            match Diff4.create_rev_update ~from_:brain_diff2 ~to_:goal_diff2 with
            | Error _ -> false
            | Ok diff4 -> used := true; need diff4; true)
      in
      if not already_in_brain
      then (
        let extend_brain =
          match Hashtbl.find could_use.by_output goal_diff2 with
          | None -> None
          | Some could_use ->
            let possibilities =
              List.filter_map could_use ~f:(fun diff4 ->
                match Hashtbl.find brain (Diff4.input diff4 ~num_lines_in_diff:1) with
                | None -> None
                | Some (_, used) -> Some (diff4, used))
            in
            List.reduce possibilities ~f:(fun (d1, u1) (d2, u2) ->
              if Diff4.num_lines d1 reviewer <= Diff4.num_lines d2 reviewer
              then (d1, u1)
              else (d2, u2))
        in
        match extend_brain with
        | None               -> need (Diff4.create_from_scratch_to_diff2 goal_diff2)
        | Some (diff4, used) -> need diff4; used := true));
    (* Remove every diff2 in the reviewer's brain that shouldn't be there. *)
    Hashtbl.iter brain ~f:(fun ({ diff2 = know; mark_kind = _}, used) ->
      if not !used
      then (
        let possibilities =
          match Hashtbl.find could_use.by_input know with
          | None -> None
          | Some diff4s ->
            (* Only use a diff4 if it achieves the goal of removing the diff from the
               reviewer's brain.  Such diffs can either show that the changes done in the
               feature were undone, or show nothing because the same change was done in
               the feature and in the base. *)
            let diff4s =
              List.filter diff4s ~f:(fun diff4 ->
                Diff2.should_automatically_forget
                  (Diff4.output diff4 ~num_lines_in_diff:1))
            in
            List.reduce diff4s ~f:(fun d1 d2 ->
              if Diff4.num_lines d1 reviewer <= Diff4.num_lines d2 reviewer
              then d1
              else d2)
        in
        let diff4 =
          match possibilities with
          | None       -> Diff4.create_forget know
          | Some diff4 -> diff4
        in
        need diff4 ~output_num_lines:0));
    !needed
  with exn ->
    raise_s
      [%sexp
        "Brain.diff4s_needed_to_extend_brain",
        { brain     : t
        ; reviewer  : Reviewer.t
        ; goal      : Diff2s.t
        ; could_use : Indexed_diff4s.t
        ; exn       : Exn.t
        }
      ]
;;

let diff4s_needed_to_extend_brain brain ~reviewer ~goal ~could_use =
  let result = diff4s_needed_to_extend_brain brain ~reviewer ~goal ~could_use in
  if verbose then
    Debug.eprint_s
      [%sexp
        "Brain.diff4s_needed_to_extend_brain_exn",
        [%here],
        { reviewer  : Reviewer.t
        ; brain     : t
        ; goal      : Diff2s.t
        ; could_use : Indexed_diff4s.t
        ; result    : Diff4s.t
        }
      ];
  result
;;

let what_would_be_extended brain =
  lazy (
    let brain = by_diff2 brain ~f:Fn.id in
    fun diff4 ->
      match Hashtbl.find brain (Diff4.input diff4 ~num_lines_in_diff:1) with
      | Some brain_marked_diff2 ->
        Ok (`Extends brain_marked_diff2)
      | None ->
        match Diff4.as_from_scratch_to_diff2 diff4 with
        | Some diff2 -> Ok (`New diff2)
        | None -> Or_error.error "inapplicable diff4" diff4 [%sexp_of: Diff4.t])
;;

let extend (brain : t) ~(with_ : Diff4s.t)
      ~(reviewer : Reviewer.t) ~(mark_kind : Mark_kind.t) =
  try
    let brain = by_diff2 brain ~f:(fun a -> (a, ref false)) in
    let new_brain =
      List.map with_ ~f:(fun { diff4; output_num_lines } ->
        let make_marked_diff2 diff2 ~previous_mark_kind =
          let mark_kind : Mark_kind.t =
            match mark_kind with
            | Internal__fully_reviewed -> mark_kind
            | User ->
              if Diff4.is_implicitly_reviewed diff4 reviewer
              then (
                match previous_mark_kind with
                | None -> Internal__fully_reviewed
                | Some mark_kind -> mark_kind)
              else mark_kind
          in
          { Marked_diff2.
            diff2 = Diff2.with_num_lines diff2 output_num_lines
          ; mark_kind
          }
        in
        match Hashtbl.find brain (Diff4.input diff4 ~num_lines_in_diff:1) with
        | Some (brain_marked_diff2, used) ->
          used := true;
          make_marked_diff2
            (Diff4.output diff4 ~num_lines_in_diff:1)
            ~previous_mark_kind:(Some brain_marked_diff2.mark_kind)
        | None ->
          match Diff4.as_from_scratch_to_diff2 diff4 with
          | Some diff2 -> make_marked_diff2 diff2 ~previous_mark_kind:None
          | None -> raise_s [%sexp "inapplicable diff4", (diff4 : Diff4.t)])
    in
    let untouched_brain =
      List.filter_map (Hashtbl.data brain) ~f:(fun (marked_diff2, used) ->
        if !used
        then None
        else
          Option.return
            (* When an internal mark happens, we reset all the untouched diff2s in the
               brain, this is a new fully reviewed checkpoint. *)
            (match mark_kind with
             | User -> marked_diff2
             | Internal__fully_reviewed ->
               { diff2     = marked_diff2.diff2
               ; mark_kind = Internal__fully_reviewed
               }))
    in
    let new_brain = untouched_brain @ new_brain in
    let new_brain =
      List.filter new_brain ~f:(fun marked_diff2 ->
        not (Diff2.should_automatically_forget marked_diff2.diff2))
    in
    let new_brain = Hashtbl.data (by_diff2 new_brain ~f:Fn.id) in
    invariant new_brain;
    new_brain
  with exn ->
    raise_s
      [%sexp
        "Brain.extend",
        { brain     : t
        ; with_     : Diff4s.t
        ; reviewer  : Reviewer.t
        ; mark_kind : Mark_kind.t
        ; exn       : Exn.t
        }
      ]
;;

let check_diff4s_needed_to_extend_brain_exn (brain : t)
      ~reviewer ~goal ~could_use =
  let needed t = diff4s_needed_to_extend_brain t ~reviewer ~goal ~could_use in
  let session1 = needed brain in
  let session2 =
    needed (extend brain ~with_:session1 ~reviewer ~mark_kind:Internal__fully_reviewed)
  in
  if not (List.is_empty session2)
  then
    raise_s
      [%sexp
        "bug in Brain.diff4s_needed_to_extend_brain",
        { reviewer  : Reviewer.t
        ; brain     : t
        ; goal      : Diff2s.t
        ; could_use : Indexed_diff4s.t
        ; session1  : Diff4s.t
        ; session2  : Diff4s.t
        }
      ]
;;

let de_alias (brain : t) user_name_by_alias =
  List.map brain ~f:(fun marked_diff2 ->
    Marked_diff2.de_alias marked_diff2 user_name_by_alias)
;;
