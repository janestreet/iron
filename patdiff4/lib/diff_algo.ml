open Core
open Import
open Patience_diff_lib.Std

let infinite_context = 100_000

module Id = Diff_algo_id

module Block = struct
  type t =
    { hint   : string list
    ; lines  : string list
    }
  [@@deriving sexp_of]
end

module View = struct
  type t =
    { id           : Id.t
    ; blocks       : Block.t list
    }
  [@@deriving fields, sexp_of]
end

type algo =
  include_hunk_breaks:bool
  -> diff4_class:Diff4_class.t
  -> context:int
  -> Slice.t Diamond.t
  -> View.t

type t = Id.t * algo

let sexp_of_t (id, _) = Id.sexp_of_t id
;;

let id : t -> Id.t = fst

let apply : t -> algo = snd
;;


let header_of_conflict_resolution ~context:_ to_range =
  let minus =
    { Header.Source.
      name = "conflict"
    ; other_names = []
    ; range = None
    }
  in
  Header.Diff2
    { minus
    ; plus = Range.to_header ~other_names:[] to_range
    }
;;

let header_of_change ~diff4_class ~full_diff change =
  let { Change.from = (from_node, from_slice) ; to_ = (to_node, to_slice) } = change in
  let names = Diamond.map full_diff ~f:(fun slice -> slice.Slice.range.Range.source) in
  let group_by = Diamond.group names ~by:diff4_class in
  let from_other_names = Diamond.get group_by from_node in
  let to_other_names = Diamond.get group_by to_node in
  let from_range = from_slice |> Slice.range in
  let to_range   = to_slice   |> Slice.range in
  { Header.
    minus = Range.to_header ~other_names:from_other_names from_range
  ; plus  = Range.to_header ~other_names:to_other_names   to_range
  }
;;

let diff_of_change
      ?(refined=true) ?(produce_unified_lines=true) ?format_rules
      ~include_hunk_breaks
      ~header
      ~context change
  =
  let format_rules =
    match format_rules with
    | None -> User_config.get () |> User_config.diff_config
    | Some rules -> rules
  in
  let { Change.from = (_, from_slice) ; to_ = (_, to_slice) } = change in
  let minus = Slice.lines from_slice in
  let plus = Slice.lines to_slice in
  let module Patdiff_core = Patdiff_lib.Patdiff_core in
  Patdiff_core.diff
    ~context
    ~keep_ws:false
    ~mine:(Array.of_list minus)
    ~other:(Array.of_list plus)
  |> (if refined
      then Patdiff_core.refine
             ~output:Patdiff_core.Output.Ansi
             ~rules:format_rules
             ~produce_unified_lines
             ~keep_ws:false
             ~split_long_lines:false
      else Patience_diff.Hunks.unified)
  |> (fun hunks ->
    let lines = ref [] in
    let f_hunk_break =
      if include_hunk_breaks
      then (
        fun a b ->
          let header = Header.add_hunk_break header a b in
          lines := List.rev_append (Header.to_string header) !lines)
      else (fun _ _ -> ())
    in
    Patdiff_core.iter_ansi
      ~rules:format_rules
      ~f_hunk_break
      ~f_line:(fun line -> lines := String.rstrip line :: !lines)
      hunks;
    List.rev !lines
  )
;;

let three_way_diff ~old_base ~new_base ~old_tip =
  List.concat
    [ [ sprintf "<<<<<<< %s" (old_tip |> Slice.range |> Range.source) ]
    ; (old_tip |> Slice.lines)
    ; [ sprintf "||||||| %s" (old_base |> Slice.range |> Range.source) ]
    ; (old_base |> Slice.lines)
    ; [ sprintf "=======" ]
    ; (new_base |> Slice.lines)
    ; [ sprintf ">>>>>>> %s" (new_base |> Slice.range |> Range.source) ]
    ]
;;

let resolve_diff_algo ~include_hunk_breaks ~diff4_class:_
      ~context
      full_diff =
  let { Diamond.
        b1 = old_base
      ; b2 = new_base
      ; f1 = old_tip
      ; f2 = new_tip
      } = full_diff in
  let conflict_lines = three_way_diff ~old_base ~new_base ~old_tip in
  let line_start = 0 in
  let change =
    { Change.
      from = (),
             { Slice.
               lines = conflict_lines
             (* Range isn't used for the patdiff, so just use a dummy value. *)
             ; range =
                 { Range.
                   source =
                     sprintf "3way diff of %s, %s, and %s"
                       (Range.source (Slice.range old_base))
                       (Range.source (Slice.range new_base))
                       (Range.source (Slice.range old_tip))
                 ; line_start
                 ; line_end = List.length conflict_lines
                 }
             }
    ; to_  = (), new_tip
    }
  in
  let header =
    let to_range = Slice.range new_tip in
    header_of_conflict_resolution ~context to_range
  in
  let lines = diff_of_change ~include_hunk_breaks ~header ~context change in
  { View.id = `conflict_resolution
  ; blocks  = [ { Block.hint = [] ; lines } ]
  }
;;

let make_ddiff_algo_fun ~id ?(hint=[]) a b ~include_hunk_breaks ~diff4_class
      ~context
      full_diff =
  let make_change (from, to_) =
    { Change.
      from = from, Diamond.get full_diff from;
      to_  = to_,  Diamond.get full_diff to_;
    }
  in
  let change_A = make_change a in
  let change_B = make_change b in
  let format_rules = User_config.get () |> User_config.ddiff_inner_config in
  let inner_diff ~with_hunk_breaks ~header =
    let context = if with_hunk_breaks then context else infinite_context in
    diff_of_change ~refined:true ~format_rules
      ~include_hunk_breaks:with_hunk_breaks
      ~header:(Header.Diff2 header)
      ~context
  in
  let a_header = header_of_change ~diff4_class ~full_diff change_A in
  let b_header = header_of_change ~diff4_class ~full_diff change_B in
  (* Heuristic: if the ddiff is essentially a diff, we prefer the version with the hunk
     breaks on the inner level. *)
  let with_hunk_breaks_a_inner =
    lazy (inner_diff ~with_hunk_breaks:true ~header:a_header change_A)
  in
  let with_hunk_breaks_b_inner =
    lazy (inner_diff ~with_hunk_breaks:true ~header:b_header change_B)
  in
  let a_inner, b_inner =
    if List.is_empty (force with_hunk_breaks_a_inner)
    || List.is_empty (force with_hunk_breaks_b_inner)
    then (force with_hunk_breaks_a_inner, force with_hunk_breaks_b_inner)
    else (
      let full_context_a_inner =
        inner_diff ~with_hunk_breaks:false ~header:a_header change_A
      in
      let full_context_b_inner =
        inner_diff ~with_hunk_breaks:false ~header:b_header change_B
      in
      full_context_a_inner, full_context_b_inner)
  in
  let make_slice lines = Slice.create ~source:"" 0 lines in
  let change =
    { Change.
      from = (), make_slice a_inner;
      to_  = (), make_slice b_inner;
    }
  in
  let header = Header.Diff4 { minus = a_header ; plus = b_header } in
  let lines =
    let format_rules = User_config.get () |> User_config.ddiff_outer_config in
    diff_of_change
      ~refined:false ~produce_unified_lines:false ~format_rules
      ~include_hunk_breaks ~header ~context change
  in
  { View.id
  ; blocks = [ { Block.hint ; lines } ]
  }
;;

let make_ddiff_algo ?hint id =
  let from, to_ =
    match id with
    | `base_ddiff    -> (`old_base, `new_base), (`old_tip , `new_tip)
    | `feature_ddiff -> (`old_base, `old_tip ), (`new_base, `new_tip)
  in
  let id = (id :> Diff_algo_id.t) in
  id, make_ddiff_algo_fun ~id ?hint from to_
;;

let base_ddiff ?hint () =
  make_ddiff_algo ?hint `base_ddiff
;;

let feature_ddiff ?hint () =
  make_ddiff_algo ?hint `feature_ddiff
;;

(* Diff Algos *)

let build_diff_lines (from, to_) ~include_hunk_breaks ~diff4_class
      ~context full_diff =
  let change =
    { Change.
      from = from, Diamond.get full_diff from;
      to_  = to_,  Diamond.get full_diff to_;
    }
  in
  let header = Header.Diff2 (header_of_change ~diff4_class ~full_diff change )in
  diff_of_change ~include_hunk_breaks ~header ~context change
;;

let build_diff_algo_fun ~id ?(hint=[]) (from, to_) ~include_hunk_breaks ~diff4_class
      ~context full_diff =
  let lines =
    build_diff_lines (from, to_) ~include_hunk_breaks ~diff4_class
      ~context full_diff
  in
  { View.id
  ; blocks = [ { Block.hint ; lines } ]
  }
;;

let build_diff_algo ?hint ((from, to_) as elements) =
  let id = Id.simple_diff ~from ~to_ in
  id, build_diff_algo_fun ~id ?hint elements
;;

let conflict_resolution =
  `conflict_resolution, resolve_diff_algo
;;

let new_base_to_new_tip  ?hint () = build_diff_algo ?hint (`new_base , `new_tip)
let old_base_to_old_tip  ?hint () = build_diff_algo ?hint (`old_base , `old_tip)
let old_base_to_new_base ?hint () = build_diff_algo ?hint (`old_base , `new_base)
let old_base_to_new_tip  ?hint () = build_diff_algo ?hint (`old_base , `new_tip)
let old_tip_to_new_tip   ?hint () = build_diff_algo ?hint (`old_tip  , `new_tip)
;;

(*
   +-----------------------+----------------+-------------------------+
   | equivalence classes   | show           | comment                 |
   |-----------------------+----------------+-------------------------|
   | { B1, B2, F1, F2 }    | nothing        | no changes              |
   | { B1, B2, F1 }        | B2->F2         | new diff                |
   | { B1, B2, F2 }        | F1->F2         | dropped feature change  |
   | { B1, B2 } { F1, F2 } | nothing        | clean merge             |
   | { B1, B2 }            | F1->F2         | diff extension          |
   | { B1, F1, F2 }        | B2->F2         | dropped base change     |
   | { B1, F1 } { B2, F2 } | nothing        | clean merge             |
   | { B1, F1 }            | B2->F2         | new diff                |
   | { B1, F2 } { B2, F1 } | B2->F2         | dropped same change     |
   | { B1, F2 }            | F1->F2, B2->F2 | dropped both changes*   |
   | { B2, F1, F2 }        | nothing        | same change             |
   | { B2, F1 }            | B2->F2         | diff extension          |
   | { B2, F2 }            | B1->F1, F1->F2 | dropped feature change* |
   | { F1, F2 }            | B1->B2, B2->F2 | dropped base change*    |
   | { }                   | ???            | conflict                |
   +-----------------------+----------------+-------------------------+
*)

type shown_as_diff2 =
  [ `b1_b2_f1
  | `b1_b2_f2
  | `b1_b2
  | `b1_f1_f2
  | `b2_f1
  | `b1_f2__b2_f1
  ]

let should_split_files_in_hunks diff4_class =
  match Diff4_class.Shown_class.of_class diff4_class with
  | None -> false
  | Some diff4_class ->
    match diff4_class with
    | #shown_as_diff2 -> false
    | `b1_f1
    | `b1_f2
    | `b2_f2
    | `f1_f2
    | `conflict
      -> true
;;

let story ~dropped_hint ~dropped_elements ~in_favor_hint ~in_favor_elements =
  let fct ~include_hunk_breaks ~diff4_class ~context full_diff =
    let build_diff elements =
      build_diff_lines elements ~include_hunk_breaks ~diff4_class
        ~context full_diff
    in
    { View.id = `story
    ; blocks =
        [ { Block.hint = [ dropped_hint ]
          ; lines = build_diff dropped_elements
          }
        ; { Block.hint = [ in_favor_hint ]
          ; lines = build_diff in_favor_elements
          }
        ]
    }
  in
  `story, fct
;;

let default_ddiff_view = feature_ddiff ()

let all_standard_views =
  [ default_ddiff_view
  ; base_ddiff            ()
  ; old_tip_to_new_tip    ()
  ; new_base_to_new_tip   ()
  ; old_base_to_old_tip   ()
  ; old_base_to_new_base  ()
  ; old_base_to_new_tip   ()
  ; conflict_resolution
  ]
;;

let select_algos_for_review : Diff4_class.Shown_class.t -> t list = function
  | #shown_as_diff2 as diff4_class ->
    let diff2_view =
      match diff4_class with
      | `b1_b2_f1     -> new_base_to_new_tip ()
      | `b1_b2_f2     -> old_tip_to_new_tip  ~hint:[Header.Hint.b1_b2_f2] ()
      | `b1_b2        -> old_tip_to_new_tip  ()
      | `b1_f1_f2     -> new_base_to_new_tip ~hint:[Header.Hint.b1_f1_f2] ()
      | `b2_f1        -> new_base_to_new_tip ()
      | `b1_f2__b2_f1 -> new_base_to_new_tip ~hint:[Header.Hint.b1_f2__b2_f1] ()
    in
    [ diff2_view ]
  | `b1_f1 ->
    [ new_base_to_new_tip () ]
  | `b1_f2        ->
    [ feature_ddiff        ~hint:[Header.Hint.b1_f2] ()
    ; base_ddiff           ~hint:[Header.Hint.b1_f2] ()
    ; old_tip_to_new_tip   ~hint:[Header.Hint.b1_f2] ()
    ; new_base_to_new_tip  ~hint:[Header.Hint.b1_f2] ()
    ]
  | `b2_f2 ->
    let dropped_hint, in_favor_hint = Header.Hint.b2_f2_story in
    [ feature_ddiff        ~hint:[Header.Hint.b2_f2] ()
    ; old_base_to_new_base ~hint:[Header.Hint.b2_f2 ; Header.Hint.b2_f2_kept] ()
    ; story
        ~dropped_hint  ~dropped_elements: (`old_base , `old_tip)
        ~in_favor_hint ~in_favor_elements:(`old_base , `new_base)
    ; old_tip_to_new_tip   ~hint:[Header.Hint.b2_f2] ()
    ; base_ddiff           ~hint:[Header.Hint.b2_f2] ()
    ; old_base_to_old_tip  ~hint:[Header.Hint.b2_f2 ; Header.Hint.b2_f2_dropped] ()
    ]

  | `f1_f2 ->
    let dropped_hint, in_favor_hint = Header.Hint.f1_f2_story in
    [ feature_ddiff       ~hint:[Header.Hint.f1_f2] ()
    ; new_base_to_new_tip ~hint:[Header.Hint.f1_f2 ; Header.Hint.f1_f2_kept] ()
    ; story
        ~dropped_hint  ~dropped_elements: (`old_base , `new_base)
        ~in_favor_hint ~in_favor_elements:(`old_base , `new_tip)
    ; base_ddiff           ~hint:[Header.Hint.f1_f2] ()
    ; old_base_to_new_base ~hint:[Header.Hint.f1_f2 ; Header.Hint.f1_f2_dropped] ()
    ]

  | `conflict     -> all_standard_views
;;

let display_forget ~context slices =
  let diff_algo = old_base_to_old_tip ~hint:[Header.forget] () in
  apply (diff_algo :> t)
    ~include_hunk_breaks:true
    ~diff4_class:`conflict
    ~context
    slices
;;

let display_errors errors =
  if List.is_empty errors then None
  else (
    let diff =
      List.concat_map errors ~f:(fun error ->
        error
        |> Error.to_string_hum
        |> String.split_lines)
    in
    let error_block =
      { Block.
        hint = []
      ; lines = Header.errors :: diff
      }
    in
    let view =
      { View.
        id     = `metadata "errors"
      ; blocks = [ error_block ]
      }
    in
    Some view)
;;

let lines t ~context slices =
  let view =
    apply t
      ~include_hunk_breaks:true
      ~diff4_class:`conflict
      ~context
      slices
  in
  List.concat_map view.blocks ~f:(fun block -> block.lines)
;;
