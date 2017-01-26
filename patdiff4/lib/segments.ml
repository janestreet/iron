open Core
open Import
open Patience_diff_lib.Std

type t = Segment.t list
[@@deriving sexp_of]

let tail list number =
  let len = List.length list in
  snd (List.split_n list (len - number))
;;

let head = List.take
;;

let%test_unit _ =
  let list = List.init 10 ~f:Fn.id in
  let check expr expect =
    [%test_result: int list] expr ~expect
  in
  check (tail list 0)  [];
  check (head list 0)  [];
  check (tail list 1)  [9];
  check (head list 1)  [0];
  check (tail list 2)  [8;9];
  check (tail list 20) list;
  check (head list 2)  [0;1];
  check (head list 20) list
;;

let equal_for_classify lines1 lines2 =
  let format_rules = User_config.get () |> User_config.diff_config in
  let module Patdiff_core = Patdiff_lib.Patdiff_core in
  Patdiff_core.diff ~context:0 ~keep_ws:false
    ~mine:lines1 ~other:lines2
  |> Patdiff_core.refine
       ~output:Patdiff_core.Output.Ansi
       ~rules:format_rules
       ~produce_unified_lines:true
       ~keep_ws:false
       ~split_long_lines:false
  |> List.is_empty
;;

let split_around list n =
  let prev, rest = List.split_n list n in
  let mid  = List.hd_exn rest in
  let next = List.tl_exn rest in
  prev, mid, next
;;

let%test_module "split_around" =
  (module struct
    type result = char list * char * char list [@@deriving sexp, compare]

    let%test_unit _ =
      [%test_result: result]
        (split_around ['a';'b';'c'] 0)
        ~expect:([],'a',['b';'c'])

    let%test_unit _ =
      [%test_result: result]
        (split_around ['a';'b';'c'] 1)
        ~expect:(['a'],'b',['c'])

    let%test_unit _ =
      [%test_result: result]
        (split_around ['a';'b';'c'] 2)
        ~expect:(['a'; 'b'],'c',[])

    let%test _ = is_error (Result.try_with (fun () ->
      split_around ['a';'b';'c'] 3
    ))

    let%test_unit _ =
      [%test_result: result]
        (split_around ['a';'b';'c'] (-1))
        ~expect:([],'a',['b';'c'])
  end)

let of_matches ~verbose ~file_names ~context ~lines_required_to_separate_ddiff_hunks
      diamond matches =
  let segments = Queue.create () in
  let current_segment = ref None in
  let current_rev_common = ref [] in
  let enqueue_segment (segment : Segment.t) =
    (if verbose
     then Debug.eprint_s
            [%sexp "of_matches: enqueue_segment", [%here]
                   , { segment         : Segment.t
                     ; current_segment : (Segment.t * Segment.t) option ref
                     }
            ]);
    match !current_segment with
    | None ->
      let pre_common = tail (List.rev !current_rev_common) context in
      current_rev_common := [];
      current_segment := Some (Segment.prepend pre_common segment, segment);
    | Some (seg1, last_of_seg1) ->
      let seg2 = segment in
      let context_between = List.rev !current_rev_common in
      current_rev_common := [];
      let context_between_len = List.length context_between in
      let diff4_class_of_last_two =
        lazy (
          if Diff4_class.equal last_of_seg1.diff4_class seg2.diff4_class
          then last_of_seg1.diff4_class
          else (
            Diamond.classify ~equal:equal_for_classify
              (Diamond.map2 last_of_seg1.slice seg2.slice ~f:(fun s1 s2 ->
                 List.concat [ s1.lines ; context_between ; s2.lines ]
                 |> Array.of_list))))
      in
      (* Either we merge this segment or we separate it. *)
      if (context_between_len <= context * 2
          && (Segment.is_shown last_of_seg1 || Segment.is_shown seg2))
      || (context_between_len < lines_required_to_separate_ddiff_hunks
          && (Segment.is_shown last_of_seg1 || Segment.is_shown seg2
              || Diff4_class.is_shown (force diff4_class_of_last_two)))
      then (
        let merged =
          (* These two segments are too close, we are going to merge them. *)
          let slice = Diamond.map2 seg1.slice seg2.slice ~f:(fun sl1 sl2 ->
            let { Slice.range = r1; lines = l1 } = sl1 in
            let { Slice.range = r2; lines = l2 } = sl2 in
            { Slice.range = Range.merge r1 r2
            ; lines = List.concat [ l1 ; context_between ; l2 ]
            })
          in
          let diff4_class =
            if Diff4_class.equal seg1.diff4_class seg2.diff4_class
            then seg1.diff4_class
            else
              Diamond.classify ~equal:equal_for_classify
                (Diamond.map slice
                   ~f:(fun slice -> slice |> Slice.lines |> Array.of_list))
          in
          { Segment.slice ; diff4_class }
        in
        current_segment := Some (merged, seg2))
      else (
        (* Distribute the context accordingly. *)
        let split_at = Int.min context (context_between_len / 2) in
        if Segment.is_shown seg1
        then (
          let split_seg1 =
            if Segment.is_shown seg2
            then split_at
            else context
          in
          let seg1 = Segment.append seg1 (head context_between split_seg1) in
          Queue.enqueue segments seg1);
        let split_seg2 =
          if Segment.is_shown seg1
          then split_at
          else context
        in
        let seg2 = Segment.prepend (tail context_between split_seg2) seg2 in
        current_segment := Some (seg2, seg2));
  in
  let finalize () =
    (match !current_segment with
     | None -> ()
     | Some (segment, _) ->
       if Segment.is_shown segment
       then (
         let post_common = head (List.rev !current_rev_common) context in
         Queue.enqueue segments (Segment.append segment post_common)));
    current_segment := None;
    current_rev_common := [];
    Queue.to_list segments
  in
  let enqueue_source_lines match_lines source_lines =
    if not (Diamond.for_all source_lines ~f:List.is_empty)
    then (
      let slice =
        Diamond.map3 file_names match_lines source_lines
          ~f:(fun source line_number lines ->
            Slice.create ~source line_number lines)
      in
      let diff4_class =
        Diamond.classify ~equal:equal_for_classify
          (Diamond.map slice ~f:(fun slice -> slice |> Slice.lines |> Array.of_list))
      in
      enqueue_segment { Segment.slice ; diff4_class });
  in
  let fold_matches (match_lines, source_lines) matches =
    (* Grab the actual lines from matching line numbers. *)
    let prev_lines, mid, next_lines =
      let split =
        Diamond.map3 source_lines matches match_lines
          ~f:(fun source_lines matches match_lines ->
            split_around source_lines (matches - match_lines))
      in
      Diamond.map ~f:fst3 split
    , Diamond.map ~f:snd3 split
    , Diamond.map ~f:trd3 split
    in
    let old_base_mid = Diamond.b1 mid in
    let () =
      try
        [%test_result: string] ~expect:old_base_mid (Diamond.b2 mid);
        [%test_result: string] ~expect:old_base_mid (Diamond.f1 mid);
        [%test_result: string] ~expect:old_base_mid (Diamond.f2 mid);
      with
      | exn ->
        raise_s
          [%sexp
            "test_result failure",
            { exn          : Exn.t
            ; matches      : int Diamond.t
            ; match_lines  : int Diamond.t
            ; source_lines : string list Diamond.t
            ; prev_lines   : string list Diamond.t
            ; mid          : string Diamond.t
            ; next_lines   : string list Diamond.t
            }
          ]
    in
    enqueue_source_lines match_lines prev_lines;
    current_rev_common := old_base_mid :: !current_rev_common;
    (Diamond.map matches ~f:succ), next_lines
  in
  let match_lines, source_lines =
    List.fold matches ~init:(Diamond.singleton 0, diamond) ~f:fold_matches
  in
  enqueue_source_lines match_lines source_lines;
  finalize ()
;;

module Diff = struct
  module type S = sig
    type elt
    val diff_common_sequence : elt array -> elt array -> (int * int) list
  end

  module Make (Elt : Hashtbl.Key) : S with type elt := Elt.t = struct
    module P = Patience_diff.Make (Elt)

    (* NB: Same signature as [Patience_diff.matches], which is the implementation of
       patience common subsequence.
       First attempt was: longest common subsequence. but turns out lcs is too slow O(n2).
       This one is O(ND) with D size of diff *)
    let diff_common_sequence a b =
      if false
      then
        P.matches a b
      else (
        let matches = ref [] in
        Plain_diff.iter_matches ~f:(fun elt -> matches := elt::!matches) a b;
        List.rev !matches)
  end

  module String = Make (String)
end

let%test_module "lcs" =
  (module struct
    module D = Diff.Make (Int)

    let%test _ = List.is_empty (D.diff_common_sequence [||] [||])

    let%test_unit _ =
      [%test_result: (int*int) list]
        (D.diff_common_sequence
           [| 0; 10; 0;     1; 42; 2;  20; 5 |]
           [| 0; 8;  0; 15; 1; 42; 10;     5 |])
        ~expect:[ (0,0); (2,2); (3,4); (4,5); (7,7) ]
    ;;
  end)

let of_files ?(verbose=false) ?(force_should_split_files_in_hunks_for_tests=false)
      ~rev_names ~context ~lines_required_to_separate_ddiff_hunks ~contents:diamond () =
  let file_names = Diamond.pretty_short_rev_names ~equal:String.equal rev_names in
  let lines = Diamond.map diamond ~f:String.split_lines in
  let arrays = Diamond.map lines ~f:Array.of_list in
  (if verbose
   then Debug.eprint_s
          [%sexp "input:", [%here]
                 , (arrays : string Array.t Diamond.t)
          ]);
  let files_diff4_class =
    let equal_for_classify (rev1, lines1) (rev2, lines2) =
      String.equal rev1 rev2 || equal_for_classify lines1 lines2
    in
    Diamond.classify ~equal:equal_for_classify
      (Diamond.map2 rev_names arrays ~f:(fun rev lines -> rev, lines))
  in
  (if verbose
   then Debug.eprint_s
          [%sexp "files_diff4_class", [%here]
                 , (files_diff4_class : Diff4_class.t)
          ]);

  if not (Diff_algo.should_split_files_in_hunks files_diff4_class
          || force_should_split_files_in_hunks_for_tests)
  then
    [ { Segment.
        slice = Diamond.map2 file_names lines ~f:(fun source -> Slice.create ~source 0)
      ; diff4_class  = files_diff4_class
      }
    ]
  else (

    (* PCS = "patience common subsequence" *)
    let pcs geta getb =
      let a_arr = geta arrays in
      let b_arr = getb arrays in
      Patience_diff.String.matches a_arr b_arr
      |> Array.of_list
      |> Array.map ~f:(fun (idx0, idx1) ->
        [%test_eq: string] a_arr.(idx0) b_arr.(idx1);
        a_arr.(idx0), idx0, idx1)
    in

    let pcs_old_base_old_tip = pcs Diamond.b1 Diamond.f1 in
    let pcs_new_base_new_tip = pcs Diamond.b2 Diamond.f2 in

    (if verbose
     then Debug.eprint_s
            [%sexp "PCS", [%here]
                   , { pcs_old_base_old_tip : (string * int * int) array
                     ; pcs_new_base_new_tip : (string * int * int) array
                     }
            ]);

    let matches =
      Diff.String.diff_common_sequence
        (Array.map ~f:fst3 pcs_old_base_old_tip)
        (Array.map ~f:fst3 pcs_new_base_new_tip)
    in

    (if verbose
     then Debug.eprint_s
            [%sexp "matches", [%here]
                   , (matches : (int*int) list)
            ]);

    let indexes_of_matches =
      List.map matches
        ~f:(fun (old_base_old_tip_idx, new_base_new_tip_idx) ->
          let (_, old_base_idx, old_tip_idx) =
            pcs_old_base_old_tip.(old_base_old_tip_idx)
          in
          let (_, new_base_idx, new_tip_idx) =
            pcs_new_base_new_tip.(new_base_new_tip_idx)
          in
          { Diamond.
            b1 = old_base_idx;
            b2 = new_base_idx;
            f1 = old_tip_idx;
            f2 = new_tip_idx;
          }
        )
    in

    (if verbose
     then Debug.amf [%here] "indexes of matches:\n%s"
            (String.concat ~sep:"\n" (List.map indexes_of_matches ~f:(fun d ->
               sprintf "%s %s" ([%sexp_of: int Diamond.t] d |> Sexp.to_string)
                 (Diamond.f2 arrays).(Diamond.f2 d)))));

    of_matches ~verbose ~file_names ~context ~lines_required_to_separate_ddiff_hunks
      lines indexes_of_matches)
;;
