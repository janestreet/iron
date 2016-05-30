open Core.Std
open Import

module View_ids_computed = struct
  type t =
    | Compute_every_view_available
    | Compute_only_by_diff4_class of Diff_algo_id.Set.t Diff4_class.Map.t
    | Compute_only_in_that_order of Diff_algo_id.t list
end

let process_segment
      ?(view_ids_computed=View_ids_computed.Compute_every_view_available)
      ~include_hunk_breaks
      ~context (segment:Segment.t) shown_class =
  let full_diff = segment.slice in
  let algos =
    let algos = Diff_algo.select_algos_for_review shown_class in
    match view_ids_computed with
    | Compute_every_view_available -> algos
    | Compute_only_in_that_order list ->
      let by_id =
        algos
        |> List.map ~f:(fun algo -> Diff_algo.id algo, algo)
        |>  Diff_algo.Id.Map.of_alist_multi
      in
      List.concat_map list ~f:(fun id ->
        Option.value ~default:[] (Map.find by_id id))
    | Compute_only_by_diff4_class map ->
      match Map.find map (shown_class :> Diff4_class.t) with
      | None -> algos
      | Some set ->
        List.filter algos ~f:(fun algo -> Set.mem set (Diff_algo.id algo))
  in
  List.map algos ~f:(fun diff_algo ->
    Diff_algo.apply diff_algo
      ~include_hunk_breaks
      ~diff4_class:segment.diff4_class
      ~context
      full_diff
  )
;;

let hunks
      ?verbose
      ?view_ids_computed
      ~include_hunk_breaks
      ~rev_names ~file_names ~header_file_name ~context
      ~scrutiny ~contents () =
  Segments.of_files ?verbose ~rev_names ~context ~contents ()
  |> List.filter_map ~f:(fun (segment : Segment.t) ->
    match Diff4_class.Shown_class.of_class segment.diff4_class with
    | None -> None
    | Some shown_class ->
      let views =
        process_segment
          ?view_ids_computed
          ~include_hunk_breaks
          ~context segment shown_class
      in
      if List.is_empty views
      then None
      else
        Some { Hunk.
               header_file_name
             ; scrutiny
             ; rev_names
             ; file_names
             ; diff4_class = segment.diff4_class
             ; views
             ; view_ids_shown = All
             })
;;

let to_lines
      ?verbose
      ~view_ids_shown
      ~rev_names ~file_names ~header_file_name ~context
      ~scrutiny ~contents () =
  let view_ids_computed : View_ids_computed.t =
    match (view_ids_shown : Hunk.View_ids_shown.t) with
    | All -> Compute_every_view_available
    | Only list -> Compute_only_in_that_order list
  in
  hunks ?verbose
    ~view_ids_computed
    ~include_hunk_breaks:true
    ~rev_names ~file_names ~header_file_name
    ~context ~scrutiny ~contents ()
  |> List.map ~f:Hunk.to_lines
;;

let compute_only_default_views_when_counting_lines =
  View_ids_computed.Compute_only_by_diff4_class
    (Map.map User_config.default_view_configuration ~f:Diff_algo_id.Set.of_list)
;;

let num_lines_to_review ~contents =
  hunks
    ~verbose:false
    ~context:0 ~contents
    ~view_ids_computed:compute_only_default_views_when_counting_lines
    ~rev_names:Diamond.pretty_short_rev_names_const
    ~file_names:Diamond.pretty_short_rev_names_const
    ~header_file_name:"new-tip"
    ~scrutiny:None
    ~include_hunk_breaks:false
    ()
  |> List.sum (module Int) ~f:Hunk.num_lines_to_review
;;

let diff
      ?verbose
      ~view_ids_shown
      ~rev_names ~file_names ~header_file_name ~context ~contents () =
  to_lines
    ?verbose
    ~view_ids_shown
    ~rev_names ~file_names ~header_file_name ~context
    ~scrutiny:None ~contents ()
  |> List.concat_map ~f:(List.concat_map ~f:(fun (hunk : Hunk.Lines.t) -> hunk.lines))
;;

let emacs_diff ?verbose ~rev_names ~file_names ~header_file_name ~context ~contents () =
  to_lines
    ?verbose ~view_ids_shown:All
    ~rev_names ~file_names ~header_file_name ~context
    ~scrutiny:None ~contents ()
  |> Structured_elisp.create
;;

let hunks
      ?verbose
      ?view_ids_computed
      ~rev_names ~file_names ~header_file_name ~context
      ~scrutiny ~contents () =
  hunks
    ?verbose
    ~include_hunk_breaks:true
    ?view_ids_computed
    ~rev_names ~file_names ~header_file_name ~context
    ~scrutiny ~contents ()
;;
