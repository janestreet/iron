open! Core
open! Import

module Index = struct
  type t =
    { current : int
    ; total   : int
    }
  [@@deriving sexp_of]

  let to_string_hum t = sprintf "%d/%d" (succ t.current) t.total
  ;;

  let with_space_if_non_trivial t =
    if t.total <= 1
    then ""
    else " " ^ to_string_hum t
  ;;
end

module View_ids_shown = struct
  type t =
    | All
    | Only of Diff_algo.Id.t list
  [@@deriving sexp]
end

module Nesting : sig
  type t
  val lines      : string list -> t
  val block      : string list -> t
  val group      : t list -> t
  val to_strings : t -> string list
end = struct


  type t =
    | Lines of string list
    | Group of t list
  ;;

  let lines lines = Lines lines
  ;;

  let block lines = Group [ Lines lines ]
  ;;

  let group group = Group group
  ;;

  (* We want to use the syntax only if there is at least two elements to group
     together. This allow not to modify the existing views in case there is no need to
     use nesting *)
  let simplify t =
    let rec simplify_in_depth t =
      match t with
      | Lines _ -> t
      | Group [ Group _ as t ] -> simplify_in_depth t
      | Group [] -> Lines []
      | Group [ Lines _ as lines ; Group group ] ->
        (match group with
         | [ ]   -> Group [ lines ]
         | [ t ] -> simplify_in_depth (Group [ lines ; t ])
         | _::_ ->
           let group = List.map group ~f:simplify_in_depth in
           if List.for_all group
                ~f:(function Group _ | Lines [] -> true | Lines (_::_) -> false)
           then
             Group (lines :: group)
           else
             Group [ lines ; Group group ])
      | Group group ->
        Group (List.map group ~f:simplify_in_depth)
    in
    let rec simplify_top t =
      match t with
      | Group group ->
        (match group with
         | [ t ] -> simplify_top t
         | [] | _::_ -> group)
      | Lines _ -> [ t ]
    in
    simplify_top (simplify_in_depth t)
  ;;

  let to_strings t ~pipe ~open_ ~close =
    let rec loop t ~nesting =
      let prefix = List.init nesting ~f:(fun (_ : int) -> pipe) |> String.concat in
      match t with
      | Lines lines -> List.map lines ~f:(fun line -> prefix ^ line)
      | Group children ->
        List.concat
          [ [ prefix ^ open_ ]
          ; List.concat_map children ~f:(loop ~nesting:(nesting + 1))
          ; [prefix ^ close]
          ]
    in
    List.concat_map (simplify t) ~f:(loop ~nesting:0)
  ;;

  let to_strings_ascii = to_strings ~pipe:"| " ~open_:"_" ~close:"|_"

  let%test_module _ =
    (module struct
      let to_strings = to_strings_ascii
      let%test_unit _ =
        let block notice lines = Group [ Lines notice ; Lines lines ] in
        let with_header ~header group = Group (Lines header :: group) in
        let view = with_header in
        let hunk = with_header in
        [%test_result: string]
          (to_strings
             (hunk ~header:["hello hunk"]
                [ view ~header:["story"]
                    [ block ["this feature change was dropped:"]
                        [ "old-base to old-tip"
                        ]
                    ; block [ "... and replaced by this base change:" ]
                        [ "old-base to { new-base, new-tip }"
                        ]
                    ]
                ; view ~header:["feature-ddiff"]
                    [ block []
                        [ "some lines"
                        ]
                    ]
                ; view ~header:["conflict"]
                    [ block []
                        [ "some lines"
                        ]
                    ]
                ]) |> String.concat ~sep:"\n")
          ~expect:"\
hello hunk
_
| story
| _
| | this feature change was dropped:
| | old-base to old-tip
| |_
| _
| | ... and replaced by this base change:
| | old-base to { new-base, new-tip }
| |_
|_
_
| feature-ddiff
| _
| | some lines
| |_
|_
_
| conflict
| _
| | some lines
| |_
|_"
    end)

  let to_strings_utf8 =
    let module Table_char = Textutils.Ascii_table.Table_char in
    to_strings
      ~pipe: (Table_char.connect ~top:  () ~bottom:() ()).utf8
      ~open_:(Table_char.connect ~right:() ~bottom:() ()).utf8
      ~close:(Table_char.connect ~top:  () ~right: () ()).utf8
  ;;

  let to_strings =
    if Iron_options.display_ascii_always
    then to_strings_ascii
    else to_strings_utf8
  ;;
end

type t =
  { header_file_name          : string
  ; scrutiny                  : File_scrutiny.t option
  ; rev_names                 : string Diamond.t
  ; file_names                : string Diamond.t
  ; diff4_class               : Diff4_class.t
  ; views                     : Diff_algo.View.t list
  ; view_ids_shown            : View_ids_shown.t
  }
[@@deriving fields, sexp_of]

let nested_views ?hunk_name t =
  let block_to_lines ?(header=[]) (block : Diff_algo.Block.t) =
    Nesting.block
      (List.concat
         [ header
         ; block.hint
         ; block.lines
         ])
  in
  let number_of_views = List.length t.views in
  let indexed_views = List.mapi t.views ~f:(fun i view ->
    { Index.current = i ; total = number_of_views }, view)
  in
  let views_shown =
    match t.view_ids_shown with
    | All -> indexed_views
    | Only shown_ids ->
      let views_by_id =
        Diff_algo.Id.Map.of_alist_multi
          (List.map indexed_views ~f:(fun (index, view) -> view.id, (index, view)))
      in
      List.concat_map shown_ids ~f:(fun id ->
        Option.value ~default:[] (Map.find views_by_id id))
  in
  let number_of_views_shown = List.length views_shown in
  let is_incomplete_view = number_of_views_shown < number_of_views in
  List.map views_shown ~f:(fun (index, view) ->
    let id = Diff_algo.View.id view in
    let view_name = Diff_algo.Id.to_string id in
    let header =
      if number_of_views_shown > 1
      || is_incomplete_view
      || not (Diff_algo.Id.is_simple_diff id)
      then (
        let hunk_name =
          (* We decide to repeat the hunk label only if there are more than one view so
             because there might be a lot of space between the last time we saw the
             hunk number. If there is only one view, the hunk name is printed right
             above so we should not bother writing it again *)
          match hunk_name with
          | Some name when number_of_views_shown > 1 -> name ^ " "
          | Some _ | None -> ""
        in
        Some [ Header.title
                 (sprintf "%sView%s : %s"
                    hunk_name (Index.with_space_if_non_trivial index) view_name) ])
      else None
    in
    let blocks =
      match view.blocks with
      | [ block ] -> [ block_to_lines ?header block ]
      | ([] | _::_) as blocks ->
        let blocks = List.map blocks ~f:block_to_lines in
        match header with
        | None -> blocks
        | Some lines -> Nesting.lines lines :: blocks
    in
    view, Nesting.group blocks)
;;

let align_alist alist =
  let max_len =
    List.fold alist ~init:0
      ~f:(fun acc (label, _) -> max acc (String.length label))
  in
  List.map alist ~f:(fun (label, data) ->
    sprintf "%-*s = %s" max_len label data
  )
;;

let file_and_rev_names_information ~use_file_separator ~in_scope t =
  let shall_include_rev_names =
    match in_scope with
    | None -> true
    | Some scope ->
      not (Diamond.for_all2 ~f:String.equal scope.rev_names t.rev_names)
  in
  let shall_include_file_names =
    match in_scope with
    | None -> true
    | Some scope ->
      not (Diamond.for_all2 ~f:String.equal scope.file_names t.file_names)
  in
  let length, filename = Header.filename_header ~filename:t.header_file_name in
  let filename_separator = Header.filename_separator ~length in
  let files_and_revs =
    List.concat
      [ (if shall_include_file_names
         then (
           match t.scrutiny with
           | None -> []
           | Some scrutiny ->
             [ sprintf "scrutiny %s" (File_scrutiny.to_string_hum scrutiny) ])
         else []
        )
      ; (if shall_include_file_names
         then (
           let files = Diamond.pretty_short_description ~label:"file" t.file_names in
           if List.length files = 1 then [] else align_alist files)
         else []
        )
      ; (if shall_include_rev_names
         then (
           let revs = Diamond.pretty_short_description ~label:"" t.rev_names in
           [ String.concat ~sep:" | "
               (List.map revs ~f:(fun (name, rev) -> name^" "^rev)) ])
         else []
        )
      ]
  in
  let lines = files_and_revs in
  let lines =
    if shall_include_file_names
    then filename :: lines
    else lines
  in
  let lines =
    if shall_include_file_names && use_file_separator
    then filename_separator :: lines
    else lines
  in
  lines
;;

let list_to_lines hunks =
  let use_file_separator =
    List.map hunks ~f:(fun hunk -> hunk.header_file_name)
    |> String.Hash_set.of_list
    |> Hash_set.length
    |> fun x -> x > 1
  in
  let number_of_hunks = List.length hunks in
  let hunks =
    List.mapi hunks
      ~f:(fun current t -> { Index.current ; total = number_of_hunks }, t)
  in
  let (_in_scope, blocks) =
    List.fold hunks ~init:(None,[]) ~f:(fun (in_scope, acc) (index, t) ->
      let hunk_name =
        if number_of_hunks > 1
        then Some (sprintf "Hunk %s" (Index.to_string_hum index))
        else None
      in
      let views =
        let views = List.map (nested_views ?hunk_name t) ~f:snd in
        match hunk_name with
        | Some title -> Nesting.lines [Header.title title] :: views
        | None -> views
      in
      let file_separator =
        Nesting.lines
          (file_and_rev_names_information ~use_file_separator ~in_scope t)
      in
      let this_nesting = Nesting.group views in
      (Some t, this_nesting :: file_separator :: acc)
    )
  in
  blocks
  |> List.rev
  |> Nesting.group
  |> Nesting.to_strings
;;

let num_lines_to_review t =
  let is_shown =
    match t.view_ids_shown with
    | All -> const true
    | Only ids ->
      let set = Diff_algo.Id.Set.of_list ids in
      (fun id -> Set.mem set id)
  in
  List.sum (module Int) t.views ~f:(fun view ->
    if not (is_shown view.id)
    then 0
    else List.sum (module Int) view.blocks ~f:(fun block -> List.length block.lines)
  )
;;
