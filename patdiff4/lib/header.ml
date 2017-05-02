open Core

module Ansi_terminal = struct
  include Textutils.Console.Ansi

  let apply_string attributes string =
    if Iron_common.Iron_options.display_ascii_always
    then string
    else string_with_attr attributes string
  ;;
end

module Source = struct
  type t =
    { name        : string
    ; other_names : string list
    ; range       : (int * int) option
    }

  let add_hunk_break t (pos, len) =
    let range =
      match t.range with
      | Some (base, _) -> base+pos, base+pos+len
      | None -> pos, pos+len
    in
    { t with range = Some range }
  ;;

  let sort_heuristic list =
    let value = function
      | "old base" -> 1
      | "base"     -> 2
      | "old tip"  -> 3
      | "new base" -> 4
      | "tip"      -> 5
      | "new tip"  -> 6
      | _          -> 10
    in
    list
    |> List.map ~f:(fun v -> value v, v)
    |> List.sort ~cmp:[%compare: int * string]
    |> List.map ~f:snd
  ;;

  let filename_and_lines t =
    let name =
      t.other_names
      |> List.filter ~f:(fun name -> not (String.equal name t.name))
      |> List.dedup_and_sort ~compare:String.compare
      |> sort_heuristic
      |> (fun names -> names @ [ t.name ]) (* the last name is followed by its range *)
      |> String.concat ~sep:", "
    in
    let range =
      match t.range with
      | None -> ""
      | Some (a, b) -> sprintf "%d,%d" a b
    in
    name, range
  ;;
end

let separator =
  let module A = Ansi_terminal in
  "@@@@@@@@"
  |> A.apply_string [ `Bright; `Blue ]
;;

let center size string =
  let string = sprintf " %s " string in
  let len = String.length string in
  let left = max 0 ((size - len) / 2) in
  let right = max 0 (size - left - len) in
  String.make left '@' ^ string ^ String.make right '@'
;;

let filename_bar_size = 84
;;

let filename_header ~filename =
  let module A = Ansi_terminal in
  let str = center filename_bar_size filename in
  String.length str, A.apply_string [ `Bright; `Blue ] str
;;

let filename_separator ~length =
  let module A = Ansi_terminal in
  String.make length '@'
  |> A.apply_string [ `Bright; `Blue ]
;;

let errors =
  let module A = Ansi_terminal in
  String.concat ~sep:" " [
    separator;
    "Errors"  |> A.apply_string [ `Red ];
    separator;
  ]
;;

let make_hint str =
  let module A = Ansi_terminal in
  String.concat ~sep:" " [
    separator;
    str |> A.apply_string [ `Magenta ];
    separator;
  ]
;;

let title = make_hint

let forget =
  make_hint "\
Forget this diff -- this file no longer has a diff you should know"
;;

module Hint = struct
  let b1_b2_f2 =
    make_hint "A change in the feature was reverted"
  ;;
  let b1_f1_f2 =
    make_hint "A change present only in the new-base was dropped"
  ;;
  let b1_f2__b2_f1 =
    make_hint "The same change from the old-tip and the new-base was dropped"
  ;;
  let b1_f2 =
    make_hint "Diverging changes in the old-tip and the new-base were both dropped"
  ;;
  let b2_f2_story =
    make_hint "This feature change was dropped... :"
  , make_hint "... in favor of this base change:"
  ;;
  let b2_f2 =
    make_hint "A feature change was dropped in favor of a base change"
  ;;
  let b2_f2_dropped =
    make_hint "The following feature change was dropped:"
  ;;
  let b2_f2_kept =
    make_hint "The following base change was kept:"
  ;;
  let f1_f2_story =
    make_hint "This base change was dropped... :"
  , make_hint "... in favor of this feature change:"
  ;;
  let f1_f2 =
    make_hint "A base change was dropped in favor of a feature change"
  ;;
  let f1_f2_dropped =
    make_hint "The following base change was dropped:"
  ;;
  let f1_f2_kept =
    make_hint "The following feature change was kept:"
  ;;
end

type 'a diff =
  { minus : 'a
  ; plus  : 'a
  }

module Diff2 = struct
  type t = Source.t diff

  let add_hunk_break t minus plus =
    { minus = Source.add_hunk_break t.minus minus
    ; plus  = Source.add_hunk_break t.plus  plus
    }
  ;;

  let to_string t =
    let minus_file, minus_lines = Source.filename_and_lines t.minus in
    let plus_file, plus_lines   = Source.filename_and_lines t.plus in
    let module A = Ansi_terminal in
    String.concat ~sep:" "
      [ separator
      ; (minus_file       |> A.apply_string [ `Red ])
      ; (minus_lines      |> A.apply_string [ `Blue ])
      ; (plus_file        |> A.apply_string [ `Green ])
      ; (plus_lines       |> A.apply_string [ `Blue ])
      ; separator
      ]
  ;;
end

module Diff4 = struct
  type t = Diff2.t diff

  let add_hunk_break t minus plus =
    { minus = Diff2.add_hunk_break t.minus minus plus
    ; plus  = Diff2.add_hunk_break t.plus  minus plus
    }
  ;;

  let to_string (t : t) =
    let { minus = a ; plus = b } = t in
    let minus_a_file , minus_a_lines = Source.filename_and_lines a.minus in
    let minus_b_file , minus_b_lines = Source.filename_and_lines b.minus in
    let plus_a_file  , plus_a_lines  = Source.filename_and_lines a.plus  in
    let plus_b_file  , plus_b_lines  = Source.filename_and_lines b.plus  in
    let module A = Ansi_terminal in
    let dd color = [ `Bright ; `Bg color ; `White ] in
    [ String.concat ~sep:" "
        [ separator
        ; ("--"             |> A.apply_string (dd `Magenta))
        ; (minus_a_file     |> A.apply_string [ `Red ])
        ; (minus_a_lines    |> A.apply_string [ `Blue ])
        ; (plus_a_file      |> A.apply_string [ `Green ])
        ; (plus_a_lines     |> A.apply_string [ `Blue ])
        ; separator
        ]
    ; String.concat ~sep:" "
        [ separator
        ; ("++"            |> A.apply_string (dd `Cyan))
        ; (minus_b_file     |> A.apply_string [ `Red ])
        ; (minus_b_lines    |> A.apply_string [ `Blue ])
        ; (plus_b_file      |> A.apply_string [ `Green ])
        ; (plus_b_lines     |> A.apply_string [ `Blue ])
        ; separator
        ]
    ]
  ;;
end

type t =
  | Diff2 of Diff2.t
  | Diff4 of Source.t diff diff
;;

let add_hunk_break t minus plus =
  match t with
  | Diff2 diff2 -> Diff2 (Diff2.add_hunk_break diff2 minus plus)
  | Diff4 diff4 -> Diff4 (Diff4.add_hunk_break diff4 minus plus)
;;

let to_string = function
  | Diff2 diff2 -> [ Diff2.to_string diff2 ]
  | Diff4 diff4 -> Diff4.to_string diff4
;;
