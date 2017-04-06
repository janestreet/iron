module Stable = struct

  open Core.Core_stable
  open Iron_common.Stable

  module Raw = struct
    module V1 = struct
      (* [content] is the text of the CR with comment markers removed from the beginning
         and end (if applicable).

         [start_line, start_col] is the two-dimensional start position of the whole
         comment in [path]. *)
      type t =
        { path                       : Relpath.V1.t
        ; content                    : string
        ; start_line                 : int
        ; start_col                  : int
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b24c1aae61acf7c3bd5b62eca34653c7 |}]
      ;;
    end
  end

  module Due = struct
    module V1 = struct
      type t =
        | Now
        | Soon
        | Someday
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| aa93e89d589eeb6e77ac61283a8745af |}]
      ;;
    end
  end

  module Assignee = struct
    module V1 = struct
      type t =
        | This of Unresolved_name.V1.t
        | Feature_owner
        | Missing_file_owner
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| a1426261ff07fcf6a68da510a963f6cb |}]
      ;;
    end
  end

  module Processed = struct
    module V1 = struct
      (* [reported_by] is [user] in [CR user...].  It is an [option] because the text
         might not have a valid user name.

         [for_] is [user2] in [CR user1 for user2: ...].

         Names stored in [Processed.t] have not yet been dealiased, so they are stored as
         [Unresolve_name.t]s. *)
      type t =
        { raw         : Raw.V1.t
        ; reported_by : Unresolved_name.V1.t option
        ; for_        : Unresolved_name.V1.t option
        ; due         : Due.V1.t
        ; is_xcr      : bool
        ; assignee    : Assignee.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 10ba8f1052a7d3b0cd60d709e6b8ebdd |}]
      ;;
    end
  end

  module Cr_soon = struct
    module V1 = struct
      module Unshared = struct
        type t =
          { cr_comment                         : Processed.V1.t
          ; digest_of_condensed_content        : Digest.V1.t
          ; hash_of_path_and_condensed_content : int
          }
        [@@deriving bin_io, compare, fields, sexp]

        let module_name = "Cr_soon"
        let hash t = t.hash_of_path_and_condensed_content
      end
      include Hash_consing.Make_stable_private (Unshared) ()

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| bf3185f7d51eee300dceb9d50f0f8503 |}]
      ;;
    end
  end

  module Summary = struct
    module V1 = struct
      module Row = struct
        type t =
          { assignee : User_name.V1.t
          ; crs      : int
          ; xcrs     : int
          }
        [@@deriving bin_io, compare, fields, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 1b65857d2828b3f9c600c9e4bf41f909 |}]
        ;;
      end
      (* invariant: Rows are in order of descending crs+xcrs and the total row is absent. *)
      type t = Row.t list [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 458ab83486389ac20003b8e1a8f72610 |}]
      ;;
    end
  end

  module V1 = struct
    module Unshared = struct
      type t =
        | Raw of Raw.V1.t
        | Processed of Processed.V1.t
      [@@deriving bin_io, compare, sexp]

      let module_name = "Cr"
      let hash (t:t) = Hashtbl.hash t
    end
    include Hash_consing.Make_stable_private (Unshared) ()

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| a1b782a92f95f253f46c764f1673ac7e |}]
    ;;
  end

end

open! Core
open! Async
open! Import

let verbose = Verbose.cr_comment

let cr_pattern_re2   = "\\bX?CR[-v: \\t]"
let cr_pattern_egrep = cr_pattern_re2

module Raw : sig

  include module type of struct include Stable.Raw.V1 end

  module For_sorted_output : sig
    type nonrec t = t [@@deriving compare]
  end

  module With_file_positions : sig
    type nonrec t =
      { content_start : int
      ; start_index   : int
      ; end_index     : int
      ; cr            : t
      }
    [@@deriving sexp_of]

    val extract
      :  ?extra_cr_comment_headers:string list
      -> Cr_comment_format.t
      -> path          : Relpath.t
      -> file_contents : string
      -> t list
  end

end = struct

  include Stable.Raw.V1

  module For_sorted_output = struct

    type nonrec t = t

    let compare t1 t2 =
      let c = Relpath.compare t1.path t2.path in
      if c <> 0
      then c
      else (
        let c = Int.compare t1.start_line t2.start_line in
        if c <> 0
        then c
        else Int.compare t1.start_col t2.start_col)
    ;;

    let compare t1 t2 =
      let c = compare t1 t2 in
      if verbose then Debug.ams [%here] "compare" (t1, t2, c) [%sexp_of: t * t * int];
      c
    ;;
  end

  let cr_regex = Regex.create_exn cr_pattern_re2

  let slice_after contents last_excluded_index =
    (* the longest suffix not containing [last_excluded_index] *)
    String.slice contents (last_excluded_index + 1) 0
  ;;

  (* several functions in Regex take a [sub] argument, but this is going to be [`Index 0]
     everywhere in this module, indicating that we're interested in the entire match *)
  let sub = `Index 0

  let find_ml_end =
    let regex = Regex.create_exn "\\(\\*|\\*\\)" in
    fun file_contents start_pos ->
      let rec nest_comments ~depth current_pos =
        let rest_of_file = slice_after file_contents current_pos in
        match Regex.get_matches_exn ~max:1 regex rest_of_file with
        | [] -> None
        | m :: _ ->
          let match_start, match_len = Regex.Match.get_pos_exn ~sub m in
          let next_pos = current_pos + match_start + match_len in
          match Regex.Match.get_exn ~sub m with
          | "(*" -> nest_comments ~depth:(depth + 1) next_pos
          | "*)" ->
            if depth = 0
            then Some (next_pos)
            else nest_comments ~depth:(depth - 1) next_pos
          | _ -> failwith "ML regex matched something other than an ML comment start/end!"
      in
      nest_comments ~depth:0 start_pos
  ;;

  let find_non_nesting_end end_regex file_contents start_pos =
    let partial_contents = slice_after file_contents start_pos in
    match Regex.get_matches_exn ~max:1 end_regex partial_contents with
    | [] -> None
    | m :: _ ->
      let pos, len = Regex.Match.get_pos_exn ~sub m in
      Some (start_pos + pos + len)
  ;;

  let find_line_comment_end regex file_contents start_pos =
    match Regex.get_matches_exn ~max:1 regex (slice_after file_contents start_pos) with
    | [] ->
      (* reached EOF without finding a match, so block ends at EOF *)
      String.length file_contents - 1
    | m :: _ ->
      let newline_pos, _ = Regex.Match.get_pos_exn ~sub m in
      start_pos + newline_pos
  ;;

  let find_comment_bounds =
    (* These end regexes are for matching the ends of block comments. *)
    let ml_end_regex = Regex.create_exn "\\*+\\)$" in
    let c_end_regex  = Regex.create_exn "\\*+/" in
    let xml_end_regex = Regex.create_exn "-->" in
    (* The line regexes match lines that DON'T start with a line comment marker. *)
    (* matches any character except [c], space, and tab *)
    let not_char c = "[^" ^ Char.to_string c ^ " \\t]" in
    (* newline, followed by any amount of space *)
    let line_start = "\\n[ \\t]*" in
    let sh_regex   = Regex.create_exn (line_start ^ not_char '#') in
    let lisp_regex = Regex.create_exn (line_start ^ not_char ';') in
    (* The C line complicates things since it's two characters long.  The idea is that the
       line can optionally start with '/', but that can't be followed by another '/'.  *)
    let c_line_regex = Regex.create_exn (line_start ^ "/?" ^ not_char '/') in
    let sql_regex  = Regex.create_exn (line_start ^ "-?" ^ not_char '-') in
    fun current_format file_contents content_start_pos ->
      let end_block kind comment_start_pos =
        let find_end, end_regex =
          match kind with
          | `ml -> find_ml_end, ml_end_regex
          | `c  -> find_non_nesting_end c_end_regex, c_end_regex
          | `xml -> find_non_nesting_end xml_end_regex, xml_end_regex
        in
        match find_end file_contents content_start_pos with
        | None -> None
        | Some end_pos ->
          (* string from "X?CR" to end of comment (including comment ender) *)
          let raw_contents = String.slice file_contents content_start_pos (end_pos + 1) in
          (* remove the comment ender *)
          let contents = Regex.rewrite_exn end_regex raw_contents ~template:"" in
          Some (comment_start_pos, end_pos, contents)
      in
      let end_lines regex comment_start_pos =
        let end_pos = find_line_comment_end regex file_contents content_start_pos in
        let contents = String.slice file_contents content_start_pos (end_pos + 1) in
        comment_start_pos, end_pos, contents
      in
      let after_format format =
        Cr_comment_format.is_after_format current_format ~format
      in
      (* Works backwards from "X?CR" to find a comment starter. *)
      let rec check_backwards ~last pos =
        let end_lines regex = Some (end_lines regex (pos + 1)) in
        if pos < 0
        then (
          match last with
          | `semi -> end_lines lisp_regex
          | `hash -> end_lines sh_regex
          | `slashes n -> if n >= 2 then end_lines c_line_regex else None
          | `dashes n -> if n >= 2 then end_lines sql_regex else None
          | `star | `not_special -> None)
        else (
          let curr_char = file_contents.[pos] in
          let check_backwards last = check_backwards ~last (pos - 1) in
          match last, curr_char with
          | `star, '*' -> check_backwards `star
          | `star, '/' -> end_block `c  pos (* found /* *)
          | `star, '(' -> end_block `ml pos (* found "(*" (* "*)" *) *)
          | `star, _ -> None
          | `slashes n, '/' -> check_backwards (`slashes (n + 1))
          | `slashes n, _ ->
            if n >= 2 then end_lines c_line_regex (* found //+ *) else None
          | `semi, ';' -> check_backwards `semi
          | `semi, _ -> end_lines lisp_regex (* found ;+ *)
          | `hash, '#' -> check_backwards `hash
          | `hash, _ -> end_lines sh_regex (* found #+ *)
          | `dashes n, '-' -> check_backwards (`dashes (n + 1))
          | `dashes n, '!'
            (* checking if we found <!-- *)
            when n >= 2 && pos > 0 && Char.(=) '<' file_contents.[pos - 1] ->
            end_block `xml (pos - 1)
          | `dashes n, _ -> if n >= 2 then end_lines sql_regex else None
          | `not_special, '/' -> check_backwards (`slashes 1)
          | `not_special, '*' -> check_backwards `star
          | `not_special, ';' -> check_backwards `semi
          | `not_special, '#' -> check_backwards `hash
          | `not_special, '-' when after_format V2_sql_xml -> check_backwards (`dashes 1)
          | `not_special, (' ' | '\t' | '\n') -> check_backwards `not_special
          | `not_special, _ -> None)
      in
      check_backwards ~last:`not_special (content_start_pos - 1)
  ;;

  let index_to_2d_pos file_contents =
    (* Maps newline positions (indices of file_contents) to the number of the line they
       begin. *)
    let map, _last_line =
      let init = (Int.Map.singleton (-1) 1, 1) in
      String.foldi file_contents ~init ~f:(fun pos ((map, prev_line) as acc) c ->
        if Char.equal c '\n'
        then (
          let curr_line = prev_line + 1 in
          Map.add map ~key:pos ~data:curr_line, curr_line)
        else
          acc)
    in
    stage (fun index ->
      match Map.closest_key map `Less_than index with
      | None -> failwith "gave a negative input to index_to_2d_pos"
      | Some (newline_index, line_num) -> line_num, index - newline_index)
  ;;

  module With_file_positions = struct

    type nonrec t =
      { content_start : int
      ; start_index   : int
      ; end_index     : int
      ; cr            : t
      }
    [@@deriving sexp_of]

    let extract ?extra_cr_comment_headers format ~path ~file_contents =
      let regex =
        Option.value_map extra_cr_comment_headers ~default:cr_regex ~f:(fun l ->
          ksprintf Regex.create_exn "\\b(%s)[-v: \\t]"
            ("X?CR" :: List.map l ~f:Regex.escape |> String.concat ~sep:"|"))
      in
      let ms = Regex.get_matches_exn regex file_contents in
      let pos_2d = lazy (unstage (index_to_2d_pos file_contents)) in
      List.filter_map ms ~f:(fun m ->
        let open Option.Let_syntax in
        let cr_start, _ = Regex.Match.get_pos_exn ~sub m in
        let%map (start_index, end_index, content) =
          find_comment_bounds format file_contents cr_start
        in
        let start_line, start_col = Lazy.force pos_2d start_index in
        { content_start = cr_start
        ; start_index
        ; end_index
        ; cr            = { path
                          ; content
                          ; start_line
                          ; start_col
                          }
        })
    ;;
  end
end

module Due = Stable.Due.V1

module Assignee = struct
  include Stable.Assignee.V1

  let user_name t ~feature_owner ~alternate_names =
    match t with
    | This unresolved_name ->
      User_name_by_alternate_name.to_user_name alternate_names unresolved_name
    | Feature_owner -> feature_owner
    | Missing_file_owner -> User_name.missing_file_owner
  ;;
end

module Processed = struct

  include Stable.Processed.V1

  let invariant (_ : t) = ()

  let compute_assignee ~file_owner ~reported_by ~for_ ~due ~is_xcr =
    if is_xcr
    then (
      match reported_by with
      | None -> Assignee.Feature_owner
      | Some user -> This user)
    else (
      match for_ with
      | Some user -> This user
      | None ->
        match (due : Due.t) with
        | Now -> Feature_owner
        | Soon | Someday ->
          match file_owner with
          | None -> Assignee.Missing_file_owner
          | Some user -> This (User_name.to_unresolved_name user))
  ;;

  let recompute_assignee t ~file_owner =
    let { raw; reported_by; for_; due; is_xcr; assignee = _ } = t in
    let assignee = compute_assignee ~file_owner ~reported_by ~for_ ~due ~is_xcr in
    { raw; reported_by; for_; due; is_xcr; assignee }
  ;;
end

module Cr_comment = Stable.V1

include Cr_comment

let invariant (_ : t) = ()

let raw t =
  match unshared_t t with
  | Raw r -> r
  | Processed p -> p.raw
;;

let path       t = Raw.path       (raw t)
let content    t = Raw.content    (raw t)
let start_line t = Raw.start_line (raw t)
let start_col  t = Raw.start_col  (raw t)

let recompute_assignee t ~file_owner =
  match unshared_t t with
  | Raw _ -> t
  | Processed processed ->
    Cr_comment.shared_t
      (Processed (Processed.recompute_assignee processed ~file_owner))
;;

let reindented_content t =
  let indent = String.make ((raw t).start_col + 2) ' ' in
  let str = content t in
  let lines = String.split str ~on:'\n' in
  let lines =
    lines
    |> List.rev
    |> List.drop_while ~f:(String.for_all ~f:Char.is_whitespace)
    |> List.rev
  in
  match Result.try_with (fun () ->
    List.mapi lines ~f:(fun i s ->
      match String.chop_prefix s ~prefix:indent with
      | None ->
        if String.is_prefix indent ~prefix:s then ""
        else if i = 0 then "  " ^ s
        else raise Exit
      | Some s -> "  " ^ s))
  with
  | Error _ -> str
  | Ok deindented_lines -> String.concat deindented_lines ~sep:"\n"
;;

module Structurally_compared = struct
  type nonrec t = t [@@deriving compare, sexp_of]
end

module For_sorted_output = struct
  type nonrec t = t
  let compare t1 t2 = Raw.For_sorted_output.compare (raw t1) (raw t2)
end

let sort ts = List.sort ts ~cmp:For_sorted_output.compare

let assignee t =
  match unshared_t t with
  | Raw _ -> Assignee.Feature_owner
  | Processed p -> p.assignee
;;

let due t =
  match unshared_t t with
  | Raw _ -> Due.Now
  | Processed p -> p.due
;;

let is_xcr t =
  match unshared_t t with
  | Raw _ -> false
  | Processed p -> p.is_xcr
;;

let work_on t : Due.t =
  match unshared_t t with
  | Raw _ -> Now
  | Processed p -> if p.is_xcr then Now else p.due
;;

let to_string ?(attributes = []) t ~include_content =
  let file_str =
    sprintf "%s:%d:%d:"
      (Relpath.to_string (path t))
      (start_line t)
      (start_col t)
  in
  let attributes =
    match attributes with
    | [] -> []
    | _ :: _ ->
      let max_width =
        List.fold_left attributes ~init:0
          ~f:(fun acc (k, _) -> max acc (String.length k))
      in
      List.map attributes ~f:(fun (k, v) ->
        sprintf "%-*s : %s" max_width k v)
  in
  let contents = if include_content then [reindented_content t] else [] in
  String.concat ~sep:"\n"
    (file_str :: attributes @ contents @ [""])
;;

let print ~attributes ~include_delim cr ~include_content =
  let str = to_string cr ~attributes ~include_content in
  let nl =
    if include_delim && include_content
    then "\n"
    else ""
  in
  printf "%s%s" nl str
;;

let print_list ~crs_and_attributes ~include_content =
  let crs_and_attributes =
    List.sort crs_and_attributes
      ~cmp:(fun (cr, _) (cr2, _) -> For_sorted_output.compare cr cr2)
  in
  let include_delim = ref false in
  List.iter crs_and_attributes ~f:(fun (cr, attributes) ->
    print ~attributes ~include_delim:!include_delim cr ~include_content;
    include_delim := true
  )
;;

module Cr_soon = struct

  module Stable = Stable.Cr_soon
  module T = Stable.V1
  include T

  let invariant (t : t) =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field (t :> Unshared.t) f in
      Unshared.Fields.iter
        ~cr_comment:(check (fun processed ->
          Processed.invariant processed;
          assert (not processed.is_xcr);
          assert (match processed.due with Soon -> true | Now | Someday -> false)))
        ~digest_of_condensed_content:(check Digest.invariant)
        ~hash_of_path_and_condensed_content:ignore)
  ;;

  let condense_whitespace =
    let regex = Regex.create_exn "\\s+" in
    (fun s -> Regex.rewrite_exn regex ~template:" " s)
  ;;

  let create ~(cr_comment : Cr_comment.t) =
    try
      match Cr_comment.unshared_t cr_comment with
      | Raw _ -> failwith "a raw CR comment cannot be a CR-soon"
      | Processed cr_comment ->
        let raw = cr_comment.raw in
        let digest_of_condensed_content =
          Digest.create (condense_whitespace raw.content)
        in
        let t =
          shared_t
            { cr_comment
            ; digest_of_condensed_content
            ; hash_of_path_and_condensed_content =
                Relpath.hash raw.path lxor Digest.hash digest_of_condensed_content
            }
        in
        invariant t;
        Ok t
    with exn ->
      error "Cr_soon.create failed" (exn, cr_comment) [%sexp_of: exn * Cr_comment.t]
  ;;

  let cr_comment t = Cr_comment.shared_t (Processed (unshared_t t).cr_comment)

  let assignee t =
    match (unshared_t t).cr_comment.assignee with
    | Feature_owner -> assert false
    | This user_name -> user_name
    | Missing_file_owner -> User_name.to_unresolved_name User_name.missing_file_owner
  ;;

  let recompute_assignee t ~file_owner =
    let { Unshared.
          cr_comment
        ; digest_of_condensed_content
        ; hash_of_path_and_condensed_content
        } = unshared_t t
    in
    shared_t
      { cr_comment = Processed.recompute_assignee cr_comment ~file_owner
      ; digest_of_condensed_content
      ; hash_of_path_and_condensed_content
      }
  ;;

  let raw        t = (unshared_t t).cr_comment.raw
  let start_line t = (raw t).start_line
  let path       t = (raw t).path
  let content    t = (raw t).content

  module Structurally_compared = struct
    type nonrec t = t [@@deriving compare, sexp_of]
  end

  module Compare_ignoring_minor_text_changes = struct

    module T = struct

      include T

      let hash t = (unshared_t t).hash_of_path_and_condensed_content

      let compare t1 t2 =
        let c = Int.compare (hash t1) (hash t2) in
        if c <> 0
        then c
        else (
          let c =
            Digest.compare
              (unshared_t t1).digest_of_condensed_content
              (unshared_t t2).digest_of_condensed_content
          in
          if c <> 0
          then c
          else (
            let c = Relpath.compare (path t1) (path t2) in
            if c <> 0
            then c
            else Unresolved_name.compare (assignee t1) (assignee t2)))
      ;;
    end

    include T
    include Comparable.Make (T)
    include Hashable.  Make (T)
  end

  module For_sorted_output = struct
    type nonrec t = t
    let compare t1 t2 = Raw.For_sorted_output.compare (raw t1) (raw t2)
  end
end

module Summary = struct

  module T = Stable.Summary.V1

  include (T : module type of T with module Row := T.Row)

  module Row = struct

    include T.Row

    let total t = t.crs + t.xcrs

    let zero (assignee : User_name.t) = { assignee; crs = 0; xcrs = 0 }

    let incr_crs  t = { t with crs  = t.crs  + 1 }
    let incr_xcrs t = { t with xcrs = t.xcrs + 1 }

    let compare_for_summary t1 t2 = - (Int.compare (total t1) (total t2))
  end

  let create crs ~feature_owner ~alternate_names =
    let counts_tbl = User_name.Table.create () in
    List.iter crs ~f:(fun cr ->
      let assignee = Assignee.user_name (assignee cr) ~feature_owner ~alternate_names in
      Hashtbl.update counts_tbl assignee ~f:(fun maybe_count ->
        let count = match maybe_count with Some x -> x | None -> Row.zero assignee in
        if is_xcr cr then Row.incr_xcrs count else Row.incr_crs count));
    Hashtbl.data counts_tbl
  ;;

  let empty = []

  let rows t = t ;;

  let to_ascii_table =
    let columns =
      Ascii_table.Column.(
        [ user                 (cell Row.assignee)
        ; crs                  (cell Row.crs)
        ; xcrs                 (cell Row.xcrs)
        ; int  ~header:"total" (cell Row.total)
        ])
    in
    let total rows =
      let crs, xcrs =
        List.fold ~init:(0, 0) rows ~f:(fun (crs, xcrs) row ->
          (crs + row.Row.crs, xcrs + row.Row.xcrs))
      in
      (* this row is used internally to trick Ascii_table into showing a total *)
      { Row. assignee = User_name.of_string "total"; crs; xcrs }
    in
    function
    | [] -> None
    | rows ->
      let rows = List.sort rows ~cmp:Row.compare_for_summary in
      Some (Ascii_table.create ~columns ~rows:(rows @ [ total rows ]))
  ;;
end

(* -------------------------------------------------------------------------- *)
(*  Matching                                                                  *)
(* -------------------------------------------------------------------------- *)

(* supported comment syntaxes:

   ML     (* X?CR ... *)   may nest recursively

   C      /* X?CR ... */   may not nest recursively

   // X?CR ... EOL   may match multiple lines
   //      ... EOL
   //      ... EOL

   Shell  # X?CR  ... EOL
   #       ... EOL
   #       ... EOL

   Lisp   ; X?CR  ... EOL
   ;       ... EOL
   ;       ... EOL

   SQL    -- X?CR ... EOL
   --      ... EOL
   --      ... EOL

   XML    <!-- X?CR ... --> may not nest recursively
*)

module Process : sig
  val process
    :  Raw.t
    -> file_owner : User_name.t option
    -> [ `Processed of Processed.t
       (* [`Property] is for in-file attributes, like "".
          cr supports them, and Iron does not.  But while a repo is handled by both,
          Iron must ignore them, to avoid reporting spurious CRs. *)
       | `Property of string * string
       ] Or_error.t
end = struct

  (* various utilities -- mostly attempting to make the code more readable *)
  let named_group name patt = String.concat [ "(?P<"; name; ">"; patt; ")" ]

  (* (?:re)? makes an optional group that can't be matched via the ~sub arguments to
     various re2 functions.  I don't know if this is any better than just (re)?, but it's
     certainly not worse. *)
  let with_flags flags patt = String.concat ["(?"; flags; ":"; patt; ")"]
  let protect patt = with_flags "" patt (* to avoid the usual stringy macro problems *)

  let optional patt = protect patt ^ "?"
  let any      patt = protect patt ^ "*"
  let some     patt = protect patt ^ "+"

  let seq patts = String.concat patts
  let alt patts = protect (String.concat ~sep:"|" patts)

  (* : and @ have other meanings in CR comments *)
  let word = "[^ \\t\\n:@]+"
  let whitespace = "\\s"

  let exactly patt = Regex.create_exn ("^" ^ patt ^ "$")

  let comment_regex =
    exactly
      (seq [
         any whitespace;
         optional (named_group "is_xcr" "X");
         "CR";
         optional
           (seq [
              "[-v]";
              named_group "due" (alt ["\\d{6}"; "soon"; "someday"]);
            ]);
         some whitespace;
         named_group "from_user" word;
         optional
           (seq [
              some whitespace;
              "for";
              some whitespace;
              named_group "for" word;
            ]);
         any whitespace;
         ":";
         with_flags "s" ".*"; (* the "s" flag makes "." match newlines *)
       ])
  ;;

  let property_regex =
    exactly
      (seq [
         any whitespace;
         "CR";
         some whitespace;
         "@";
         named_group "key" word;
         any whitespace;
         ":";
         named_group "value" (any ".");
       ])
  ;;

  let process raw ~file_owner =
    try
      let content = raw.Raw.content in
      match Regex.get_matches_exn ~max:1 comment_regex content with
      | [] ->
        (match Regex.get_matches_exn ~max:1 property_regex content with
         | [] -> error "Invalid CR comment" content String.sexp_of_t
         | m :: _ ->
           let get field_name = Regex.Match.get ~sub:(`Name field_name) m in
           match get "key", get "value" with
           | Some key, Some value -> Ok (`Property (String.strip key, String.strip value))
           | None, _ | _, None ->
             error "Invalid property specification" content String.sexp_of_t)
      | m :: _ ->
        let get field_name = Regex.Match.get ~sub:(`Name field_name) m in
        match get "from_user" with
        | None -> error "Couldn't parse username" content String.sexp_of_t
        | Some reported_by ->
          let unresolved_name string =
            Option.try_with (fun () -> Unresolved_name.of_string string)
          in
          let reported_by = unresolved_name reported_by in
          let is_xcr = is_some (get "is_xcr") in
          let for_ = Option.bind (get "for") ~f:unresolved_name in
          let due =
            match get "due" with
            | None           -> Ok Due.Now
            | Some "soon"    -> Ok Due.Soon
            | Some "someday" -> Ok Due.Someday
            | Some _s -> (* dated CR -> CR-someday *) Ok Due.Someday
          in
          match due with
          | Error _ as err -> err
          | Ok due ->
            let assignee =
              Processed.compute_assignee ~file_owner ~reported_by ~for_ ~due ~is_xcr
            in
            Ok (`Processed { Processed. raw; reported_by; for_; due; is_xcr; assignee })
    with exn ->
      error "could not process CR" (raw, exn) [%sexp_of: Raw.t * exn]
  ;;
end

module Crs = struct
  type nonrec t =
    { due_now     : t list
    ; due_soon    : Cr_soon.t list
    ; due_someday : t list
    }
end

module Crs_due_now_and_soon = struct
  type nonrec t =
    { due_now  : t list
    ; due_soon : Cr_soon.t list
    }

  let of_crs { Crs. due_now; due_soon; due_someday = _ } = { due_now; due_soon }
end

let extract format ~path ~file_contents ~file_owner =
  List.filter_map
    (Raw.With_file_positions.extract format ~path ~file_contents)
    ~f:(fun { cr = raw; _ } ->
      match Process.process raw ~file_owner with
      | Ok (`Property _) -> None
      | Ok (`Processed p) -> Some (Cr_comment.shared_t (Processed p))
      | Error _ -> Some (Cr_comment.shared_t (Raw raw)))
;;

module Files_to_grep = struct
  type t =
    | All_files_below of Path_in_repo.t
    | Only_those_files of Path_in_repo.t list
end

let grep_files repo_root format ~files_to_grep ~file_owner =
  let absolute_path path_in_repo =
    Abspath.to_string
      (Abspath.append (Repo_root.to_abspath repo_root)
         (Path_in_repo.to_relpath path_in_repo))
  in
  let below, files_to_grep =
    match (files_to_grep : Files_to_grep.t) with
    | All_files_below below             -> below, Hg.files ~include_:below repo_root
    | Only_those_files only_those_files -> Path_in_repo.root, return only_those_files
  in
  let%bind files_to_grep =
    let module Process = Async.Process in
    let%bind files_to_grep = files_to_grep in
    let%bind grep_process =
      Process.create
        ~working_dir:(Repo_root.to_string repo_root) ~prog:"xargs" ~args:
        [ "-r"; "-d"; "\n"; "grep"; "--no-messages"; "-E"; "-l"
        ; "--binary-files=without-match"; cr_pattern_egrep ]
        ()
    in
    let grep_process = ok_exn grep_process in
    let grep_in = Process.stdin grep_process in
    Writer.set_buffer_age_limit grep_in `Unlimited;
    List.iter files_to_grep ~f:(fun file ->
      Writer.write_line grep_in (Path_in_repo.to_string file));
    let%map result =
      Process.collect_stdout_and_wait grep_process ~accept_nonzero_exit:[ 123 ]
    in
    result
    |> ok_exn
    |> String.split_lines
    |> List.map ~f:Path_in_repo.of_string
  in
  let throttle = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:20 in
  let all_due_now     = ref [] in
  let all_due_soon    = ref [] in
  let all_due_someday = ref [] in
  let%map () =
    Deferred.List.iter files_to_grep ~how:`Parallel ~f:(fun path_in_repo ->
      let%map file_contents =
        Throttle.enqueue throttle (fun () ->
          Reader.file_contents (absolute_path path_in_repo))
      in
      let file_owner =
        match file_owner path_in_repo with
        | Error _  -> None
        | Ok user -> Some user
      in
      List.iter
        (extract format ~path:(ok_exn (Relpath.chop_prefix
                                         ~prefix:(Path_in_repo.to_relpath below)
                                         (Path_in_repo.to_relpath path_in_repo)))
           ~file_contents ~file_owner)
        ~f:(fun t ->
          match work_on t with
          | Someday -> all_due_someday := t :: !all_due_someday
          | Now     -> all_due_now := t :: !all_due_now
          | Soon    ->
            match Cr_soon.create ~cr_comment:t with
            | Error _ -> all_due_now := t :: !all_due_now
            | Ok cr_soon -> all_due_soon := cr_soon :: !all_due_soon))
  in
  { Crs.
    due_now     = !all_due_now
  ; due_soon    = !all_due_soon
  ; due_someday = !all_due_someday
  }
;;

let grep repo_root format ~below ~file_owner =
  grep_files repo_root format ~files_to_grep:(All_files_below below) ~file_owner
;;

module Cached_facts_for_incremental_computation = struct
  type t =
    { base_rev       : Rev.t
    ; base_crs       : Crs_due_now_and_soon.t
    ; base_cr_format : Cr_comment_format.t
    }
end

module Due_by_path_in_repo = struct
  type nonrec t =
    { due_now  : t list         Path_in_repo.Table.t
    ; due_soon : Cr_soon.t list Path_in_repo.Table.t
    }

  let hashtbl_multi_of_list crs get_path =
    let tbl = Path_in_repo.Table.create () in
    List.iter crs ~f:(fun cr ->
      Hashtbl.add_multi tbl
        ~key:(Path_in_repo.of_relpath (get_path cr))
        ~data:cr);
    tbl
  ;;

  let of_crs { Crs_due_now_and_soon. due_now; due_soon } =
    { due_now  = hashtbl_multi_of_list due_now  path
    ; due_soon = hashtbl_multi_of_list due_soon Cr_soon.path
    }
  ;;
end

let maybe_incremental_grep repo_root format ~incremental_based_on ~file_owner =
  let recompute_from_scratch () =
    let%map crs =
      grep_files repo_root format
        ~files_to_grep:(All_files_below Path_in_repo.root) ~file_owner
    in
    Crs_due_now_and_soon.of_crs crs
  in
  match incremental_based_on with
  | None -> recompute_from_scratch ()
  | Some { Cached_facts_for_incremental_computation.
           base_rev
         ; base_crs
         ; base_cr_format
         } ->
    if not (Cr_comment_format.equal base_cr_format format)
    then recompute_from_scratch ()
    else (
      let%bind status =
        Hg.status repo_root (Between { src = base_rev; dst = `Working_copy })
      in
      if List.is_empty status then return base_crs
      else (
        let%bind files_at_tip = Hg.files repo_root in
        let active_files = Hg.Status.dst_path_in_repo status in
        let%map crs_in_active_files =
          let%map crs =
            grep_files repo_root format
              ~files_to_grep:(Only_those_files active_files) ~file_owner
          in
          Crs_due_now_and_soon.of_crs crs
        in
        let due_in_active_files = Due_by_path_in_repo.of_crs crs_in_active_files in
        let due_in_base = Due_by_path_in_repo.of_crs base_crs in
        let add_crs list table path_in_repo recompute_assignee =
          match Hashtbl.find table path_in_repo with
          | None -> ()
          | Some crs ->
            list := List.rev_append (List.rev_map crs ~f:recompute_assignee) !list
        in
        let due_now  = ref [] in
        let due_soon = ref [] in
        let add_crs path_in_repo ~should_recompute_assignee
              (due_by_path_in_repo : Due_by_path_in_repo.t) =
          let maybe_recompute_assignee f =
            if should_recompute_assignee
            then (
              let file_owner =
                match file_owner path_in_repo with
                | Error _  -> None
                | Ok owner -> Some owner
              in
              (fun cr -> f cr ~file_owner))
            else Fn.id
          in
          add_crs due_now  due_by_path_in_repo.due_now  path_in_repo
            (maybe_recompute_assignee recompute_assignee);
          add_crs due_soon due_by_path_in_repo.due_soon path_in_repo
            (maybe_recompute_assignee Cr_soon.recompute_assignee);
        in
        let active_files = Path_in_repo.Set.of_list active_files in
        List.iter files_at_tip ~f:(fun path_in_repo ->
          let in_active_files = Set.mem active_files path_in_repo in
          add_crs path_in_repo ~should_recompute_assignee:(not in_active_files)
            (if in_active_files
             then due_in_active_files
             else due_in_base)
        );
        { Crs_due_now_and_soon.
          due_now  = !due_now
        ; due_soon = !due_soon
        }))
;;
