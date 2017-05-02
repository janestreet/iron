open! Core
open Import

module Ascii_table = Textutils.Ascii_table
module Console     = Textutils.Console

module Align = Ascii_table.Align

module Attr = struct
  type color = Console.Ansi.color
  type t = Console.Ansi.attr
end

module Column = struct

  include Ascii_table.Column

  type ('a, 'b) cell = 'a -> Attr.t list * 'b

  let cell f a = ([], f a)
  let attr_cell f = f

  let lift cell ~f a1 = cell (f a1)

  let string ?(show_zero = true) ?(show = `If_not_empty) ?min_width
        ?truncate_after_n_char ?align ~header f =
    let f =
      match truncate_after_n_char with
      | None -> f
      | Some n ->
        (fun res ->
           let (attrs, str) = f res in
           let str =
             if String.length str <= n
             then str
             else sprintf "%s ..." (String.sub str ~pos:0 ~len:n)
           in
           attrs, str)
    in
    let f_no_zero res =
      let attrs, str = f res in
      let str = if String.(=) str "0" then "" else str in
      attrs, str
    in
    create_attr header (if show_zero then f else f_no_zero) ~show
      ?align
      ?min_width
      ~max_width:2000
  ;;

  let int ?(show_zero = false) ?show ~header f =
    string ~header ~align:Right ?show (fun x ->
      let (attrs, i) = f x in
      (attrs, if i = 0 && not show_zero then "" else Int.to_string_hum i))
  ;;

  let int_or_error ~header cell =
    string ~header ~align:Right (fun a ->
      let attrs, int_or_error = cell a in
      match int_or_error with
      | Error _ -> (`Red :: attrs, "error")
      | Ok i -> (attrs, if i = 0 then "" else Int.to_string_hum i))
  ;;

  let num_lines f = int ~header:"lines" f
  let crs       f = int ~header:"CRs"   f
  let xcrs      f = int ~header:"XCRs"  f

  let num_lines_or_error f =
    string ~header:"lines" ~align:Right ~show:`If_not_empty (fun x ->
      let attrs, int_or_error = f x in
      match int_or_error with
      | Error _ -> (`Red :: attrs, "unknown")
      | Ok int -> (attrs, if int = 0 then "" else Int.to_string_hum int))
  ;;

  let of_to_string ~header to_string cell =
    string ~header (fun a ->
      let attrs, x = cell a in
      attrs, to_string x)
  ;;

  let feature cell = of_to_string ~header:"feature" Feature_path.to_string cell
  let user    cell = of_to_string ~header:"user"    User_name.to_string    cell

end

type 'row t_ =
  { columns : 'row Column.t list
  ; rows    : 'row list
  }

type t = T : _ t_ -> t

let create ~columns ~rows = T { columns; rows }

let to_string ?force_unicode_bars (T t) ~display_ascii ~max_output_columns =
  if List.is_empty t.rows
  then ""
  else (
    let bars =
      if display_ascii && Option.is_none force_unicode_bars
      then `Ascii
      else `Unicode
    in
    (if display_ascii
     then Ascii_table.to_string_noattr
     else Ascii_table.to_string)
      t.columns t.rows ~bars ~limit_width_to:max_output_columns)
;;

let is_empty (T t) = List.is_empty t.rows
