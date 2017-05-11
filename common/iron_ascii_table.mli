open! Core
open! Import

module Attr : sig
  type color =
    [ `Black
    | `Red
    | `Green
    | `Yellow
    | `Blue
    | `Magenta
    | `Cyan
    | `White
    ]

  type t =
    [ `Bright
    | `Dim
    | `Underscore
    | `Reverse
    | color
    | `Bg of color
    ]
end

module Align : sig
  type t = Left | Right | Center
end

(** A ['row Column.t] describes how to render a column in a table of ['row] rows. *)
module Column : sig

  (** A [('row, 'field) cell] is a function that takes a ['row] and returns the contents
      of the cell with additional rendering attributes. *)
  type ('row, 'field) cell

  val cell      : ('row ->               'field) -> ('row, 'field) cell
  val attr_cell : ('row -> Attr.t list * 'field) -> ('row, 'field) cell

  val lift : ('row2, 'field) cell -> f:('row1 -> 'row2) -> ('row1, 'field) cell

  (** include module type of Textutils.Ascii_table.Column *)
  type 'row t

  val string
    :  ?show_zero:bool (** = true *)
    -> ?show  : [ `Yes | `No | `If_not_empty ]  (** default is [`If_not_empty] *)
    -> ?min_width : int
    -> ?truncate_after_n_char: int
    -> ?align : Align.t  (** default is [Left] *)
    -> header : string
    -> ('row, string) cell
    -> 'row t

  val of_to_string
    :  header : string
    -> ('a -> string)
    -> ('row, 'a) cell
    -> 'row t

  val int
    :  ?show_zero : bool  (** default is [false] *)
    -> ?show      : [ `If_not_empty | `No | `Yes ]
    -> header     : string
    -> ('row, int) cell
    -> 'row t

  val int_or_error
    : header : string
    -> ('row, int Or_error.t) cell
    -> 'row t

  val feature            : ('row, Feature_path.t) cell -> 'row t
  val crs                : ('row, int           ) cell -> 'row t
  val num_lines          : ('row, int           ) cell -> 'row t
  val num_lines_or_error : ('row, int Or_error.t) cell -> 'row t
  val user               : ('row, User_name.t   ) cell -> 'row t
  val xcrs               : ('row, int           ) cell -> 'row t
end

type t

val create : columns:'row Column.t list -> rows:'row list -> t

val to_string
  : ?force_unicode_bars:unit
  -> t
  -> display_ascii:bool
  -> max_output_columns:int
  -> string

val is_empty : t -> bool
