open! Core.Std
open! Import

module View_ids_shown : sig
  type t =
    | All
    | Only of Diff_algo.Id.t list
  [@@deriving sexp]
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

val align_alist : (string * string) list -> string list

module Lines : sig
  type t =
    { id           : Diff_algo.Id.t
    ; jump_to_line : Jump_to_line.t
    ; lines        : string list
    }
end

val to_lines
  : t
  -> Lines.t list

val num_lines_to_review : t -> int

val list_to_lines : t list -> string list
