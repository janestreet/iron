open Core
open Import

type t =
  { return         : Error.t return
  ; file           : Path.t
  ; info           : Info.t option
  ; sexp           : Sexp.t option
  ; annotated_sexp : Sexp.Annotated.t option
  }

let within (type ok) ~file (f : t -> ok) : ok Or_error.t =
  match
    Or_error.try_with (fun () ->
      with_return (fun return ->
        let return = With_return.prepend return ~f:(fun err -> Error err) in
        Ok (f { return; file; info = None; sexp = None; annotated_sexp = None })))
  with
  | Ok (_ as x) -> x
  | Error e -> raise_s [%sexp "Iron bug", [%here], (e : Error.t), (file : Path.t)]
;;

let raise_x t x sexp_of_x =
  let line, column =
    match t.sexp, t.annotated_sexp with
    | None, _ | _, None -> 0, 0
    | Some sexp, Some annotated_sexp ->
      match Sexp.Annotated.find_sexp annotated_sexp sexp with
      | None -> 0, 0
      | Some annotated_sexp ->
        let start_pos = (Sexp.Annotated.get_range annotated_sexp).start_pos in
        start_pos.line, start_pos.col
  in
  let error_pos =
    { Source_code_position.
      pos_fname = Path.to_string t.file
    ; pos_lnum  = line
    ; pos_bol   = 0
    ; pos_cnum  = column
    }
  in
  t.return.return
    (match t.info with
     | None ->
       Error.create (Source_code_position.to_string error_pos) x [%sexp_of: x]
     | Some info ->
       Error.create (Source_code_position.to_string error_pos) (info, x)
         [%sexp_of: Info.t * x])
;;

let raise_f t fmt  = ksprintf (fun s () -> raise_x t s [%sexp_of: string]) fmt
let raise_s t sexp = raise_x t sexp Fn.id
let raise   t err  = raise_x t err  [%sexp_of: Error.t]

let augment ?annotated_sexp ?info ?sexp t =
  let maybe old new_ =
    match new_ with
    | None -> old
    | Some _ -> new_
  in
  { t with
    info           = maybe t.info           info
  ; sexp           = maybe t.sexp           sexp
  ; annotated_sexp = maybe t.annotated_sexp annotated_sexp
  }
;;
