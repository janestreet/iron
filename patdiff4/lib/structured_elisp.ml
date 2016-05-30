open Core.Std

type t = Hunk.Lines.t list list

let create = Fn.id

let cons = List.cons

let quote_regex     = Re2.Regex.create_exn "\""
let backslash_regex = Re2.Regex.create_exn "\\\\"

let elisp_quote s =
  begin
    let open Or_error.Monad_infix in
    Re2.Regex.replace backslash_regex ~f:(const "\\\\") s
    >>= Re2.Regex.replace quote_regex ~f:(const "\\\"")
  end |> Or_error.ok_exn
;;

let%test_unit _ =
  [%test_result: string]
    (elisp_quote "")
    ~expect:""
;;

let%test_unit _ =
  [%test_result: string]
    (elisp_quote "\"the black\\cat\\\"")
    ~expect:"\\\"the black\\\\cat\\\\\\\""
;;

module Sexp = struct
  include Sexp
  let rec map ~f =
    function
    | Atom s  -> Atom (f s)
    | List xs -> List (List.map ~f:(map ~f) xs)

  let rec dumb_print =
    function
    | Atom s  -> sprintf "\"%s\"" s
    | List xs -> sprintf "(%s)" (String.concat ~sep:" " (List.map ~f:dumb_print xs))
end

(* Not exported! This is just for conversion to elisp. *)
let sexp_of_t t =
  let sexp_map ~f xs = Sexp.List (List.map ~f xs) in
  let sexp_of_algo_jump_to_lines (t : Hunk.Lines.t) =
    Sexp.List
      [ Sexp.Atom (Diff_algo.Id.to_string t.id)
      ; Sexp.Atom (Int.to_string t.jump_to_line)
      ; Sexp.Atom (String.concat ~sep:"\n" t.lines)
      ]
  in
  let sexp_of_algos algos =
    sexp_map algos ~f:sexp_of_algo_jump_to_lines
  in
  sexp_map t ~f:sexp_of_algos
;;

let to_elisp t =
  t
  |> sexp_of_t
  |> Sexp.map ~f:elisp_quote
  |> Sexp.dumb_print
;;
