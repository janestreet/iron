open Core
open Import

type t = (syntax, Sexp.t) And_sexp.t
and syntax =
  | All_files
  | Complement of t
  | Diff       of t * t
  | Files      of File_name.t sexp_list
  | Globs      of Simple_glob.t sexp_list
  | Inter      of t sexp_list
  | Re2s       of Whole_regexp.t sexp_list
  | Union      of t sexp_list
[@@deriving sexp]

let all_files = And_sexp.create All_files

let files files = And_sexp.create (Files files)

let eval t ~universe e =
  let empty = File_name.Set.empty in
  let eval_glob glob e =
    let matching =
      Set.filter universe ~f:(fun file_name -> Simple_glob.matches glob file_name)
    in
    if Set.is_empty matching
    then
      Error_context.raise_s e
        [%sexp "glob did not match any files", (glob : Simple_glob.t)];
    matching
  in
  let eval_re2 re2 e =
    let matching =
      Set.filter universe ~f:(fun file_name ->
        Whole_regexp.matches re2 (File_name.to_string file_name))
    in
    if Set.is_empty matching
    then Error_context.raise_s e
           [%sexp "regexp didn't match any files", (re2 : Whole_regexp.t)];
    matching
  in
  let union xs e eval_x =
    List.fold xs ~init:empty ~f:(fun ac x -> Set.union ac (eval_x x e))
  in
  let rec loop (t : t) e =
    let e = Error_context.augment e ?sexp:t.sexp in
    match t.syntax with
    | All_files -> universe
    | Inter ts -> List.fold ts ~init:universe ~f:(fun ac t -> Set.inter ac (loop t e))
    | Union ts    -> union ts    e loop
    | Globs globs -> union globs e eval_glob
    | Re2s re2s   -> union re2s  e eval_re2
    | Diff (t1, t2) -> Set.diff (loop t1 e) (loop t2 e)
    | Complement t  -> Set.diff universe    (loop t  e)
    | Files files ->
      let not_in_universe =
        List.filter files ~f:(fun file -> not (Set.mem universe file))
      in
      if not (List.is_empty not_in_universe)
      then
        Error_context.raise_s e
          [%sexp "reference to non-existent files", (not_in_universe : File_name.t list)];
      File_name.Set.of_list files
  in
  loop t e
;;
