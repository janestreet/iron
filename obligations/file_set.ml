open Core.Std
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

let complement t = And_sexp.create (Complement t)

let files files = And_sexp.create (Files files)

let union (ts : t list) =
  let ts =
    List.concat_map ts ~f:(fun t ->
      match t.syntax with
      | Union ts -> ts
      | _ -> [ t ])
  in
  And_sexp.create (Union ts)
;;

let eval t ~universe e =
  let empty = File_name.Set.empty in
  let eval_glob glob e =
    let matching =
      Set.filter universe ~f:(fun file_name -> Simple_glob.matches glob file_name)
    in
    if Set.is_empty matching
    then
      Error_context.error_s e
        [%sexp "glob did not match any files", (glob : Simple_glob.t)];
    matching
  in
  let eval_re2 re2 e =
    let matching =
      Set.filter universe ~f:(fun file_name ->
        Whole_regexp.matches re2 (File_name.to_string file_name))
    in
    if Set.is_empty matching
    then Error_context.error_s e
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
        Error_context.error_s e
          [%sexp "reference to non-existent files", (not_in_universe : File_name.t list)];
      File_name.Set.of_list files
  in
  loop t e
;;

let synthesize ~desired ~universe =
  let universe_length = Set.length universe in
  let unexpected_desired = Set.diff desired universe in
  if not (Set.is_empty unexpected_desired)
  then
    raise_s
      [%sexp
        "File_set.synthesize got desired files not in the universe",
        { universe           : File_name.Set.t
        ; desired            : File_name.Set.t
        ; unexpected_desired : File_name.Set.t
        }
      ];
  let add_remaining sexp_string =
    lazy begin
      let t = sexp_string |> Sexp.of_string |> [%of_sexp: t] in
      match Error_context.within ~file:Path.root (fun e -> eval t ~universe e) with
      | Error _ -> None
      | Ok set ->
        if Set.length set <= 2
        then None
        else
          let excess = Set.diff set desired in
          if not (Set.is_empty excess)
          then None
          else
            let remaining_desired = Set.diff desired set in
            Some (if Set.is_empty remaining_desired
                  then t
                  else union [ t
                             ; files (Set.to_list remaining_desired)
                             ])
    end
  in
  let heuristics         =
    [ lazy (if Set.length desired = universe_length
            then Some all_files
            else None)
    ; add_remaining "(Globs *.ml *.mli)"
    ; add_remaining "(Globs *.ml)"
    ; add_remaining "(Globs *.mli)"
    ; if true
      then lazy None
      else
        lazy (let complement_desired = Set.diff universe desired in
              if Set.length desired > 2 * Set.length complement_desired
              then Some (complement (files (Set.to_list complement_desired)))
              else None)
    ]
  in
  match List.find_map heuristics~f:force with
  | Some value -> value
  | None -> files (Set.to_list desired)
;;
