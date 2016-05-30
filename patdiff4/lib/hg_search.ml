open! Core.Std
open! Async.Std
open! Import

module Regexes : sig
  type t
  val old_base    : t
  val new_base    : t
  val old_tip : t

  val find : repo_root:Repo_root.t -> t -> string -> Rev.t Or_error.t Deferred.t
end = struct
  module Regex = Re2.Regex
  type t = Regex.t

  let create marker =
    Regex.create_exn ~options:Re2.Options.noisy
      (sprintf "%s[^[]*%s(\\w+)%s" marker (Regex.escape "[") (Regex.escape "]"))
  ;;

  let old_base    = create (Regex.escape "|||||||")
  let new_base    = create ">>>>>>>"
  let old_tip     = create "<<<<<<<"

  let find_aux = Re2.Regex.find_first ~sub:(`Index 1)

  let%test_unit _ =
    [%test_result: string]
      (Or_error.ok_exn (find_aux old_base "||||||| old base: jane-11.06+59 [abcdef]"))
      ~expect:"abcdef"
  ;;

  let%test_unit _ =
    [%test_result: string]
      (Or_error.ok_exn (find_aux new_base ">>>>>>> hi [abcdef]"))
      ~expect:"abcdef"
  ;;

  let%test_unit _ =
    [%test_result: string]
      (Or_error.ok_exn (find_aux old_tip "<<<<<<< hi [abcdef]"))
      ~expect:"abcdef"
  ;;

  let find ~repo_root t contents =
    match find_aux t contents with
    | Error _ as error -> return error
    | Ok str ->
      Hg.create_rev repo_root (Hg.Revset.of_string str)
  ;;
end

let find_diamond ?(verbose=false) ~repo_root ~path_in_repo ~new_tip_rev () =
  match%map
    Hg.cat_one repo_root new_tip_rev path_in_repo
    >>=? fun file_contents ->
    Regexes.(find ~repo_root old_base file_contents)
    >>=? fun old_base_rev ->
    Regexes.(find ~repo_root new_base file_contents)
    >>=? fun new_base_rev ->
    Regexes.(find ~repo_root old_tip file_contents)
    >>|? fun old_tip_rev ->
    if verbose
    then Core.Std.eprintf "hey we found something! (%s %s %s)\n%!"
           (Rev.to_string_12 old_base_rev)
           (Rev.to_string_12 new_base_rev)
           (Rev.to_string_12 old_tip_rev);
    { Diamond.
      b1 = old_base_rev
    ; b2 = new_base_rev
    ; f1 = old_tip_rev
    ; f2 = new_tip_rev
    }
  with
  | Ok x -> Some x
  | Error err ->
    if verbose
    then Core.Std.eprintf
           !"error while grepping for revs in\n\
             %{Path_in_repo}\n\
             at %s:\n\
             %{sexp:Error.t}\n"
           path_in_repo (Rev.to_string_12 new_tip_rev) err;
    None
;;

let list_find list ~f =
  let rec aux = function
    | [] -> return None
    | hd::tl ->
      match%bind f hd with
      | (Some _) as res -> return res
      | None -> aux tl
  in
  aux list
;;

(* If you know the revision of the "merged" result, you can use this function to walk
   backwards to find the revision with conflict markers, and use it to solve for the
   other three. *)
let walk_to_find_diamond ?(verbose=false) ~repo_root ~path_in_repo ~new_tip_rev () =
  match%bind
    Hg.create_revs ~restrict_to:path_in_repo repo_root
      (Revset.limit ~limit:3 (
         Revset.reverse (
           Revset.and_ [ Revset.ancestors (Revset.of_rev new_tip_rev)
                       ; Revset.merge
                       ])))
  with
  | Error _ -> return None
  | Ok candidates ->
    list_find candidates ~f:(fun new_tip_rev ->
      find_diamond ~verbose ~repo_root ~path_in_repo ~new_tip_rev ()
    )
;;

let find_merged_rev ?(verbose=false) ~repo_root ~path_in_repo ~rev () =
  match%bind
    Hg.create_revs repo_root
      (Revset.difference
         (Revset.descendants (Revset.of_rev rev))
         (Revset.ancestors   (Revset.of_rev rev)))
  with
  | Error _ -> return None
  | Ok descendants ->
    Deferred.List.fold
      descendants
      ~init:None
      ~f:(fun init new_tip_rev ->
        match init with
        | Some _ -> return init
        | None ->
          match%map
            find_diamond
              ~verbose:false ~repo_root ~path_in_repo ~new_tip_rev ()
          with
          | None   ->
            if verbose
            then Core.Std.eprintf "%s:%s    => merged:%s\n"
                   (Path_in_repo.to_string path_in_repo)
                   (Rev.to_string_12 rev)
                   (Rev.to_string_12 new_tip_rev);
            Some new_tip_rev
          | Some _ -> None)
;;
