open! Core.Std
open! Async.Std
open! Import

let obligations_global_path = Relpath.of_string ".fe/obligations-global.sexp"
let obligations_repo_path   = Relpath.of_string ".fe/obligations-repo.sexp"

let load (module Hg : Hg_dependency.S) repo_root ~aliases =
  let repo_root_as_abspath = Repo_root.to_abspath repo_root in
  let lose e file msg =
    Error (Error.tag_arg e msg file [%sexp_of: Abspath.t])
  in
  (* 1: Try to read in the repo's .fe/obligations-repo.sexp spec file. *)
  let obligations_repo = Abspath.append repo_root_as_abspath obligations_repo_path in
  match%bind
    Reader.load_annotated_sexps (Abspath.to_string obligations_repo)
      Obligations_repo.Declaration.of_annotated_sexp
  with
  | Error err ->
    return (lose err repo_root_as_abspath
              (sprintf "Error loading %s file at repo root"
                 (Relpath.to_string obligations_repo_path)))
  | Ok repo_declarations ->
    (* 2: Try to read in the global .fe/obligations-global.sexp file. *)
    let try_globalspec specfile =
      match%map
        Reader.load_annotated_sexps (Abspath.to_string specfile)
          Obligations_global.Declaration.of_annotated_sexp
      with
      | Ok _ as ok -> ok
      | Error err  -> lose err specfile "Unable to load global spec file."
    in
    (* Try searching upward for a .fe/obligations-global.sexp file *)
    let relpaths_to_look_for =
      [ obligations_global_path;
        Relpath.append (Relpath.of_string "..") obligations_global_path;
      ]
    in
    let abspaths_to_look_for =
      List.map relpaths_to_look_for
        ~f:(fun relpath -> Abspath.append (Repo_root.to_abspath repo_root) relpath)
    in
    let%map maybe_obligations_global_declarations =
      match%bind
        Deferred.List.find abspaths_to_look_for ~f:(fun specfile ->
          let%map access = Unix.access (Abspath.to_string specfile) [`Exists] in
          is_ok access)
      with
      | Some specfile -> try_globalspec specfile
      | None ->
        let scaffold_requires_global_tag_or_rev_hash =
          List.exists repo_declarations
            ~f:Obligations_repo.Declaration.scaffold_requires_global_tag_or_rev_hash
        in
        match%map
          Hg.cat_from_scaffold repo_root
            (Path_in_repo.of_relpath obligations_global_path)
            ~scaffold_requires_global_tag_or_rev_hash
        with
        | `No_scaffold scaffold_sexp ->
          Or_error.error
            (sprintf !"\
Error finding %{Relpath}: looked for these files in turn, but none exist"
               obligations_global_path)
            (relpaths_to_look_for @ [ scaffold_sexp ])
            [%sexp_of: Relpath.t list]

        | `Scaffold_exists (Error (_ as error)) ->
          Or_error.error
            (sprintf !"Error loading %{Relpath} via %{File_name}"
               obligations_global_path File_name.scaffold_sexp)
            error
            [%sexp_of: Error.t]

        | `Scaffold_exists (Ok contents) ->
          Or_error.try_with (fun () ->
            match Sexp.Annotated.of_string ("(" ^ contents ^ ")") with
            | Atom _ -> assert false
            | List (_, declarations, _) ->
              List.map declarations ~f:Obligations_global.Declaration.of_annotated_sexp)
    in
    (* 3: We've got both global & repo spec declarations. Parse & resolve them. *)
    let open Or_error.Let_syntax in
    let%bind gspec =
      let%bind decls = maybe_obligations_global_declarations in
      Obligations_global.eval decls
        ~obligations_global:(Relpath obligations_global_path)
        ~aliases
    in
    Obligations_repo.eval gspec repo_declarations
      ~obligations_repo:(Abspath obligations_repo)
      ~aliases
;;
