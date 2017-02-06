module Unstable = struct
  open! Import
  module Hash_consing       = Hash_consing
end

module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module Build_projection            = Build_projection.            Stable
  module Groups                      = Groups.                      Stable
  module Scrutiny                    = Scrutiny.                    Stable

  module V5 = struct
    module Unshared = struct
      type t =
        { users                   : Unresolved_name.V1.Set.t
        ; groups                  : Groups.V1.t
        ; tags                    : Tag.V1.Set.t
        ; scrutinies              : Scrutiny.V2.t Scrutiny_name.V1.Map.t
        ; build_projections       : Build_projection.V2.t Build_projection_name.V1.Map.t
        ; disallow_useless_dot_fe : bool
        ; allow_review_for        : Allow_review_for.V1.t
        ; obligations_version     : Obligations_version.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let module_name = "Iron_obligations.Obligations_repo"

      let hash t =
        let module Hash_consing = Unstable.Hash_consing in
        let hash field = Hash_consing.field t field in
        Fields.fold
          ~init:Hash_consing.init
          ~users:(hash Unresolved_name.V1.Set.hash)
          ~groups:(hash Groups.V1.hash)
          ~tags:(hash Tag.V1.Set.hash)
          ~scrutinies:(hash (Scrutiny_name.V1.Map.hash Scrutiny.V2.hash))
          ~build_projections:(hash (Build_projection_name.V1.Map.hash
                                      Build_projection.V2.hash))
          ~disallow_useless_dot_fe:(hash (Hashtbl.hash : bool -> int))
          ~allow_review_for:(hash Allow_review_for.V1.hash)
          ~obligations_version:(hash Obligations_version.V1.hash)
    end
    include Unshared
    include Hash_consing.Make_stable_public (Unshared) ()

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| a211d5c033385ad120133a69fa1e1948 |}]
    ;;
  end
  module Model = V5
end

open! Core
open! Import

module Declaration = struct
  module Build_projection_syntax = struct
    type t =
      { default_scrutiny        : Scrutiny_name.t
      ; require_low_review_file : bool [@default false ];
      }
    [@@deriving sexp]
  end

  type u = (syntax, Sexp.t) And_sexp.t
  and syntax =
    | Allow_review_for of Users.Allow_review_for.t * [ `by ] * Users.Allow_review_for.t
    | Define_build_projection of Build_projection_name.t * Build_projection_syntax.t
    | Define_group of Group_name.t * Unresolved_name.Set.t
    | Define_scrutiny of Scrutiny_name.t * Scrutiny.Syntax.t
    | Define_tags of Tag.t sexp_list
    | Disallow_useless_dot_fe
    | Scaffold_requires_global_tag_or_rev_hash
    | Use_cr_spec
    | Users of Unresolved_name.t sexp_list
  [@@deriving sexp]

  type t = (u, Sexp.Annotated.t) And_sexp.t

  let of_annotated_sexp annotated_sexp : t =
    { syntax = annotated_sexp |> Sexp.Annotated.get_sexp |> [%of_sexp: u]
    ; sexp   = Some annotated_sexp
    }
  ;;

  let scaffold_requires_global_tag_or_rev_hash (t : t) =
    match t.syntax.syntax with
    | Scaffold_requires_global_tag_or_rev_hash -> true
    | _ -> false
  ;;
end

type t = Stable.Model.t =
  { users                   : Unresolved_name.Set.t
  ; groups                  : Groups.t
  ; tags                    : Tag.Set.t
  ; scrutinies              : Scrutiny.t Scrutiny_name.Map.t
  ; build_projections       : Build_projection.t Build_projection_name.Map.t
  ; disallow_useless_dot_fe : bool
  ; allow_review_for        : Allow_review_for.t
  ; obligations_version     : Obligations_version.t
  }
[@@deriving sexp_of]

let shared_t = Stable.Model.shared_t

module Defined_in_local_repo = struct
  type t =
    { tags : Tag.Set.t
    }
  [@@deriving sexp_of]
end

let check_duplicate_global e label sexp_of_a duplicate_set =
  if not (Set.is_empty duplicate_set)
  then
    Error_context.raise_s e
      [%sexp (sprintf "\
remove %s from .fe/obligations-repo.sexp that are also in .fe/obligations-global.sexp \
(which comes from the scaffolded repo)"
                label : string),
             (duplicate_set : a)
      ];
;;

let eval
      (obligations_global : Obligations_global.t)
      (declarations : Declaration.t list)
      ~obligations_repo ~aliases =
  let { Obligations_global.
        disallow_useless_dot_fe = global_disallow_useless_dot_fe
      ; scrutinies              = global_scrutinies
      ; tags                    = global_tags
      ; users                   = global_users
      ; groups                  = global_groups
      ; obligations_version
      }
    = obligations_global
  in
  Error_context.within ~file:obligations_repo (fun e ->
    let allow_review_for         = Queue.create () in
    let define_build_projections = Queue.create () in
    let define_group             = Queue.create () in
    let define_scrutiny          = Queue.create () in
    let define_tags              = Queue.create () in
    let disallow_useless_dot_fe  = ref false in
    let users                    = Queue.create () in
    List.iter declarations ~f:(fun declaration ->
      let e =
        Error_context.augment e
          ?annotated_sexp:declaration.sexp
          ?sexp:declaration.syntax.sexp
      in
      let add = Queue.enqueue in
      match declaration.syntax.syntax with
      | Allow_review_for        (x, `by, y) -> add allow_review_for (e, x, y)
      | Define_build_projection (x, y)      -> add define_build_projections (e, x, y)
      | Define_group            (x, y)      -> add define_group (e, x, y)
      | Define_scrutiny         (x, y)      -> add define_scrutiny (e, x, y)
      | Define_tags              x          -> add define_tags (e, x)
      | Disallow_useless_dot_fe -> disallow_useless_dot_fe := true
      | Scaffold_requires_global_tag_or_rev_hash -> ()
      | Use_cr_spec ->
        Error_context.raise_s e
          [%sexp "Use_cr_spec is deprecated.  Old projections \
                  format is no longer supported"
          ]
      | Users x -> add users (e, x));
    let users =
      let duplicated_users = Unresolved_name.Hash_set.create () in
      let local_users = Unresolved_name.Hash_set.create () in
      Queue.iter users ~f:(fun (_e, users) ->
        List.iter users ~f:(fun user ->
          match Hash_set.strict_add local_users user with
          | Ok ()   -> ()
          | Error _ -> Hash_set.add duplicated_users user));
      let duplicated_users = Unresolved_name.Set.of_hash_set duplicated_users in
      let local_users = Unresolved_name.Set.of_hash_set local_users in
      (if not (Set.is_empty duplicated_users)
       && Obligations_version.is_at_least_version obligations_version ~version:V4
       then
         Error_context.raise_s e
           [%sexp "multiply defined users", (duplicated_users : Unresolved_name.Set.t)]);
      check_duplicate_global e "users" [%sexp_of: Unresolved_name.Set.t]
        (Set.inter local_users global_users);
      Set.union local_users global_users
    in
    let groups =
      let local_groups = Group_name.Table.create () in
      Queue.iter define_group ~f:(fun (e, group_name, users_in_group) ->
        (match Hashtbl.add local_groups ~key:group_name ~data:users_in_group with
         | `Ok -> ()
         | `Duplicate ->
           Error_context.raise_s e
             [%sexp "multiply defined group", (group_name : Group_name.t)]));
      let local_groups = Group_name.Map.of_hashtbl_exn local_groups in
      check_duplicate_global e "groups" [%sexp_of: Group_name.Set.t]
        (Set.inter
           (Set.of_map_keys local_groups)
           (Set.of_map_keys global_groups));
      let groups = Groups.extend global_groups ~add_or_override_with:local_groups in
      (match Groups.check_users groups ~known_users:users with
       | Ok ()     -> ()
       | Error err -> Error_context.raise e err);
      groups
    in
    let tags, local_tags =
      let local_tags = Tag.Hash_set.create () in
      Queue.iter define_tags ~f:(fun (e, define_tags) ->
        List.iter define_tags ~f:(fun tag ->
          if Hash_set.mem local_tags tag
          then
            Error_context.raise_s e
              [%sexp "multiply defined tag", (tag : Tag.t)];
          Hash_set.add local_tags tag));
      let local_tags = Tag.Set.of_hash_set local_tags in
      check_duplicate_global e "tags" [%sexp_of: Tag.Set.t]
        (Set.inter local_tags global_tags);
      Set.union local_tags global_tags, local_tags
    in
    let scrutinies =
      let local_scrutinies = Scrutiny_name.Table.create () in
      Queue.iter define_scrutiny ~f:(fun (e, name, syntax) ->
        let scrutiny =
          Scrutiny.eval name syntax e ~aliases ~allowed_users:users ~known_groups:groups
        in
        (match
           Hashtbl.add local_scrutinies ~key:name ~data:scrutiny
         with
         | `Ok -> ()
         | `Duplicate ->
           Error_context.raise_s e
             [%sexp "multiply defined scrutiny", (name : Scrutiny_name.t)]));
      let local_scrutinies = Scrutiny_name.Map.of_hashtbl_exn local_scrutinies in
      check_duplicate_global e "scrutinies" [%sexp_of: Scrutiny_name.Set.t]
        (Set.inter
           (Set.of_map_keys local_scrutinies)
           (Set.of_map_keys global_scrutinies));
      Map.merge local_scrutinies global_scrutinies ~f:(fun ~key:_ -> function
        | `Left a | `Right a -> Some a
        | `Both _ ->
          (* This is prevented by the check above. *)
          assert false)
    in
    let build_projections =
      let build_projections = Build_projection_name.Table.create () in
      Queue.iter define_build_projections
        ~f:(fun (e, name, { default_scrutiny; require_low_review_file }) ->
          (if Hashtbl.mem build_projections name
           then
             Error_context.raise_s e
               [%sexp "multiply defined build projection"
                    , (name : Build_projection_name.t)]);
          (match Map.find scrutinies default_scrutiny with
           | None ->
             Error_context.raise_s e
               [%sexp "undefined scrutiny", (default_scrutiny : Scrutiny_name.t)]
           | Some default_scrutiny ->
             Hashtbl.add_exn build_projections ~key:name
               ~data:(Build_projection.create ~name ~default_scrutiny
                        ~require_low_review_file)));
      Build_projection_name.Map.of_hashtbl_exn build_projections
    in
    let disallow_useless_dot_fe =
      global_disallow_useless_dot_fe || !disallow_useless_dot_fe
    in
    let allow_review_for =
      let allow_review_for_ref = ref None in
      Queue.iter allow_review_for ~f:(fun (e, reviewed_for, reviewed_by) ->
        let eval_allow_review_for_users expr =
          Users.Allow_review_for.eval expr e ~aliases
            ~allowed_users:users
            ~known_groups:groups
        in
        allow_review_for_ref :=
          Some
            (Allow_review_for.also_allow
               (Option.value !allow_review_for_ref ~default:Allow_review_for.none)
               ~reviewed_by: (eval_allow_review_for_users reviewed_by)
               ~reviewed_for:(eval_allow_review_for_users reviewed_for)));
      Option.value !allow_review_for_ref ~default:Allow_review_for.all
    in
    shared_t
      { users
      ; groups
      ; tags
      ; scrutinies
      ; build_projections
      ; disallow_useless_dot_fe
      ; allow_review_for
      ; obligations_version
      }
  , { Defined_in_local_repo.
      tags = local_tags
    })
;;

let obligations_global_path = Relpath.of_string ".fe/obligations-global.sexp"
let obligations_repo_path   = Relpath.of_string ".fe/obligations-repo.sexp"

let load (module Hg : Hg_dependency.S) repo_root ~aliases =
  let open Async in
  let repo_root_as_abspath = Repo_root.to_abspath repo_root in
  let lose e file msg =
    Error (Error.tag_arg e msg file [%sexp_of: Abspath.t])
  in
  (* 1: Try to read in the repo's .fe/obligations-repo.sexp spec file. *)
  let obligations_repo = Abspath.append repo_root_as_abspath obligations_repo_path in
  match%bind
    Reader.load_annotated_sexps (Abspath.to_string obligations_repo)
      Declaration.of_annotated_sexp
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
            ~f:Declaration.scaffold_requires_global_tag_or_rev_hash
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
    eval gspec repo_declarations
      ~obligations_repo:(Abspath obligations_repo)
      ~aliases
;;
