module Unstable = struct
  open! Import
  module Hash_consing       = Hash_consing
end

module Stable = struct
  open! Core.Stable
  open! Import_stable

  module Build_projection            = Build_projection.            Stable
  module Groups                      = Groups.                      Stable
  module Obligations_global          = Obligations_global.          Stable

  module V3 = struct
    module Unshared = struct
      type t =
        { build_projections    : Build_projection.V1.t Build_projection_name.V1.Map.t
        ; tags                 : Tag.V1.Set.t
        ; use_cr_spec          : bool
        ; users                : Unresolved_name.V1.Set.t
        ; groups               : Groups.V1.t
        ; obligations_global   : Obligations_global.V3.t
        ; allow_review_for     : Allow_review_for.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let module_name = "Iron_obligations.Obligations_repo"

      let hash t =
        let module Hash_consing = Unstable.Hash_consing in
        let hash field = Hash_consing.field t field in
        Fields.fold
          ~init:Hash_consing.init
          ~build_projections:(hash (Build_projection_name.V1.Map.hash
                                       Build_projection.V1.hash))
          ~tags:(hash Tag.V1.Set.hash)
          ~use_cr_spec:(hash (Hashtbl.hash : bool -> int))
          ~users:(hash Unresolved_name.V1.Set.hash)
          ~groups:(hash Groups.V1.hash)
          ~obligations_global:(hash Obligations_global.V3.hash)
          ~allow_review_for:(hash Allow_review_for.V1.hash)
    end
    include Unshared
    include Hash_consing.Make_stable_public (Unshared) ()
  end
  module Model = V3
end

open! Core.Std
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
    | Define_tags of Tag.t sexp_list
    | Define_group of Group_name.t * Unresolved_name.Set.t
    | Scaffold_requires_global_tag_or_rev_hash
    | Use_cr_spec
    | Users of Unresolved_name.t sexp_list
  [@@deriving sexp]

  type t = (u, Sexp.Annotated.t) And_sexp.t

  let sexp_of_t (t : t) = t.syntax.syntax |> [%sexp_of: syntax]

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

type deprecated_use_cr_spec = bool [@@deriving sexp_of]

type t = Stable.Model.t =
   { build_projections    : Build_projection.t Build_projection_name.Map.t
   ; tags                 : Tag.Set.t
   ; use_cr_spec          : deprecated_use_cr_spec
   ; users                : Unresolved_name.Set.t
   ; groups               : Groups.t
   ; obligations_global   : Obligations_global.t
   ; allow_review_for     : Allow_review_for.t
   }
[@@deriving sexp_of]

let shared_t = Stable.Model.shared_t

let eval
      (obligations_global : Obligations_global.t)
      (declarations : Declaration.t list)
      ~obligations_repo ~aliases =
  Error_context.within ~file:obligations_repo (fun e ->
    let build_projections = Build_projection_name.Table.create () in
    let tags = Tag.Hash_set.create () in
    let known_users = ref Unresolved_name.Set.empty in
    let groups = Group_name.Table.create () in
    let allow_review_for = ref None in
    let known_local_groups () =
      groups
      |> Hashtbl.to_alist
      |> Group_name.Map.of_alist_exn
    in
    let known_groups () =
      Groups.extend obligations_global.groups
        ~add_or_override_with:(known_local_groups ())
    in
    List.iter declarations ~f:(fun declaration ->
      let e =
        Error_context.augment e
          ?annotated_sexp:declaration.sexp
          ?sexp:declaration.syntax.sexp
      in
      let eval_allow_review_for_users ~known_groups users =
        Users.Allow_review_for.eval users e ~aliases
          ~allowed_users:(Set.union obligations_global.users !known_users)
          ~known_groups
      in
      match declaration.syntax.syntax with
      | Allow_review_for (reviewed_for, `by, reviewed_by) ->
        let known_groups = known_groups () in
        allow_review_for :=
          Some
            (Allow_review_for.also_allow
               (Option.value !allow_review_for ~default:Allow_review_for.none)
               ~reviewed_by: (eval_allow_review_for_users ~known_groups reviewed_by)
               ~reviewed_for:(eval_allow_review_for_users ~known_groups reviewed_for))
      | Define_build_projection (name, { default_scrutiny; require_low_review_file }) ->
        if Hashtbl.mem build_projections name
        then
          Error_context.error_s e
            [%sexp "multiply defined build projection", (name : Build_projection_name.t)];
        begin match Map.find obligations_global.scrutinies default_scrutiny with
        | None ->
          Error_context.error_s e
            [%sexp "undefined scrutiny", (default_scrutiny : Scrutiny_name.t)]
        | Some default_scrutiny ->
          Hashtbl.add_exn build_projections ~key:name
            ~data:(Build_projection.create ~name ~default_scrutiny
                     ~require_low_review_file)
        end;
      | Define_tags define_tags ->
        List.iter define_tags ~f:(fun tag ->
          if Hash_set.mem tags tag
          then
            Error_context.error_s e
              [%sexp "multiply defined tag", (tag : Tag.t)];
          Hash_set.add tags tag;
        )
      | Define_group (group_name, users_in_group) ->
        if Hashtbl.mem groups group_name
        then Error_context.error_s e
               [%sexp "multiply defined group", (group_name : Group_name.t)]
        else Hashtbl.set groups ~key:group_name ~data:users_in_group
      | Scaffold_requires_global_tag_or_rev_hash -> ()
      | Use_cr_spec ->
        Error_context.error_s e
          [%sexp "Use_cr_spec is deprecated.  Old projections \
                  format is no longer supported"
          ]
      | Users more_users ->
        known_users := Set.union !known_users (Unresolved_name.Set.of_list more_users));
    let known_users = !known_users in
    let allow_review_for =
      Option.value !allow_review_for ~default:Allow_review_for.all
    in
    let check_duplicate_global label sexp_of_a duplicate_set =
      if not (Set.is_empty duplicate_set)
      then
        Error_context.error_s e
          [%sexp (sprintf "\
remove %s from .fe/obligations-repo.sexp that are also in .fe/obligations-global.sexp \
(which comes from the scaffolded repo)"
                    label : string),
                 (duplicate_set : a)
          ];
    in
    let known_tags =
      tags
      |> Hash_set.to_list
      |> Tag.Set.of_list
    in
    let groups = known_local_groups () in
    check_duplicate_global "users" [%sexp_of: Unresolved_name.Set.t]
      (Set.inter known_users obligations_global.users);
    check_duplicate_global "tags" [%sexp_of: Tag.Set.t]
      (Set.inter known_tags obligations_global.tags);
    check_duplicate_global "groups" [%sexp_of: Group_name.Set.t]
      (let keys map = map |> Map.keys |> Group_name.Set.of_list in
       Set.inter (keys groups) (keys obligations_global.groups));
    begin
      let known_users = Set.union obligations_global.users known_users in
      match Groups.check_users groups ~known_users with
      | Ok ()     -> ()
      | Error err -> Error_context.raise e err
    end;
    shared_t
      { build_projections            = build_projections
                                       |> Hashtbl.to_alist
                                       |> Build_projection_name.Map.of_alist_exn
      ; tags                         = known_tags
      ; use_cr_spec                  = false
      ; users                        = known_users
      ; groups
      ; allow_review_for
      ; obligations_global
      })
;;

let union_of_groups_defined t =
  Groups.extend t.obligations_global.groups ~add_or_override_with:t.groups
;;

let union_of_tags_defined  t = Set.union t.tags  t.obligations_global.tags
let union_of_users_defined t = Set.union t.users t.obligations_global.users
