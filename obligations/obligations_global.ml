module Unstable = struct
  open! Import
  module Hash_consing       = Hash_consing
end

module Stable = struct
  open! Core.Stable
  open! Import_stable

  module Groups = Groups.Stable
  module Scrutiny = Scrutiny.Stable

  module V3 = struct
    module Unshared = struct
      type t =
        { disallow_useless_dot_fe : bool
        ; scrutinies              : Scrutiny.V1.t Scrutiny_name.V1.Map.t
        ; tags                    : Tag.V1.Set.t
        ; users                   : Unresolved_name.V1.Set.t
        ; groups                  : Groups.V1.t
        ; obligations_version     : Obligations_version.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let module_name = "Iron_obligations.Obligation_global"

      let hash t =
        let module Hash_consing = Unstable.Hash_consing in
        let hash field = Hash_consing.field t field in
        Fields.fold
          ~init:Hash_consing.init
          ~disallow_useless_dot_fe:(hash (Hashtbl.hash : bool -> int))
          ~scrutinies:(hash (Scrutiny_name.V1.Map.hash Scrutiny.V1.hash))
          ~tags:(hash Tag.V1.Set.hash)
          ~users:(hash Unresolved_name.V1.Set.hash)
          ~groups:(hash Groups.V1.hash)
          ~obligations_version:(hash Obligations_version.V1.hash)
      ;;
    end
    include Unshared
    include Hash_consing.Make_stable_public (Unshared) ()
  end

  module Model = V3
end

open! Core.Std
open! Import

module Declaration = struct
  type t =
    | Define_scrutiny of Scrutiny_name.t * Scrutiny.Syntax.t
    | Define_tags of Tag.t sexp_list
    | Define_group of Group_name.t * Unresolved_name.Set.t
    | Disallow_useless_dot_fe
    | Users of Unresolved_name.t sexp_list
    | Cr_comment_format of Cr_comment_format.t
    | Obligations_version of Obligations_version.t
  [@@deriving sexp]
end

type t = Stable.Model.t =
  { disallow_useless_dot_fe : bool
  ; scrutinies              : Scrutiny.t Scrutiny_name.Map.t
  ; tags                    : Tag.Set.t
  ; users                   : Unresolved_name.Set.t
  ; groups                  : Groups.t
  ; obligations_version     : Obligations_version.t
  }
[@@deriving sexp_of]

let shared_t = Stable.Model.shared_t

let eval declarations =
  let disallow_useless_dot_fe = ref false in
  let scrutinies = Scrutiny_name.Table.create () in
  let tags = Tag.Hash_set.create () in
  let users = Unresolved_name.Hash_set.create () in
  let groups = Group_name.Table.create () in
  let obligations_version = ref None in
  let process_declaration (declaration : Declaration.t) =
    match declaration with
    | Define_scrutiny (name, scrutiny) ->
      begin
        match
          Hashtbl.add scrutinies ~key:name ~data:(Scrutiny.create name scrutiny)
        with
        | `Ok -> ()
        | `Duplicate ->
          raise_s [%sexp "multiply defined scrutiny", (name : Scrutiny_name.t)]
      end;
    | Define_tags define_tags ->
      List.iter define_tags ~f:(fun tag ->
        if Hash_set.mem tags tag
        then raise_s [%sexp "multiply defined tag", (tag : Tag.t)]
        else Hash_set.add tags tag)
    | Define_group (group_name, users_in_group) ->
      if Hashtbl.mem groups group_name
      then raise_s [%sexp "multiply defined group", (group_name : Group_name.t)]
      else Hashtbl.set groups ~key:group_name ~data:users_in_group
    | Disallow_useless_dot_fe -> disallow_useless_dot_fe := true
    | Users people -> List.iter people ~f:(fun user -> Hash_set.add users user)
    | Obligations_version format ->
      begin match !obligations_version with
      | Some _ -> failwith "multiple Obligations_version specifications"
      | None -> obligations_version := Some format
      end
    | Cr_comment_format format ->
      begin match !obligations_version with
      | Some _ -> failwith "multiple Cr_comment_format specifications"
      | None ->
        let (version : Obligations_version.t) =
          match format with
          | V1         -> V1
          | V2_sql_xml -> V2
        in
        obligations_version := Some version
      end
  in
  Or_error.try_with (fun () ->
    List.iter declarations ~f:process_declaration;
    let scrutinies =
      scrutinies
      |> Hashtbl.to_alist
      |> Scrutiny_name.Map.of_alist_exn
    in
    let tags =
      tags
      |> Hash_set.to_list
      |> Tag.Set.of_list
    in
    let users =
      users
      |> Hash_set.to_list
      |> Unresolved_name.Set.of_list
    in
    let groups =
      groups
      |> Hashtbl.to_alist
      |> Group_name.Map.of_alist_exn
    in
    let obligations_version =
      Option.value !obligations_version ~default:Obligations_version.default
    in
    ok_exn (Groups.check_users groups ~known_users:users);
    shared_t
      { disallow_useless_dot_fe = !disallow_useless_dot_fe
      ; tags
      ; scrutinies
      ; users
      ; groups
      ; obligations_version
      })
;;
