module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module V1 = struct
    type t = Unresolved_name.V1.Set.t Group_name.V1.Map.t
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 784896b81c01cfe4fee3ea9dfd807fdf |}]
    ;;

    let hash = Group_name.V1.Map.hash Unresolved_name.V1.Set.hash
  end
end

open! Core
open! Import

include Stable.V1

let check_users t ~known_users =
  let unknown_users = Group_name.Table.create () in
  Map.iteri t ~f:(fun ~key:group ~data:users_in_group ->
    Set.iter users_in_group ~f:(fun user ->
      if not (Set.mem known_users user)
      then Hashtbl.add_multi unknown_users ~key:group ~data:user));
  unknown_users
  |> Hashtbl.to_alist
  |> List.filter_map ~f:(fun (group, unknown_users) ->
    let unknown_users = Unresolved_name.Set.of_list unknown_users in
    if Set.is_empty unknown_users
    then None
    else
      Some
        [%sexp
          { group         : Group_name.t
          ; unknown_users : Unresolved_name.Set.t
          }
        ])
  |> function
  | [] -> Ok ()
  | (_::_) as errors ->
    Or_error.error "groups with unknown users"
      errors [%sexp_of: Sexp.t list]
;;

let eval_exn e t group =
  match Map.find t group with
  | Some users -> users
  | None ->
    Error_context.raise_s e [%sexp "no such group", (group : Group_name.t)]
;;

let extend t1 ~add_or_override_with:t2 =
  Map.merge t1 t2 ~f:(fun ~key:_ value ->
    Some (match value with
      | `Left x      -> x
      | `Right x     -> x
      | `Both (_, x) -> x))
;;

let get_users_exn t blang =
  let undefined_groups =
    List.filter (Blang.to_list blang) ~f:(fun group -> not (Map.mem t group))
    |> Group_name.Set.of_list
  in
  if not (Set.is_empty undefined_groups)
  then
    raise_s
      [%sexp
        (sprintf "undefined group%s"
           (if Set.length undefined_groups > 1 then "s" else "") : string)
      , (undefined_groups : Group_name.Set.t)];
  let universe =
    lazy
      (Map.fold t ~init:Unresolved_name.Set.empty ~f:(fun ~key:_ ~data acc ->
         Unresolved_name.Set.union acc data))
  in
  let to_set group =
    Option.value (Map.find t group) ~default:Unresolved_name.Set.empty
  in
  Blang.eval_set ~universe to_set blang
;;
