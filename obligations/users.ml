open Core
open Import

type ('syntax, 'semantics) eval_user
  =  'syntax
  -> Error_context.t
  -> aliases       : User_name_by_alternate_name.t
  -> allowed_users : Unresolved_name.Set.t
  -> 'semantics

type ('syntax, 'semantics) eval
  = ('syntax,  known_groups:Groups.t -> 'semantics) eval_user

let eval_users users e ~aliases ~allowed_users =
  let missing = List.filter users ~f:(fun user -> not (Set.mem allowed_users user)) in
  if List.is_empty missing
  then
    List.map users ~f:(fun user ->
      User_name_by_alternate_name.to_user_name aliases user)
  else
    Error_context.raise_s e
      [%sexp "users not in obligations-global.sexp or obligations-repo.sexp",
             (missing : Unresolved_name.t list)
      ]
;;

let eval_user user e ~aliases ~allowed_users =
  match eval_users [ user ] e ~aliases ~allowed_users with
  | [ user ] -> user
  | _ -> assert false
;;

type t =
  | Users of Unresolved_name.t sexp_list
  | Group of Group_name.t
  | Union of t sexp_list
[@@deriving sexp]

let eval_group group e ~aliases ~allowed_users:_ ~known_groups =
  Groups.eval_exn e known_groups group
  |> Set.to_list
  |> List.map ~f:(fun user -> User_name_by_alternate_name.to_user_name aliases user)
  |> User_name.Set.of_list
;;

let rec eval t e ~aliases ~allowed_users ~known_groups =
  match t with
  | Users users -> User_name.Set.of_list (eval_users users e ~aliases ~allowed_users)
  | Group group -> eval_group group e ~aliases ~allowed_users ~known_groups
  | Union ts ->
    List.map ts ~f:(fun t -> eval t e ~aliases ~allowed_users ~known_groups)
    |> User_name.Set.union_list
;;

let synthesize users =
  Users (List.map (Set.to_list users) ~f:User_name.to_unresolved_name)
;;

module Allow_review_for = struct
  type t =
    | All_users
    | Users of Unresolved_name.t sexp_list
    | Group of Group_name.t
    | Union of t sexp_list
  [@@deriving sexp]

  module Users = Allow_review_for.Users

  let rec eval t e ~aliases ~allowed_users ~known_groups =
    match t with
    | All_users   -> Users.all_users
    | Group group -> Users.users (eval_group group e ~aliases ~allowed_users ~known_groups)
    | Users users ->
      Users.users (User_name.Set.of_list (eval_users users e ~aliases ~allowed_users))
    | Union ts ->
      List.map ts ~f:(fun t -> eval t e ~aliases ~allowed_users ~known_groups)
      |> Users.union_list
  ;;
end
