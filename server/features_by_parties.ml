open Core
open! Import

module Parties = struct
  module T = struct
    type t =
      | Owners
      | Whole_feature_followers
    [@@deriving compare, enumerate, sexp]
    let hash (t:t) = Hashtbl.hash t
  end
  include T
  include Hashable.Make (T)
end

module Users_by_feature_id = struct
  type t = User_name.Set.t Feature_id.Table.t
  [@@deriving sexp_of]

  let equal (t1 : t) t2 = Hashtbl.equal t1 t2 User_name.Set.equal

  let ensure_equal ~actual ~expected =
    if not (equal actual expected)
    then raise_s [%sexp "mismatch", { actual : t; expected : t }]
  ;;
end

module By_parties = struct
  type t =
    { by_user             : Feature_id.Hash_set.t User_name.Table.t
    ; users_by_feature_id : Users_by_feature_id.t
    }
  [@@deriving sexp_of]

  let invariant t =
    let expected =
      List.concat_map (Hashtbl.to_alist t.by_user) ~f:(fun (user, feature_ids) ->
        List.map (Hash_set.to_list feature_ids) ~f:(fun feature_id ->
          (feature_id, user)))
      |> Feature_id.Table.of_alist_multi
      |> Hashtbl.map ~f:User_name.Set.of_list
    in
    Users_by_feature_id.ensure_equal ~actual:t.users_by_feature_id ~expected;
  ;;

  let create () =
    { by_user             = User_name.Table.create ()
    ; users_by_feature_id = Feature_id.Table.create ()
    }
  ;;

  let find t user =
    match Hashtbl.find t.by_user user with
    | None -> []
    | Some feature_ids -> Hash_set.to_list feature_ids
  ;;

  let set_users t feature_id new_users =
    let previous_users =
      Option.value (Hashtbl.find t.users_by_feature_id feature_id)
        ~default:User_name.Set.empty
    in
    if Set.is_empty new_users
    then Hashtbl.remove t.users_by_feature_id feature_id
    else Hashtbl.set t.users_by_feature_id ~key:feature_id ~data:new_users;
    Set.iter (Set.diff previous_users new_users) ~f:(fun previous_user ->
      Hash_set.remove (Hashtbl.find_exn t.by_user previous_user) feature_id);
    Set.iter (Set.diff new_users previous_users) ~f:(fun new_user ->
      Hash_set.add (Hashtbl.find_or_add t.by_user new_user
                      ~default:Feature_id.Hash_set.create) feature_id);
  ;;

  let remove_feature t feature_id =
    match Hashtbl.find t.users_by_feature_id feature_id with
    | None -> ()
    | Some users ->
      Hashtbl.remove t.users_by_feature_id feature_id;
      Set.iter users ~f:(fun user ->
        Hash_set.remove (Hashtbl.find_exn t.by_user user) feature_id);
  ;;
end

type t =
  { by_parties : By_parties.t Parties.Table.t
  }
[@@deriving sexp_of]

let create () =
  { by_parties = Parties.Table.create ()
  }
;;

let find_parties t parties =
  Hashtbl.find_or_add t.by_parties parties ~default:(fun () -> By_parties.create ())
;;

let invariant t =
  Hashtbl.iter t.by_parties ~f:By_parties.invariant
;;

let set_users t feature_id parties users =
  By_parties.set_users (find_parties t parties) feature_id users
;;

let find t user parties =
  By_parties.find (find_parties t parties) user
;;

let remove_feature t feature_id =
  Hashtbl.iter t.by_parties
    ~f:(fun by_parties -> By_parties.remove_feature by_parties feature_id)
;;
