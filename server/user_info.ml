open! Core.Std
open! Import

module Persist = struct
  module User_names_set = struct
    include Persistent.Make
        (struct let version = 1 end)
        (User_name.Stable.V1.Set)
  end
  module Alternate_names = struct
    include Persistent.Make
        (struct let version = 1 end)
        (User_name_by_alternate_name.Stable.V1)
  end
end

type t =
  { mutable existing_users : User_name.Hash_set.t
  ; mutable valid_users    : User_name.Hash_set.t
  ; mutable aliases_seen   : User_name_by_alternate_name.t
  ; mutable typos          : User_name_by_alternate_name.t
  ; mutable invalid_users  : User_name_occurrence.t list User_name.Map.t
  ; mutable using_locked_sessions : User_name.Hash_set.t
  ; mutable serializer     : Serializer.t
  }
[@@deriving fields, sexp_of]

type origin =
  | Alias_of  of User_name.t
  | Typo_of   of User_name.t
  | Has_alias of Alternate_name.t
  | Has_typo  of Alternate_name.t
  | Valid_user
[@@deriving sexp_of]

let check_no_intersections_exn t =
  let origins = String.Table.create () in
  let add key data = Hashtbl.add_multi origins ~key ~data in
  Fields.Direct.iter t
    ~existing_users:(fun _ _ -> ignore)
    ~valid_users:(fun _ _ ->
      Hash_set.iter ~f:(fun u -> add (User_name.to_string u) Valid_user))
    ~aliases_seen:(fun _ _ ->
      User_name_by_alternate_name.iteri ~f:(fun ~key ~data ->
        add (Alternate_name.to_string key) (Alias_of data);
        add (User_name.to_string data) (Has_alias key)))
    ~typos:(fun _ _ ->
      User_name_by_alternate_name.iteri ~f:(fun ~key ~data ->
        add (Alternate_name.to_string key) (Typo_of data);
        add (User_name.to_string data) (Has_typo key)))
    ~invalid_users:(fun _ _ -> ignore)
    ~using_locked_sessions:(fun _ _ -> ignore)
    ~serializer:(fun _ _ -> ignore);
  let all_errors =
    Hashtbl.filter origins ~f:(fun origins ->
      match origins with
      | [] | [_] -> false
      | _ :: _ :: _ ->
        List.exists origins ~f:(function
          | Alias_of _ | Typo_of _ -> true
          | Has_alias _ | Has_typo _ | Valid_user -> false))
    |> Hashtbl.to_alist
    |> List.sort ~cmp:(fun (user1, _) (user2, _) -> String.compare user1 user2)
  in
  if not (List.is_empty all_errors)
  then
    raise_s
      [%sexp "collisions in the user info", (all_errors : (string * origin list) list)]
;;

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~existing_users:(check (fun existing_users ->
        Hash_set.iter existing_users ~f:User_name.invariant))
      ~valid_users:(check (fun valid_users ->
        Hash_set.iter valid_users ~f:User_name.invariant))
      ~aliases_seen:(check User_name_by_alternate_name.invariant)
      ~typos:(check User_name_by_alternate_name.invariant)
      ~using_locked_sessions:(check (fun using_locked_sessions ->
        Hash_set.iter using_locked_sessions ~f:User_name.invariant))
      ~serializer:(check Serializer.invariant)
      ~invalid_users:(check (fun map ->
        Map.iteri map ~f:(fun ~key ~data ->
          User_name.invariant key;
          assert (not (List.is_empty data)))));
    check_no_intersections_exn t)
;;

let aliases_file               = Relpath.of_string "aliases"
let existing_users_file        = Relpath.of_string "existing-users"
let typos_file                 = Relpath.of_string "typos"
let valid_users_file           = Relpath.of_string "valid-users"
let using_locked_sessions_file = Relpath.of_string "using-locked-sessions"

let persist_existing_users t =
  Serializer.set_contents t.serializer ~file:existing_users_file
    (t.existing_users
     |> Hash_set.to_list
     |> User_name.Set.of_list)
    (module Persist.User_names_set)
;;

let persist_valid_users t =
  Serializer.set_contents t.serializer ~file:valid_users_file
    (t.valid_users
     |> Hash_set.to_list
     |> User_name.Set.of_list)
    (module Persist.User_names_set)
;;

let persist_aliases t =
  Serializer.set_contents t.serializer ~file:aliases_file
    t.aliases_seen (module Persist.Alternate_names)
;;

let persist_typos t =
  Serializer.set_contents t.serializer ~file:typos_file
    t.typos (module Persist.Alternate_names)
;;

let persist_using_locked_sessions t =
  Serializer.set_contents t.serializer ~file:using_locked_sessions_file
    (t.using_locked_sessions
     |> Hash_set.to_list
     |> User_name.Set.of_list)
    (module Persist.User_names_set)
;;

let set_valid_users t value =
  t.valid_users <- value;
  persist_valid_users t;
;;

let set_aliases t value =
  t.aliases_seen <- value;
  persist_aliases t;
;;

let set_typos t value =
  t.typos <- value;
  persist_typos t;
;;

let user_exists t user_name = Hash_set.mem t.existing_users user_name

let ensure_user_exists t user_name =
  if not (user_exists t user_name)
  then raise_s [%sexp "never heard of user", (user_name : User_name.t)]
;;

let ensure_users_exist t user_names =
  let do_not_exist = List.filter user_names ~f:(fun u -> not (user_exists t u)) in
  if not (List.is_empty do_not_exist)
  then raise_s [%sexp "never heard of users", (do_not_exist : User_name.t list)]
;;

let are_acting_for_themselves_or_for_invalid_user t ~for_ ~by =
  let for_as_unresolved = User_name.to_unresolved_name for_ in
  let de_aliased =
    User_name_by_alternate_name.to_user_name t.aliases_seen for_as_unresolved
  in
  User_name.equal by de_aliased || not (Hash_set.mem t.valid_users for_)
;;

let dump t (which_user_info : Iron_protocol.Dump.Which_user_info.t) =
  match which_user_info with
  | Existing_users ->
    List.sort (Hash_set.to_list t.existing_users) ~cmp:User_name.compare
    |> [%sexp_of: User_name.t list]
  | Valid_users ->
    List.sort (Hash_set.to_list t.valid_users) ~cmp:User_name.compare
    |> [%sexp_of: User_name.t list]
  | Aliases -> t.aliases_seen |> [%sexp_of: User_name_by_alternate_name.t]
  | Typos   -> t.typos        |> [%sexp_of: User_name_by_alternate_name.t]
;;

let complete t ~prefix (which : Iron_protocol.Complete.Which_user_info.t) =
  let completions = ref [] in
  let maybe_add to_string elt =
    let string = to_string elt in
    if String.is_prefix ~prefix string
    then completions := string :: !completions
  in
  begin match which with
  | Existing_user ->
    Hash_set.iter t.existing_users
      ~f:(fun u -> maybe_add User_name.to_string u)
  | Typo ->
    User_name_by_alternate_name.iteri t.typos
      ~f:(fun ~key:u ~data:_ -> maybe_add Alternate_name.to_string u)
  | Alias ->
    User_name_by_alternate_name.iteri t.aliases_seen
      ~f:(fun ~key:u ~data:_ -> maybe_add Alternate_name.to_string u)
  end;
  !completions
;;

let refresh_existing_users t ~occurrences_by_user_name =
  let existing_users =
    User_name.Hash_set.of_list
      (Hashtbl.keys occurrences_by_user_name
       @ Hash_set.to_list t.valid_users)
  in
  let invalid_users =
    Hashtbl.filteri occurrences_by_user_name
      ~f:(fun ~key:user_name ~data:_ ->
        not (Hash_set.mem t.valid_users user_name))
    |> Hashtbl.to_alist
    |> User_name.Map.of_alist_exn
  in
  t.existing_users <- existing_users;
  t.invalid_users <- invalid_users;
  persist_existing_users t;
;;

let add_user_using_locked_sessions t user =
  if not (Hash_set.mem t.using_locked_sessions user) then begin
    Hash_set.add t.using_locked_sessions user;
    persist_using_locked_sessions t;
    Ok ()
  end
  else
    Or_error.error_s [%sexp "user is already using locked sessions", (user : User_name.t)]
;;

let remove_user_using_locked_sessions t user =
  if Hash_set.mem t.using_locked_sessions user then begin
    Hash_set.remove t.using_locked_sessions user;
    persist_using_locked_sessions t;
    Ok ()
  end
  else
    Or_error.error_s [%sexp "user is not using locked sessions", (user : User_name.t)]
;;

let users_using_locked_sessions t =
  t.using_locked_sessions
  |> Hash_set.to_list
  |> User_name.Set.of_list
;;

let alternate_names t ~which =
  match which with
  | `Aliases -> t.aliases_seen
  | `Typos   -> t.typos
  | `All     ->
    User_name_by_alternate_name.merge_exn t.aliases_seen t.typos
      ~on_error:(fun _ _ -> check_no_intersections_exn t; assert false)
;;

let remove_alternate_names_exn t alternate_names_to_remove ~which =
  let alternate_names, set, name =
    match which with
    | `Aliases -> t.aliases_seen, set_aliases, "aliases"
    | `Typos   -> t.typos       , set_typos  , "typos"
  in
  let unremoved, alternate_names =
    List.fold_left alternate_names_to_remove ~init:([], alternate_names)
      ~f:(fun (unremoved, acc) alternate_name ->
        match User_name_by_alternate_name.remove_if_present acc ~alternate_name with
        | None -> (alternate_name :: unremoved, acc)
        | Some acc -> (unremoved, acc))
  in
  set t alternate_names;
  match unremoved with
  | [] -> ()
  | _ :: _ ->
    failwiths (sprintf "The following %s were not removed: " name) unremoved
      [%sexp_of: Alternate_name.t list]
;;

let typo_conflict typo means =
  raise_s
    [%sexp
      "typo can be resolved to different user names",
      { typo  = (typo : Alternate_name.t)
      ; means = (means : User_name.t list)
      }
    ]
;;

let alias_conflict alias user_names =
  raise_s
    [%sexp
      "alias can be resolved to different user names",
      { alias      = (alias : Alternate_name.t)
      ; user_names = (user_names : User_name.t list)
      }
    ]
;;

let define_typos_exn t (definitions : Iron_protocol.Define_typos.Definition.t list) =
  let typos =
    List.fold_left definitions ~init:t.typos ~f:(fun typos { typo; means } ->
      let typo_as_user_name = User_name.of_string (Alternate_name.to_string typo) in
      let typo_as_unresolved = Unresolved_name.of_string (Alternate_name.to_string typo) in
      let means_as_unresolved = Unresolved_name.of_string (User_name.to_string means) in
      if Hash_set.mem t.valid_users typo_as_user_name
      then failwithf !"%{Alternate_name} is a valid user name" typo ();
      List.iter [ t.aliases_seen, "an alias"; t.typos, "a typo" ] ~f:(fun (map, name) ->
        match User_name_by_alternate_name.to_user_name_opt map means_as_unresolved with
        | None -> ()
        | Some resolved ->
          failwithf !"%{User_name} is already %s for %{User_name}" means name resolved ()
      );
      begin
        match
          User_name_by_alternate_name.to_user_name_opt t.aliases_seen typo_as_unresolved
        with
        | None -> ()
        | Some not_alias ->
          failwithf !"%{Alternate_name} is already an alias for %{User_name}"
            typo not_alias ()
      end;
      User_name_by_alternate_name.add_exn typos ~alternate_name:typo ~user_name:means
        ~on_error:typo_conflict)
  in
  set_typos t typos
;;

let update_valid_users_and_aliases_exn t user_names_and_aliases =
  let module M = Iron_protocol.Update_valid_users_and_aliases.User_aliases in
  let valid_users =
    List.map user_names_and_aliases ~f:(fun r -> r.M.user_name)
    |> User_name.Hash_set.of_list
  in
  (* We start with [t.aliases_seen] to accumulate aliases, so that when a person leaves,
     we don't forget about his aliases. *)
  let aliases_seen =
    List.fold_left user_names_and_aliases ~init:t.aliases_seen
      ~f:(fun acc { M.user_name; aliases } ->
        List.fold_left aliases ~init:acc ~f:(fun acc alias ->
          User_name_by_alternate_name.add_exn acc ~alternate_name:alias ~user_name
            ~on_error:alias_conflict))
  in
  check_no_intersections_exn { t with valid_users; aliases_seen };
  set_valid_users t valid_users;
  set_aliases t aliases_seen;
;;

let deserializer = Deserializer.with_serializer (fun serializer ->
  let open Deserializer.Let_syntax in
  let%map_open () = return ()
  and existing_users =
    one (module Persist.User_names_set)
      ~default:User_name.Set.empty
      ~in_file:existing_users_file
  and valid_users =
    one (module Persist.User_names_set)
      ~default:User_name.Set.empty
      ~in_file:valid_users_file
  and aliases_seen =
    one (module Persist.Alternate_names)
      ~default:User_name_by_alternate_name.not_available
      ~in_file:aliases_file
  and typos =
    one (module Persist.Alternate_names)
      ~default:User_name_by_alternate_name.not_available
      ~in_file:typos_file
  and using_locked_sessions =
    one (module Persist.User_names_set)
      ~default:User_name.Set.empty
      ~in_file:using_locked_sessions_file
  in
  { existing_users = User_name.Hash_set.of_list (Set.to_list existing_users)
  ; valid_users    = User_name.Hash_set.of_list (Set.to_list valid_users)
  ; aliases_seen
  ; typos
  ; invalid_users  = User_name.Map.empty
  ; using_locked_sessions
    = User_name.Hash_set.of_list (Set.to_list using_locked_sessions)
  ; serializer
  }
)
;;

