module Stable = struct
  open! Core.Core_stable
  open! Import_stable
  module User_info = struct
    module V2 = struct
      type t =
        { machines : Unclean_workspace.V2.t list Machine_name.V1.Map.t
        ; query    : unit Query.V1.t
        }
      [@@deriving sexp]
      let to_model (t : t) = t
      let of_model (t : t) = t
    end
    module V1 = struct
      type t =
        { machines : Unclean_workspace.V1.t list Machine_name.V1.Map.t
        ; query    : unit Query.V1.t
        }
      [@@deriving sexp]

      open! Core
      open! Import

      let to_model { machines; query } =
        V2.to_model
          { machines = Map.map machines ~f:(List.map ~f:Unclean_workspace.Stable.V1.to_v2)
          ; query
          }
      ;;

      let of_model m =
        let { V2.machines; query } = V2.of_model m in
        { machines = Map.map machines ~f:(List.map ~f:Unclean_workspace.Stable.V1.of_v2)
        ; query
        }
      ;;
    end
    module Model = V2
  end
end

open! Core
open! Async
open! Import

module By_machine_name = struct
  type t = Unclean_workspace_reason.t Machine_name.Table.t
  [@@deriving sexp_of]

  let invariant t =
    Hashtbl.iteri t ~f:(fun ~key:machine_name ~data:reason ->
      Invariant.invariant [%here] machine_name [%sexp_of: Machine_name.t] (fun () ->
        Unclean_workspace_reason.invariant reason))
  ;;
end

module Index = Hashtbl2_pair.Make (User_name) (Feature_path)

type t =
  { entries               : By_machine_name.t Index.t
  ; query_by_user         : unit Query.t User_name.Table.t
  ; dynamic_upgrade_state : Dynamic_upgrade.State.t
  ; serializer            : Serializer.t
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~entries:(check (Hashtbl2_pair.invariant
                         User_name.invariant
                         Feature_path.invariant
                         By_machine_name.invariant))
      ~query_by_user:(check ignore)
      ~dynamic_upgrade_state:(check Dynamic_upgrade.State.Reference.invariant)
      ~serializer:(check Serializer.invariant))
;;

let indexed_by_machine_name by_feature_path =
  let all_by_machine_name = Machine_name.Table.create () in
  Hashtbl.iteri by_feature_path ~f:(fun ~key:feature_path ~data:by_machine_name ->
    Hashtbl.iteri by_machine_name ~f:(fun ~key:machine_name ~data:reason ->
      Hashtbl.add_multi all_by_machine_name ~key:machine_name
        ~data:{ Unclean_workspace. feature_path; reason }));
  Hashtbl.to_alist all_by_machine_name
  |> Machine_name.Map.of_alist_exn
;;

let dump t what_users =
  list_of_iter (fun ~f ->
    let f user by_feature_path = f (user, indexed_by_machine_name by_feature_path) in
    match what_users with
    | `All_users -> Hashtbl2_pair.iter1 t.entries ~f
    | `User user -> Option.iter (Hashtbl2_pair.find1 t.entries user) ~f:(f user))
  |> List.map ~f:(fun (user, machines) ->
    user, machines, Hashtbl.find t.query_by_user user)
  |> [%sexp_of: (User_name.t
                 * Unclean_workspace.t list Machine_name.Map.t
                 * unit Query.t option) list]
;;

let find_user t user =
  match Hashtbl2_pair.find1 t.entries user with
  | None -> Machine_name.Map.empty
  | Some by_feature_path -> indexed_by_machine_name by_feature_path
;;

let find_feature t feature_path =
  match Hashtbl2_pair.find2 t.entries feature_path with
  | None -> User_name.Map.empty
  | Some by_user ->
    by_user
    |> Hashtbl.to_alist
    |> List.filter_map ~f:(fun (user, by_machine_name) ->
      Option.map ~f:(fun reason -> user, reason)
        (List.reduce (Hashtbl.data by_machine_name) ~f:Unclean_workspace_reason.add))
    |> User_name.Map.of_alist_exn
;;

let users t =
  list_of_iter (fun ~f -> Hashtbl2_pair.iter1 t.entries ~f:(fun key _ -> f key))
  |> User_name.Set.of_list
;;

let set_reason t user_name feature_path machine_name reason =
  match Hashtbl2_pair.find t.entries user_name feature_path with
  | Some by_machine_name ->
    let status = ref `Was_already_set in
    Hashtbl.update by_machine_name machine_name ~f:(fun previous ->
      status :=
        (match previous with
         | None -> `Done
         | Some previous_reason ->
           if Unclean_workspace_reason.equal previous_reason reason
           then `Was_already_set
           else `Done);
      reason);
    !status

  | None ->
    let by_machine_name = Machine_name.Table.create () in
    Hashtbl2_pair.set t.entries user_name feature_path by_machine_name;
    Hashtbl.set by_machine_name ~key:machine_name ~data:reason;
    `Done
;;

let set_reason_internal t user_name feature_path machine_name reason =
  match set_reason t user_name feature_path machine_name reason with
  | `Was_already_set | `Done -> ()
;;

let user_subtree user =
  Relpath.of_list [ File_name.of_string (User_name.to_string user) ]
;;

let workspaces_file = File_name.of_string "workspaces"

module Context = struct
  type nonrec t = t [@@deriving sexp_of]
end

module Persist = struct
  module User_info = struct
    include Persistent.Make_with_context
        (Context)
        (struct let version = 2 end)
        (Stable.User_info.V2)
    include Register_read_write_old_version
        (struct let version = 1 end)
        (struct
          include Stable.User_info.V1
          let to_model _ t = Stable.User_info.V1.to_model t
          let of_model _ m = Stable.User_info.V1.of_model m
          let should_write_this_version t =
            match
              Dynamic_upgrade.commit_to_upgrade t.dynamic_upgrade_state
                ~allowed_from:U2
            with
            | `Not_allowed_yet -> true
            | `Ok              -> false
        end)
  end
end

module User_info = Stable.User_info.Model

let persist_user t query user_name =
  let query = Query.with_action query () in
  let user_info =
    { User_info.
      machines = find_user t user_name
    ; query
    }
  in
  Hashtbl.set t.query_by_user ~key:user_name ~data:query;
  Serializer.set_contents t.serializer
    ~file:(Relpath.extend (user_subtree user_name) workspaces_file)
    (t, user_info) (module Persist.User_info.Writer)
;;

let deserializer ~dynamic_upgrade_state =
  Deserializer.with_serializer (fun serializer ->
    let open Deserializer.Let_syntax in
    let%map_open () = return ()
    and users =
      all_subdirs
        (one (module Persist.User_info.Reader)
           ~in_file:(Relpath.of_list [ workspaces_file ]))
    in
    let t =
      { entries       = Index.create ()
      ; query_by_user = User_name.Table.create ()
      ; dynamic_upgrade_state
      ; serializer
      }
    in
    Map.iteri users ~f:(fun ~key:file ~data ->
      let { User_info. machines; query } = data t in
      let user_name = User_name.of_string (File_name.to_string file) in
      Hashtbl.set t.query_by_user ~key:user_name ~data:query;
      Map.iteri machines ~f:(fun ~key:machine_name ~data:unclean_workspaces ->
        List.iter unclean_workspaces ~f:(fun { feature_path; reason } ->
          set_reason_internal t user_name feature_path machine_name reason)));
    t)
;;

let remove_user_exn t _query user_name =
  if not (Hashtbl2_pair.mem1 t.entries user_name)
  then raise_s [%sexp "no such user", (user_name : User_name.t)]
  else (
    Hashtbl.remove t.query_by_user user_name;
    Hashtbl2_pair.remove_all1 t.entries user_name;
    Serializer.remove_subtree t.serializer ~dir:(user_subtree user_name))
;;

let mutating_iteri table ~f:mutating_f =
  List.iter (Hashtbl.to_alist table) ~f:(fun (a, b) -> mutating_f ~key:a ~data:b)
;;

let remove_machine_exn t query user_name machine_name =
  match Hashtbl2_pair.find1 t.entries user_name with
  | None -> raise_s [%sexp "no such user", (user_name : User_name.t)]
  | Some by_feature_path ->
    let removed = ref false in
    mutating_iteri by_feature_path ~f:(fun ~key:feature_path ~data:by_machine_name ->
      if Hashtbl.mem by_machine_name machine_name
      then (
        removed := true;
        Hashtbl.remove by_machine_name machine_name;
        if Hashtbl.is_empty by_machine_name
        then Hashtbl2_pair.remove_exn t.entries user_name feature_path));
    (if not !removed
     then
       raise_s
         [%sexp
           "no such machine for user",
           { user_name    : User_name.t
           ; machine_name : Machine_name.t
           }
         ]);
    persist_user t query user_name;
;;

let update t query
      { Iron_protocol.Update_unclean_workspaces.Action.
        for_               = user_name
      ; machine_name
      ; unclean_workspaces
      ; clean_workspaces
      } =
  let was_changed = ref false in
  let remove_from_machines feature_path by_machine_name =
    if Hashtbl.mem by_machine_name machine_name
    then (
      was_changed := true;
      Hashtbl.remove by_machine_name machine_name;
      if Hashtbl.is_empty by_machine_name
      then Hashtbl2_pair.remove_exn t.entries user_name feature_path)
  in
  (match Hashtbl2_pair.find1 t.entries user_name with
   | None -> ()
   | Some by_feature_path ->
     match clean_workspaces with
     | `At_least_these feature_paths ->
       Set.iter (Feature_path.Set.of_list feature_paths) ~f:(fun feature_path ->
         Option.iter (Hashtbl.find by_feature_path feature_path)
           ~f:(remove_from_machines feature_path))

     | `Complement_of_those_listed_as_unclean ->
       let listed_as_unclean =
         unclean_workspaces
         |> List.map ~f:Unclean_workspace.feature_path
         |> Feature_path.Set.of_list
       in
       mutating_iteri by_feature_path ~f:(fun ~key:feature_path ~data:by_machine_name ->
         if not (Set.mem listed_as_unclean feature_path)
         then remove_from_machines feature_path by_machine_name));
  List.iter unclean_workspaces ~f:(fun { Unclean_workspace. feature_path; reason } ->
    match set_reason t user_name feature_path machine_name reason with
    | `Was_already_set -> ()
    | `Done -> was_changed := true
  );
  if !was_changed then persist_user t query user_name
;;
