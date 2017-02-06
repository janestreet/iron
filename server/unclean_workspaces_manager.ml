module Stable = struct
  open! Core.Core_stable
  open! Import_stable
  module User_info = struct
    module V1 = struct
      type t =
        { machines : Unclean_workspace.V1.t list Machine.V1.Map.t
        ; query    : unit Query.V1.t
        }
      [@@deriving sexp]
    end
  end
end

open! Core
open! Async
open! Import

module By_machine = struct
  type t = Unclean_workspace_reason.t Machine.Table.t
  [@@deriving sexp_of]

  let invariant t =
    Hashtbl.iteri t ~f:(fun ~key:machine ~data:reason ->
      Invariant.invariant [%here] machine [%sexp_of: Machine.t] (fun () ->
        Unclean_workspace_reason.invariant reason))
  ;;
end

module Index = Hashtbl2_pair.Make (User_name) (Feature_path)

type t =
  { entries       : By_machine.t Index.t
  ; query_by_user : unit Query.t User_name.Table.t
  ; serializer    : Serializer.t
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~entries:(check (Hashtbl2_pair.invariant
                         User_name.invariant
                         Feature_path.invariant
                         By_machine.invariant))
      ~query_by_user:(check ignore)
      ~serializer:(check Serializer.invariant))
;;

let indexed_by_machine by_feature_path =
  let all_by_machine = Machine.Table.create () in
  Hashtbl.iteri by_feature_path ~f:(fun ~key:feature_path ~data:by_machine ->
    Hashtbl.iteri by_machine ~f:(fun ~key:machine ~data:reason ->
      Hashtbl.add_multi all_by_machine ~key:machine
        ~data:{ Unclean_workspace. feature_path; reason }));
  Hashtbl.to_alist all_by_machine
  |> Machine.Map.of_alist_exn
;;

let dump t what_users =
  list_of_iter (fun ~f ->
    let f user by_feature_path = f (user, indexed_by_machine by_feature_path) in
    match what_users with
    | `All_users -> Hashtbl2_pair.iter1 t.entries ~f
    | `User user -> Option.iter (Hashtbl2_pair.find1 t.entries user) ~f:(f user))
  |> List.map ~f:(fun (user, machines) ->
    user, machines, Hashtbl.find t.query_by_user user)
  |> [%sexp_of: (User_name.t
                 * Unclean_workspace.t list Machine.Map.t
                 * unit Query.t option) list]
;;

let find_user t user =
  match Hashtbl2_pair.find1 t.entries user with
  | None -> Machine.Map.empty
  | Some by_feature_path -> indexed_by_machine by_feature_path
;;

let find_feature t feature_path =
  match Hashtbl2_pair.find2 t.entries feature_path with
  | None -> User_name.Map.empty
  | Some by_user ->
    by_user
    |> Hashtbl.to_alist
    |> List.filter_map ~f:(fun (user, by_machine) ->
      Option.map ~f:(fun reason -> user, reason)
        (List.reduce (Hashtbl.data by_machine) ~f:Unclean_workspace_reason.add))
    |> User_name.Map.of_alist_exn
;;

let users t =
  list_of_iter (fun ~f -> Hashtbl2_pair.iter1 t.entries ~f:(fun key _ -> f key))
  |> User_name.Set.of_list
;;

let set_reason t user_name feature_path machine reason =
  match Hashtbl2_pair.find t.entries user_name feature_path with
  | Some by_machine ->
    let status = ref `Was_already_set in
    Hashtbl.update by_machine machine ~f:(fun previous ->
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
    let by_machine = Machine.Table.create () in
    Hashtbl2_pair.set t.entries user_name feature_path by_machine;
    Hashtbl.set by_machine ~key:machine ~data:reason;
    `Done
;;

let set_reason_internal t user_name feature_path machine reason =
  match set_reason t user_name feature_path machine reason with
  | `Was_already_set | `Done -> ()
;;

let user_subtree user =
  Relpath.of_list [ File_name.of_string (User_name.to_string user) ]
;;

let workspaces_file = File_name.of_string "workspaces"

module Persist = struct
  module User_info = struct
    include Persistent.Make
        (struct let version = 1 end)
        (Stable.User_info.V1)
  end
end

module User_info = Stable.User_info.V1

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
    user_info (module Persist.User_info)
;;

let deserializer = Deserializer.with_serializer (fun serializer ->
  let open Deserializer.Let_syntax in
  let%map_open () = return ()
  and users =
    all_subdirs
      (one (module Persist.User_info)
         ~in_file:(Relpath.of_list [ workspaces_file ]))
  in
  let t =
    { entries       = Index.create ()
    ; query_by_user = User_name.Table.create ()
    ; serializer
    }
  in
  Map.iteri users ~f:(fun ~key:file ~data:{ User_info. machines; query } ->
    let user_name = User_name.of_string (File_name.to_string file) in
    Hashtbl.set t.query_by_user ~key:user_name ~data:query;
    Map.iteri machines ~f:(fun ~key:machine ~data:unclean_workspaces ->
      List.iter unclean_workspaces ~f:(fun { feature_path; reason } ->
        set_reason_internal t user_name feature_path machine reason)));
  t
)
;;

let remove_user_exn t _query user_name =
  if not (Hashtbl2_pair.mem1 t.entries user_name)
  then raise_s [%sexp "no such user", (user_name : User_name.t)]
  else (
    Hashtbl.remove t.query_by_user user_name;
    Hashtbl2_pair.remove_all1 t.entries user_name;
    Serializer.remove_subtree t.serializer ~dir:(user_subtree user_name))
;;

let remove_machine_exn t query user_name machine =
  match Hashtbl2_pair.find1 t.entries user_name with
  | None -> raise_s [%sexp "no such user", (user_name : User_name.t)]
  | Some by_feature_path ->
    let removed = ref false in
    List.iter (Hashtbl.to_alist by_feature_path) ~f:(fun (feature_path, by_machine) ->
      if Hashtbl.mem by_machine machine
      then (
        removed := true;
        Hashtbl.remove by_machine machine;
        if Hashtbl.is_empty by_machine
        then Hashtbl2_pair.remove_exn t.entries user_name feature_path));
    (if not !removed
     then
       raise_s
         [%sexp
           "no such machine for user",
           { user    = (user_name : User_name.t)
           ; machine : Machine.t
           }
         ]);
    persist_user t query user_name;
;;

let update t query
      { Iron_protocol.Update_unclean_workspaces.Action.
        for_               = user_name
      ; machine
      ; unclean_workspaces
      ; clean_workspaces
      } =
  let was_changed = ref false in
  let remove_from_machines feature_path by_machine =
    if Hashtbl.mem by_machine machine
    then (
      was_changed := true;
      Hashtbl.remove by_machine machine;
      if Hashtbl.is_empty by_machine
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
       List.iter (Hashtbl.to_alist by_feature_path) ~f:(fun (feature_path, by_machine) ->
         if not (Set.mem listed_as_unclean feature_path)
         then remove_from_machines feature_path by_machine));
  List.iter unclean_workspaces ~f:(fun { Unclean_workspace. feature_path; reason } ->
    match set_reason t user_name feature_path machine reason with
    | `Was_already_set -> ()
    | `Done -> was_changed := true
  );
  if !was_changed then persist_user t query user_name
;;
