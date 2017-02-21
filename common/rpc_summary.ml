open! Core
open! Import

type 'a contents =
  | Registering of (Rpc_description.t Int.Table.t * 'a) String.Table.t
  | Finalized   of (Rpc_description.t Int.Map.t   * 'a) String.Map.t

and 'a t = 'a contents ref

let create () = { contents = Registering (String.Table.create ()) }

let finalized t =
  match t.contents with
  | Finalized t -> t
  | Registering contents ->
    let contents =
      String.Map.of_hashtbl_exn contents
      |> Map.map ~f:(Tuple2.map_fst ~f:Int.Map.of_hashtbl_exn)
    in
    t.contents <- Finalized contents;
    contents
;;

let assert_registration_still_allowed t name version =
  match t.contents with
  | Registering contents -> contents
  | Finalized _ ->
    raise_s [%sexp "Cannot register after querying the summary."
                 , { name    : string
                   ; version : int
                   }]
;;

let rpc_descriptions t =
  lazy (List.concat_map (Map.data (finalized t)) ~f:(fun (descriptions, _) ->
    Map.data descriptions))
;;

let arg_type t = Command.Arg_type.of_map (Map.map (finalized t) ~f:snd)

let find_rpc_exn t ~name ~version =
  match Map.find (finalized t) name with
  | None -> raise_s [%sexp "Unknown rpc.", (name : string)]
  | Some (descriptions, _) ->
    match Map.find descriptions version with
    | None -> raise_s [%sexp "Unknown version.", (name : string), (version : int)]
    | Some description -> description
;;

let register t ~name ~version ~query ~response ~metadata =
  let t = assert_registration_still_allowed t name version in
  let description = Rpc_description.create ~name ~version ~query ~response in
  let versions = Int.Table.of_alist_exn [ version, description ] in
  Hashtbl.add_exn t ~key:name ~data:(versions, metadata)
;;

let register_old_version t ~name ~version ~query ~response =
  let t = assert_registration_still_allowed t name version in
  let description = Rpc_description.create ~name ~version ~query ~response in
  match Hashtbl.find t name with
  | Some (versions, _) -> Hashtbl.set versions ~key:version ~data:description
  | None ->
    raise_s [%sexp "Iron bug.  Registered old version before registering rpc."
                 , { name    : string
                   ; version : int
                   }]
;;
