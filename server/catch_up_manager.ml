open Core
open Import

module Heap = Heap.Removable

module T = struct
  (* The type is polymorphic so we can feed it to Container.Make. *)
  type 'a t =
    { by_id            : 'a Heap.Elt.t Session_id.Table.t
    ; by_creation_time : 'a Heap.t
    }
  [@@deriving fields, sexp_of]

  module Container = Container.Make (struct
      type nonrec 'a t = 'a t
      let fold t = Heap.fold t.by_creation_time
      let iter = `Define_using_fold
    end)

  include Container

  let invariant id invariant_a t =
    Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      Fields.iter
        ~by_id:ignore
        ~by_creation_time:ignore;
      iter t ~f:invariant_a;
      let heap_as_table =
        Heap.to_list t.by_creation_time
        |> List.map ~f:(fun session -> id session, session)
        |> Session_id.Table.of_alist_exn
      in
      [%test_result: Int.t] ~expect:(Hashtbl.length t.by_id)
        (Hashtbl.length heap_as_table);
      Hashtbl.iteri t.by_id ~f:(fun ~key:id ~data:elt ->
        let session = Heap.Elt.value_exn elt in
        match Hashtbl.find heap_as_table id with
        | Some session' ->
          if not (phys_equal session session')
          then raise_s [%sexp "session mismatch", (id : Session_id.t)]
        | None ->
          raise_s [%sexp "session missing from the heap", (id : Session_id.t)]))
  ;;

  let mem t catch_up_session =
    Hashtbl.mem t.by_id (Catch_up_session.id catch_up_session)
  ;;

  let find t id =
    match Hashtbl.find t.by_id id with
    | Some session -> Some (Heap.Elt.value_exn session)
    | None -> None
  ;;

  let add t session =
    let id = Catch_up_session.id session in
    let elt =
      match Hashtbl.find t.by_id id with
      | None     -> Heap.add_removable t.by_creation_time session
      | Some elt -> Heap.update t.by_creation_time elt session
    in
    Hashtbl.set t.by_id ~key:id ~data:elt;
  ;;

  let remove_link t id =
    match Hashtbl.find t.by_id id with
    | None -> ()
    | Some elt ->
      Heap.remove t.by_creation_time elt;
      Hashtbl.remove t.by_id id;
  ;;

  let create ~heap_cmp () =
    { by_id            = Session_id.Table.create ()
    ; by_creation_time = Heap.create ~cmp:heap_cmp ()
    }
  ;;

  let is_empty t = Hashtbl.is_empty t.by_id
  ;;
end

open T
include (T : module type of T with type 'a t := 'a T.t)

type t = Catch_up_session.t T.t [@@deriving sexp_of]

let invariant t =
  invariant Catch_up_session.id Catch_up_session.invariant t;
;;

let heap_cmp a b =
  Time.compare (Catch_up_session.creation_time a) (Catch_up_session.creation_time b)
;;

let create () = create ~heap_cmp ()
;;

let line_count_remaining_to_catch_up t =
  fold t ~init:Line_count.Catch_up.zero ~f:(fun acc session ->
    Line_count.Catch_up.(acc + Catch_up_session.line_count_remaining_to_catch_up session))
;;

let to_protocol t session ~is_archived ~lines_required_to_separate_ddiff_hunks =
  let module Creation = Catch_up_session.Creation in
  let creation = Catch_up_session.creation session in
  { Iron_protocol.Get_catch_up_session.Catch_up_session.
    catch_up_session_id              = Creation.session_id                 creation
  ; catch_up_session_tip             = Creation.session_tip                creation
  ; creation_time                    = Catch_up_session.creation_time      session
  ; reviewer_in_session              = Catch_up_session.reviewer           session
  ; diff4s_to_catch_up               = Catch_up_session.diff4s_to_catch_up session
  ; line_count_remaining_to_catch_up = line_count_remaining_to_catch_up t
  ; remote_rev_zero                  = Creation.remote_rev_zero            creation
  ; remote_repo_path                 = Creation.remote_repo_path           creation
  ; feature_id                       = Creation.feature_id                 creation
  ; feature_path                     = Catch_up_session.feature_path       session
  ; whole_feature_reviewers          = Creation.whole_feature_reviewers    creation
  ; owners                           = Creation.owners                     creation
  ; base                             = Creation.base                       creation
  ; tip                              = Creation.tip                        creation
  ; description                      = Creation.description                creation
  ; is_permanent                     = Creation.is_permanent               creation
  ; is_archived
  ; seconder                         = Creation.seconder                   creation
  ; lines_required_to_separate_ddiff_hunks
  }
;;

let get_next_session t =
  match Heap.top t.by_creation_time with
  | None         -> `Up_to_date
  | Some session -> `Catch_up_session session
;;

let find_all t ~f =
  let found = ref [] in
  Heap.iter t.by_creation_time ~f:(fun session ->
    if f session then found := session :: !found);
  !found
;;

let remove_all_links t ~f =
  let to_remove = find_all t ~f in
  List.iter to_remove
    ~f:(fun session -> remove_link t (Catch_up_session.id session));
  to_remove
;;

let has_feature_id feature_id session =
  Feature_id.equal feature_id (Catch_up_session.feature_id session)
;;

let has_feature_path feature_path session =
  Feature_path.equal feature_path (Catch_up_session.feature_path session)
;;

let find_all_for_feature_id t feature_id =
  find_all t ~f:(has_feature_id feature_id)
;;

let remove_all_links_for_feature_id t feature_id =
  remove_all_links t ~f:(has_feature_id feature_id)
;;

let find_all_for_feature_path t feature_path =
  find_all t ~f:(has_feature_path feature_path)
;;
