open! Core
open! Import

module Id = Uuid

module Action = struct
  module Next_base_update_expiration = struct
    type t =
      { feature_id : Feature_id.t
      }
    [@@deriving compare, fields, sexp_of]

    let invariant t =
      let check f = Invariant.check_field t f in
      Fields.iter
        ~feature_id:(check Feature_id.invariant)
    ;;
  end

  type t =
    | Next_base_update_expiration of Next_base_update_expiration.t
  [@@deriving compare, sexp_of]

  let invariant = function
    | Next_base_update_expiration next_base_update_expiration ->
      Next_base_update_expiration.invariant next_base_update_expiration
  ;;
end

type t =
  { id     : Id.t
  ; action : Action.t
  ; event  : Clock.Event.t_unit
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~action:(check Action.invariant)
      ~id:(check Id.invariant)
      ~event:
        (check (Clock.Event.invariant (ignore : unit -> unit) (ignore : unit -> unit)))
  )
;;

let sort_by_scheduled_time =
  let compare =
    let f field compare x y = compare (Field.get field x) (Field.get field y) in
    let compare_by compare ~f x y = compare (f x) (f y) in
    Comparable.lexicographic
      [ f Fields.event (compare_by Time.compare ~f:Clock.Event.scheduled_at)
      ; f Fields.id    Id.compare
      ]
  in
  List.sort ~cmp:compare
;;

module Table = struct
  type timed_event = t
  [@@deriving sexp_of]


  type t =
    { errors  : Error.t Queue.t
    ; execute : (Id.t -> Action.t -> unit) Set_once.t
    ; events  : timed_event Id.Table.t
    }
  [@@deriving fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~errors:ignore
        ~execute:ignore
        ~events:(check (Hashtbl.iteri ~f:(fun ~key:id ~data ->
          [%test_result: Id.t] ~expect:id data.id;
          invariant data
        ))))
  ;;

  let dump { errors
           ; execute = _
           ; events
           } =
    [%sexp
      { errors : Error.t Queue.t
      ; events = (Hashtbl.data events |> sort_by_scheduled_time : timed_event list)
      }
    ]
  ;;

  let add_error t err =
    if Queue.length t.errors >= 500
    then ignore (Queue.dequeue_exn t.errors : Error.t);
    Queue.enqueue t.errors err;
  ;;

  let execute t id action =
    Hashtbl.remove t.events id;
    try (Set_once.get_exn t.execute [%here]) id action
    with exn ->
      add_error t
        (Error.create_s
           [%sexp "exception during execution of timed_event"
                , { id     : Id.t
                  ; action : Action.t
                  ; exn    : Exn.t
                  }
           ])
  ;;

  module Errors = struct
    let get t = Queue.to_list t.errors
    let clear t = Queue.clear t.errors
  end
end

let the_table = Memo.unit (fun () ->
  { Table.
    errors  = Queue.create ()
  ; execute = Set_once.create ()
  ; events  = Id.Table.create ()
  })
;;

let set_execute_exn ~execute =
  Set_once.set_exn (the_table ()).execute [%here] execute
;;

let abort_if_possible t =
  let table = the_table () in
  Clock.Event.abort_if_possible t.event ();
  Hashtbl.remove table.events t.id;
;;

let has_id t id = Id.equal t.id id

let scheduled_at t = Clock.Event.scheduled_at t.event

let run when_ action =
  let table = the_table () in
  let id = Id.create () in
  let event =
    let run () = Table.execute table id action in
    match when_ with
    | `After span -> Clock.Event.run_after span run ()
    | `At    time -> Clock.Event.run_at    time run ()
  in
  let t = { action; id; event } in
  Hashtbl.set table.events ~key:id ~data:t;
  t
;;
