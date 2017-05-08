open! Core
open! Import

module Virtual_time : sig
  type t [@@deriving sexp_of]

  include Comparable.S with type t := t
  include Invariant.S  with type t := t

  val tick : unit -> t
end = struct
  include Int

  let start_time = 0

  let invariant t = assert (t >= start_time)

  let now_ref = ref start_time

  let tick () = incr now_ref; !now_ref
end

module Invalidator = struct

  type t =
    { mutable last_changed_at : Virtual_time.t
    ; mutable callback        : (unit -> unit) option
    ; debug_information       : Sexp.t
    }
  [@@deriving fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~last_changed_at:(check Virtual_time.invariant)
        ~callback:(check (ignore : ((unit -> unit) option) -> unit))
        ~debug_information:(check (ignore : Sexp.t -> unit)))
  ;;

  let equal (t1 : t) t2 = phys_equal t1 t2

  let invalidate_dependents t =
    t.last_changed_at <- Virtual_time.tick ();
    Option.iter t.callback ~f:(fun f ->
      try f ()
      with _ when not am_functional_testing -> ())
  ;;

  let set_callback t ~callback_no_exn =
    t.callback <- Some callback_no_exn;
  ;;

  let create ~debug_information =
    { last_changed_at = Virtual_time.tick ()
    ; callback = None
    ; debug_information
    }
  ;;
end

module Cached_result = struct
  type 'a t =
    { computation_initiated_at : Virtual_time.t
    ; computation_finished_at  : Virtual_time.t
    ; depends_on  : Invalidator.t list
    ; result      : 'a Or_error.t
    }
  [@@deriving fields, sexp_of]

  let invariant invariant_a t =
    Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~computation_initiated_at:(check Virtual_time.invariant)
        ~computation_finished_at:(check Virtual_time.invariant)
        ~depends_on:(check (List.iter ~f:Invalidator.invariant))
        ~result:(check (Or_error.invariant invariant_a)))
  ;;

  let needs_recomputation t ~depends_on =
    not (List.equal depends_on t.depends_on ~equal:Invalidator.equal)
    || List.exists depends_on
         ~f:(fun invalidator ->
           Virtual_time.( <= )
             t.computation_initiated_at (Invalidator.last_changed_at invalidator))
  ;;
end

type 'a t =
  { compute_depends_on    : unit -> Invalidator.t list
  ; equal                 : 'a -> 'a -> bool
  ; sexp_of_a             : 'a -> Sexp.t
  ; compute_result        : unit -> 'a
  ; mutable cached_result : 'a Cached_result.t option
  }
[@@deriving fields, sexp_of]

let invariant invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~compute_depends_on:ignore
      ~equal:ignore
      ~sexp_of_a:ignore
      ~compute_result:ignore
      ~cached_result:(check (Option.invariant (Cached_result.invariant invariant_a ))))
;;

type packed = T : _ t -> packed

module type S = sig
  type t [@@deriving sexp_of]
  include Equal.S with type t := t
end

let create (type a) (module M : S with type t = a) ~compute_result ~compute_depends_on =
  { compute_depends_on
  ; equal              = M.equal
  ; sexp_of_a          = [%sexp_of: M.t]
  ; compute_result
  ; cached_result      = None
  }
;;

let uninitialized ~name () =
  { compute_depends_on = (fun () -> [])
  ; equal              = (fun _ _ -> false)
  ; sexp_of_a          = const (Sexp.Atom "<uninitialized>")
  ; compute_result     = (fun _ ->
      raise_s [%sexp "attempt to compute uninitialized Cached.t", (name : string)])
  ; cached_result      = None
  }
;;

let set t cached_result =
  t.cached_result <- Some cached_result
;;

let compute_depends_on t = Or_error.try_with t.compute_depends_on

let status t =
  match compute_depends_on t with
  | Error error ->
    `Error_computing_dependencies (Error.tag error ~tag:"Error computing dependencies")
  | Ok depends_on ->
    match t.cached_result with
    | None -> `Needs_recomputation depends_on
    | Some cached_result ->
      if Cached_result.needs_recomputation cached_result ~depends_on
      then `Needs_recomputation depends_on
      else `Up_to_date cached_result
;;

let compute_cached_result (type a) (t : a t) ~depends_on =
  (* We need to register the time as of before the computation of the result in case
     this computation invalidates the cache *)
  let computation_initiated_at = Virtual_time.tick () in
  let result = Or_error.try_with t.compute_result in
  let computation_finished_at = Virtual_time.tick () in
  let cached_result : a Cached_result.t =
    { computation_initiated_at
    ; computation_finished_at
    ; depends_on
    ; result
    }
  in
  if List.exists depends_on ~f:(fun (t : Invalidator.t) ->
    Virtual_time.( >= ) t.last_changed_at computation_initiated_at)
  then (
    let sexp =
      (* Force the creation of the sexp here to guarantee consistency of the values,
         since some of the values contained are mutable. *)
      let sexp_of_a = t.sexp_of_a in
      cached_result |> [%sexp_of: a Cached_result.t]
    in
    { cached_result with
      result = Or_error.error "Cached.compute_result modified one of its invalidators"
                 sexp [%sexp_of: Sexp.t]
    })
  else cached_result
;;

let clear t = t.cached_result <- None

let get t =
  match status t with
  | `Up_to_date cached_result           -> cached_result.result
  | `Error_computing_dependencies error -> Error error
  | `Needs_recomputation depends_on     ->
    let cached_result = compute_cached_result t ~depends_on in
    set t cached_result;
    cached_result.result
;;

module Fixed_incorrect_cache = struct
  type 'a t =
    { was_cached_as : 'a Or_error.t
    ; set_cache_to  : 'a Or_error.t
    }
  [@@deriving sexp_of]
end

let is_correct t ~was_cached_as ~set_cache_to ~ignore_diffs_in_errors =
  match was_cached_as, set_cache_to with
  | Ok _, Error _
  | Error _, Ok _      -> false
  | Error e1, Error e2 -> ignore_diffs_in_errors || Error.compare e1 e2 = 0
  | Ok t1, Ok t2       -> t.equal t1 t2
;;

let check t ~ignore_diffs_in_errors =
  match status t with
  | `Needs_recomputation _              -> Ok ()
  | `Error_computing_dependencies error -> Error error
  | `Up_to_date cached_result            ->
    let was_cached_as = cached_result.result in
    let cached_result = compute_cached_result t ~depends_on:cached_result.depends_on in
    let set_cache_to = cached_result.result in
    if is_correct t ~was_cached_as ~set_cache_to ~ignore_diffs_in_errors
    then Ok ()
    else (
      set t cached_result;
      let sexp_of_a = t.sexp_of_a in
      error_s
        [%sexp
          "fixed incorrect cache",
          { was_cached_as : a Or_error.t
          ; set_cache_to  : a Or_error.t
          }
        ])
;;

let force_set_for_test_exn t result =
  if not am_functional_testing
  then
    failwith "Cached.force_set_for_test_exn may be called during functional tests only";
  let depends_on = compute_depends_on t |> ok_exn in
  let tick = Virtual_time.tick () in
  set t
    { computation_initiated_at = tick
    ; computation_finished_at  = tick
    ; depends_on
    ; result
    }
;;
