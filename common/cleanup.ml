open! Core
open! Async
open! Import

type t =
  { start                          : unit Ivar.t
  ; finished                       : unit Deferred.t
  ; mutable to_run_at_shutdown_elt : t Bag.Elt.t option
  }
[@@deriving sexp_of]

let to_run_at_shutdown = Bag.create ()

let run t =
  (match t.to_run_at_shutdown_elt with
   | None -> ()
   | Some elt ->
     t.to_run_at_shutdown_elt <- None;
     Bag.remove to_run_at_shutdown elt;
     Ivar.fill t.start ());
  t.finished
;;

let () =
  Shutdown.at_shutdown (fun () ->
    Deferred.List.iter ~how:`Parallel (Bag.to_list to_run_at_shutdown) ~f:run)
;;

let create f =
  let start = Ivar.create () in
  let finished = let%bind () = Ivar.read start in f () in
  let t =
    { start
    ; finished
    ; to_run_at_shutdown_elt = None
    }
  in
  t.to_run_at_shutdown_elt <- Some (Bag.add to_run_at_shutdown t);
  Shutdown.don't_finish_before t.finished;
  t
;;
