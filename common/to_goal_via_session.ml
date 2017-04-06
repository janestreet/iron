open! Core
open! Import

type 'a t =
  | Fully_known of 'a
  | Partially_known of
      { to_finish_session        : 'a
      ; from_session_end_to_goal : Error.t Or_pending.t
      }
[@@deriving compare, sexp_of]

let invariant invariant t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    match t with
    | Fully_known a -> invariant a
    | Partially_known { to_finish_session = a; from_session_end_to_goal = _ } ->
      invariant a)
;;

let map t ~f =
  match t with
  | Fully_known a -> Fully_known (f a)
  | Partially_known { to_finish_session
                    ; from_session_end_to_goal
                    }
    -> Partially_known { to_finish_session = f to_finish_session
                       ; from_session_end_to_goal
                       }
;;

let fully_known_exn = function
  | Fully_known a -> a
  | Partially_known { from_session_end_to_goal; _ } ->
    let err =
      try Or_pending.known_exn from_session_end_to_goal with e -> Error.of_exn e
    in
    raise_s [%sexp "line count is not knowable", (err : Error.t)]
;;

let maybe_partially_known = function
  | Fully_known x -> x
  | Partially_known { to_finish_session = x; from_session_end_to_goal = _ } -> x
;;
