open! Core
open! Import

type t =
  { num_retries     : int
  ; force_next_time : bool
  }
[@@deriving compare, fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~num_retries:(check (fun n -> [%test_pred: int] (fun n -> n >= 0) n))
      ~force_next_time:ignore
  )
;;

let empty =
  { num_retries     = 0
  ; force_next_time = false
  }
;;

let force_next_time t =
  { t with force_next_time = true }
;;

let inc_num_retries t =
  { t with num_retries = 1 + num_retries t }
;;

let should_force t =
  if t.force_next_time
  then Some (inc_num_retries { t with force_next_time = false })
  else None
;;

let can_retry t =
  if num_retries t < 2
  then Some (inc_num_retries t)
  else None
;;
