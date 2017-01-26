module Stable = struct
  open Core.Core_stable

  module Time = Iron_time.Stable

  module V1 = struct

    type 'a t =
      | Pending_since of Time.V1_round_trippable.t
      | Known of 'a
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: Bin_digest_type_variable.tick_a t];
      [%expect {| a16ee75d08e382077df34a36895f0718 |}]
    ;;

    let map t ~f =
      match t with
      | Pending_since _ as t -> t
      | Known a -> Known (f a)

  end

  module Or_error = struct
    module V1 = struct
      type 'a t = 'a Or_error.V1.t V1.t
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: Bin_digest_type_variable.tick_a t];
        [%expect {| 7f36a8d4a889af075ce3c9f6a3e43a35 |}]
      ;;

      let map t ~f = V1.map t ~f:(fun t -> Or_error.V1.map t ~f)
    end
  end
end

open! Core
open! Import

include Stable.V1

let is_known = function
  | Pending_since _ -> false
  | Known         _ -> true
;;

let is_pending = function
  | Pending_since _ -> true
  | Known         _ -> false
;;

let pending_for_sexp since = [%sexp "pending for", (how_long ~since : Time.Span.t)]

let known_exn t =
  match t with
  | Known a -> a
  | Pending_since since -> raise_s (pending_for_sexp since)
;;

let pending_error t =
  match t with
  | Known or_error -> or_error
  | Pending_since since -> error_s (pending_for_sexp since)
;;

let or_pending_error = function
  | Known (Ok ok)       -> Ok ok
  | Known (Error err)   -> Error (Known err)
  | Pending_since since -> Error (Pending_since since)
;;

include Monad.Make (struct

    type nonrec 'a t = 'a t

    let return x = Known x

    let map = `Custom map

    let bind t ~f =
      match t with
      | Pending_since time -> Pending_since time
      | Known x -> f x
    ;;
  end)

include Container.Make (struct

    type nonrec 'a t = 'a t

    let fold t ~init ~f =
      match t with
      | Pending_since _ -> init
      | Known x -> f init x
    ;;

    let iter = `Custom (fun t ~f ->
      match t with
      | Pending_since _ -> ()
      | Known x -> f x)
    ;;
  end)

let invariant invariant_a t =
  match t with
  | Pending_since _ -> ()
  | Known a -> invariant_a a
;;

let pending_now () = Pending_since (Time.now ())

module Or_error = struct
  include Stable.Or_error.V1

  let map t ~f =
    match t with
    | Pending_since _
    | Known (Error _) as z -> z
    | Known (Ok a) -> Known (Ok (f a))
  ;;

  include Monad.Make (struct

      type nonrec 'a t = 'a t

      let return x = Known (Ok x)

      let map = `Custom map

      let bind t ~f =
        match t with
        | Pending_since time -> Pending_since time
        | Known ((Error _) as error) -> Known error
        | Known (Ok x) -> f x
      ;;
    end)

  include Container.Make (struct

      type nonrec 'a t = 'a t

      let fold t ~init ~f =
        match t with
        | Pending_since _ -> init
        | Known (Error _) -> init
        | Known (Ok x) -> f init x
      ;;

      let iter = `Custom (fun t ~f ->
        match t with
        | Pending_since _ -> ()
        | Known (Error _) -> ()
        | Known (Ok x) -> f x)
      ;;
    end)

  let invariant invariant_a t = iter ~f:invariant_a t
  ;;

end
