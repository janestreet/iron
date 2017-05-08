module T = struct
  open! Core
  open! Import

  type t =
    | U1
    | U2
    | U3
  [@@deriving enumerate]

  let to_int = function
    | U1 -> 1
    | U2 -> 2
    | U3 -> 3
  ;;

  let hash t = Int.hash (to_int t)

  let of_int = function
    | 1 -> U1
    | 2 -> U2
    | 3 -> U3
    | n -> raise_s [%sexp "Dynamic_upgrade: version is out of range", (n : int)]
  ;;

  let sexp_of_t t = sexp_of_int (to_int t)

  let compare t1 t2 = Int.compare (to_int t1) (to_int t2)
end

module Stable = struct
  module V1 = struct
    open! Core.Core_stable
    let hash = T.hash
    include Make_stable.Of_stable_format.V1 (struct
        type t = int [@@deriving bin_io, compare, sexp]
      end) (struct
        type t = T.t [@@deriving compare]
        let to_stable_format = T.to_int
        let of_stable_format = T.of_int
      end)
    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 698cfa4093fe5e51523842d37b92aeac |}]
    ;;
  end
  module State = struct
    module V1 = struct
      type t =
        { committed_to  : V1.t
        ; current_value : V1.t
        }
      [@@deriving compare, fields, sexp]
    end
    module Model = V1
  end
end

open! Core
open! Import

include T
include Comparable.Make_plain (T)

let%test_unit _ =
  List.iter all ~f:(fun t ->
    [%test_result: t]
      (of_int (to_int t))
      ~expect:t)
;;

module State = struct

  module Persist = struct
    module State = struct
      include Persistent.Make
          (struct let version = 1 end)
          (Stable.State.V1)
    end
    let state_file = Relpath.of_string "state"
  end

  type t =
    { mutable committed_to  : T.t
    ; mutable current_value : T.t
    ; serializer            : Serializer.t
    }
  [@@deriving sexp_of]

  let invariant ({ current_value; committed_to; serializer = _ } as t) =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      assert (current_value >= committed_to))
  ;;

  module Reference = struct
    type nonrec t = t
    let invariant : t -> unit = ignore
  end

  let deserializer = Deserializer.with_serializer (fun serializer ->
    let open Deserializer.Let_syntax in
    let%map_open values =
      one_opt (module Persist.State) ~in_file:Persist.state_file
    in
    match values with
    | Some { committed_to; current_value } ->
      { committed_to
      ; current_value
      ; serializer
      }
    | None ->
      { committed_to  = U1
      ; current_value = U1
      ; serializer
      })
  ;;

  let persist_internal { committed_to; current_value; serializer } =
    Serializer.set_contents serializer { Stable.State.Model.committed_to; current_value }
      (module Persist.State) ~file:Persist.state_file
  ;;

  let dump { committed_to; current_value; serializer = _ } =
    [%sexp
      { committed_to  : T.t
      ; current_value : T.t
      }
    ]
end

let set_exn (t : State.t) trying_to_set_as =
  if trying_to_set_as >= t.committed_to
  then (
    if t.current_value <> trying_to_set_as
    then (
      t.current_value <- trying_to_set_as;
      State.persist_internal t))
  else
    raise_s
      [%sexp "Cannot lower the upgrade state to that value anymore, associated \
              functionality was already used"
           , (State.dump t : Sexp.t)
           , { trying_to_set_as : T.t }
      ]
;;

let commit_to_upgrade (t : State.t) ~allowed_from =
  if allowed_from > t.current_value
  then `Not_allowed_yet
  else (
    (if allowed_from > t.committed_to
     then (
       t.committed_to <- allowed_from;
       State.persist_internal t));
    `Ok)
;;
