open! Core
open! Import

module Elt = struct
  type 'a t =
    { check_code : int
    ; value      : 'a
    ; index      : int
    }
  [@@deriving fields]
end

type 'a t =
  { check_code      : int
  ; elements        : 'a array
  ; mutable pointer : int
  ; deleted         : bool array
  }

let check_code = ref 0

let create list =
  incr check_code;
  let elements = Array.of_list list in
  let deleted = Array.map elements ~f:(fun _ -> false) in
  let pointer = 0 in
  { check_code = !check_code
  ; elements
  ; pointer
  ; deleted
  }
;;

let pointed t =
  if Array.is_empty t.elements then None else
  if t.deleted.(t.pointer)
  then None
  else
    Some
      { Elt.value = t.elements.(t.pointer)
      ; index = t.pointer
      ; check_code = t.check_code }
;;

let move_pointer direction t =
  if not (Array.is_empty t.elements)
  then (
    let delta =
      match direction with
      | `forward  -> 1
      | `backward -> (-1)
    in
    let length = Array.length t.elements in
    let initial_index = t.pointer in
    let rec loop index =
      let index = Int.(%) (index + delta) length in
      if index = initial_index
      then ()
      else
      if t.deleted.(index)
      then loop index
      else (t.pointer <- index)
    in
    loop initial_index)
;;

let goto_next t = move_pointer `forward t
;;

let goto_previous t = move_pointer `backward t
;;

let current t =
  match pointed t with
  | Some _ as some -> some
  | None ->
    goto_next t;
    pointed t
;;

let _delete_current t =
  if not (Array.is_empty t.elements)
  then (
    t.deleted.(t.pointer) <- true;
    goto_next t)
;;

let values t =
  Array.to_list (Array.filteri t.elements ~f:(fun i _ -> not t.deleted.(i)))
;;

let is_empty t = Array.for_all ~f:Fn.id t.deleted
;;

let mem t elt =
  Int.(=) t.check_code (Elt.check_code elt)

let delete t elt =
  (if not (mem t elt)
   then failwith "Review_ring.delete. Not a element of that ring");
  let len = Array.length t.elements in
  let index = Elt.index elt in
  if index >= 0 && index < len
  then (
    t.deleted.(index) <- true;
    if t.pointer = index
    then goto_next t)
;;

let to_list t =
  Array.mapi t.elements ~f:(fun i elt -> t.deleted.(i), i, elt)
  |> Array.to_list
  |> List.filter_map ~f:(fun (deleted, index, value) ->
    if deleted then None else Some { Elt.index; value ; check_code = t.check_code }
  )
;;

let length t = Array.count t.deleted ~f:(fun deleted -> not deleted)
;;

let goto t elt =
  if not (mem t elt)
  then Or_error.error_string "Elt not a member of review ring"
  else if t.deleted.(elt.index)
  then Or_error.error_string "Elt was deleted"
  else (
    t.pointer <- elt.index;
    Ok ())
;;
