open Core.Std;;
open Int;;

(* Why isn't there a polymorphic list hash function already? *)
(* Why isn't there a rotate function for ints? *)

let rotate_left x i =
  let  i = i % num_bits                         in        (* The % will get reduced *)
  let hi = shift_left          x              i in        (* away when this fun is  *)
  let lo = shift_right_logical x (num_bits - i) in        (* inlined below.         *)
  bit_or hi lo
;;

(* Hash *all* the items, then xor them together, rotating 1 bit after each xor. *)
let hash itemhash xs =
  let rec lp acc xs =
    match xs with
    |[]    -> acc
    |x::xs -> lp (bit_xor (rotate_left acc 1)
                    (itemhash x))
                xs
  in lp 0 xs
;;
