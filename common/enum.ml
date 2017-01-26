open! Core
open Import

module type S = sig
  type t [@@deriving sexp_of]
  val all : t list
end

type 'a t = (module S with type t = 'a)

(* Map a constructor name to a command-line string: downcase the name and convert
   [_] to [-]. *)
let to_string_hum (type a) (m : a t) a =
  let module M = (val m) in
  String.lowercase (String.tr (Sexp.to_string (a |> [%sexp_of: M.t]))
                      ~target:'_' ~replacement:'-')
;;
