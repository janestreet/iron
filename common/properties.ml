open Core.Std
open Import

type t = Sexp.t String.Table.t [@@deriving sexp_of]

let invariant t = Hashtbl.invariant ignore ignore t

let to_rows t =
  List.sort (Hashtbl.to_alist t)
    ~cmp:(fun (key1, _) (key2, _) -> String.compare key1 key2)
  |> List.map ~f:(fun (key, value) -> (key, Sexp.to_string value))
;;

let create () = String.Table.create ()
;;

module Stable = struct
  module V1 = struct
    include
      Wrap_stable.F
        (struct
          type t = (string * Sexp.t) list
          [@@deriving bin_io, compare, sexp]
        end)
        (struct
          type nonrec t = t

          let of_stable : _ -> t = String.Table.of_alist_exn

          let to_stable = Hashtbl.to_alist
        end)
    ;;
  end
end
