open Core.Std

type 'a sexpable = (module Sexpable with type t = 'a)

type _ t =
  | MapN            : ('load, 'save, 'r) map_args * 'load * 'save -> 'r t
  | Field           : string * 'a sexpable * 'a -> 'a t
  | Field_opt       : string * 'a sexpable -> 'a option t
and (_, _, _) map_args =
  | Nil : ('a, 'a -> unit, 'a) map_args
  | And : 'a t * ('load, 'save, 'r) map_args
    -> ('a -> 'load, ('a -> unit) -> 'save, 'r) map_args

let nil = Nil
let ( & ) part map_args = And (part, map_args)
let mapN map_args ~load ~save = MapN (map_args, load, save)
let field name sexpable ~default = Field (name, sexpable, default)
let field_opt name sexpable = Field_opt (name, sexpable)

let of_sexp t sexp =
  let map = [%of_sexp: Sexp.t String.Map.t] sexp in
  let field (type a) name (sexpable : a sexpable) ~default =
    let module M = (val sexpable) in
    match Map.find map name with
    | None      -> default
    | Some sexp -> Option.value (Option.try_with (fun () -> M.t_of_sexp sexp)) ~default
  in
  let field_opt (type a) name (sexpable : a sexpable) =
    let module M = (val sexpable) in
    match Map.find map name with
    | None      -> None
    | Some sexp ->
      Option.value (Option.try_with (fun () -> Some (M.t_of_sexp sexp))) ~default:None
  in
  let rec load : type a . (a t -> a) = function
    | MapN (map_args, f, _save) ->
      let rec load_map_args : type load save r . (load, save, r) map_args -> load -> r
        = fun map_args f ->
          match map_args with
          | Nil -> f
          | And (t, map_args) ->
            load_map_args map_args (f (load t))
      in
      load_map_args map_args f
    | Field (name, sexpable, default) -> field name sexpable ~default
    | Field_opt (name, sexpable) -> field_opt name sexpable
  in
  load t
;;

let to_sexp t value =
  let table = String.Table.create () in
  let field (type a) name (sexpable : a sexpable) f =
    let module M = (val sexpable) in
    f (fun field -> Hashtbl.set table ~key:name ~data:(M.sexp_of_t field))
  in
  let field_opt (type a) name (sexpable : a sexpable) f =
    let module M = (val sexpable) in
    f (fun field ->
      Option.iter field
        ~f:(fun field -> Hashtbl.set table ~key:name ~data:(M.sexp_of_t field)))
  in
  let rec save : type a b. (a t -> ((a -> unit) -> b) -> b) = fun t f ->
    match t with
    | MapN (map_args, _load, fN) ->
      let rec save_map_args
        : type load save r . (load, save, r) map_args -> save -> (r -> unit)
        = fun map_args f ->
          match map_args with
          | Nil -> f
          | And (t, map_args) ->
            save_map_args map_args (save t f)
      in
      f (save_map_args map_args fN)
    | Field (name, sexpable, _default) -> field name sexpable f
    | Field_opt (name, sexpable) -> field_opt name sexpable f
  in
  save t (fun f -> f value);
  let map = String.Map.of_alist_exn (Hashtbl.to_alist table) in
  [%sexp_of: Sexp.t String.Map.t] map
;;
