open! Core
open! Import

module Type_id = Type_equal.Id

module Set_stats = struct

  type t =
    { table_length            : int
    ; number_of_entries       : int
    ; sum_of_bucket_lengths   : int
    ; smallest_bucket_length  : int
    ; median_bucket_length    : int
    ; biggest_bucket_length   : int
    }
  [@@deriving fields, sexp_of]

  (* From Ocaml manual: Weak.S

     val stats : t -> int * int * int * int * int * int
     Return statistics on the table. The
     numbers are, in order: table length,
     number of entries, sum of bucket lengths,
     smallest bucket length, median bucket
     length, biggest bucket length.
  *)
  let create
        (table_length
        , number_of_entries
        , sum_of_bucket_lengths
        , smallest_bucket_length
        , median_bucket_length
        , biggest_bucket_length)
    =
    { table_length
    ; number_of_entries
    ; sum_of_bucket_lengths
    ; smallest_bucket_length
    ; median_bucket_length
    ; biggest_bucket_length
    }
end

module Id = Unique_id.Int()

module Module_name = struct
  type t = string [@@deriving compare, sexp_of]
  let equal = String.equal

  let module_name_without_libname module_name =
    if not (String.is_prefix module_name ~prefix:"Iron_")
    then module_name
    else (
      match String.lsplit2 module_name ~on:'.' with
      | Some (_libname, module_name) -> module_name
      | None -> module_name)
  ;;
end

type weak_set =
  { stats       : unit -> Set_stats.t
  ; elements    : unit -> Sexp.t list
  ; hash_data   : unit -> Sexp.t list
  ; module_name : Module_name.t
  }

type t = weak_set Id.Table.t

let the_one_and_only = Memo.unit (fun () -> Id.Table.create ())

let register weak_set =
  Hashtbl.set (the_one_and_only ()) ~key:(Id.create ()) ~data:weak_set
;;

let weak_sets_sorted_by_module_name t =
  Hashtbl.data t
  |> List.sort ~cmp:(fun set1 set2 ->
    Module_name.compare set1.module_name set2.module_name)
;;

let detailed_stats t =
  weak_sets_sorted_by_module_name t
  |> List.map ~f:(fun set -> set.module_name, set.stats ())
;;

let stats t =
  detailed_stats t
  |> [%sexp_of: (Module_name.t * Set_stats.t) list]
;;

let sexp_of_t t =
  weak_sets_sorted_by_module_name t
  |> List.map ~f:(fun set -> set.module_name, set.elements ())
  |> [%sexp_of: (Module_name.t * Sexp.t list) list]
;;

let with_values t ~module_name ~f =
  Hashtbl.data t
  |> List.filter ~f:(fun set -> Module_name.equal module_name set.module_name)
  |> (function
    | [] -> raise_s [%sexp "no such module", (module_name : Module_name.t)]
    | x -> x)
  |> List.concat_map ~f
  |> [%sexp_of: Sexp.t list]
;;

let module_values = with_values ~f:(fun set -> set.elements ())
;;

let module_hash_data = with_values ~f:(fun set -> set.hash_data ())
;;

module What_to_dump = struct
  type t =
    [ `Stats
    | `Values
    | `Module_values of Module_name.t
    | `Module_hash_data of Module_name.t
    ]
  [@@deriving sexp_of]

  let require_admin_privileges = function
    | `Stats              -> false
    | `Values             -> true
    | `Module_values _    -> false
    | `Module_hash_data _ -> false
  ;;
end

let dump t = function
  | `Stats  -> stats t
  | `Values -> sexp_of_t t
  | `Module_values    module_name -> module_values    t ~module_name
  | `Module_hash_data module_name -> module_hash_data t ~module_name
;;

module type Unshared = sig
  type t [@@deriving compare, sexp_of]
  val hash : t -> int
  val module_name : Module_name.t
end

module type S = sig
  type t [@@deriving compare, sexp_of]
  val shared_t   : t -> t
  val unshared_t : t -> t
end

module Make (X : Unshared) () = struct

  type t = X.t [@@deriving compare, sexp_of]

  module Weak_hashset = Caml.Weak.Make (struct
      include X
      let equal t1 t2 = compare t1 t2 = 0
    end)

  let stats set = Set_stats.create (Weak_hashset.stats set)

  module Hash_data = struct
    type t =
      { value           : X.t
      ; hash            : int
      ; hash_mod_length : int
      }
    [@@deriving fields, sexp_of]

    let create ~table_length value =
      let hash = X.hash value in
      { value
      ; hash
      ; hash_mod_length = hash mod table_length
      }
    ;;

    let sort_by_hash =
      let f compare field x y = compare (Field.get field x) (Field.get field y) in
      List.sort ~cmp:(Comparable.lexicographic
                        [ f Int.compare Fields.hash_mod_length
                        ; f Int.compare Fields.hash
                        ; f X.compare   Fields.value
                        ])
    ;;
  end

  let elements set =
    list_of_iter (fun ~f -> Weak_hashset.iter f set)
    |> List.sort ~cmp:X.compare
    |> List.map ~f:X.sexp_of_t
  ;;

  let hash_data set =
    let table_length = Set_stats.table_length (stats set) in
    list_of_iter (fun ~f -> Weak_hashset.iter f set)
    |> List.map ~f:(Hash_data.create ~table_length)
    |> Hash_data.sort_by_hash
    |> List.map ~f:Hash_data.sexp_of_t
  ;;

  let set = lazy (
    let set = Weak_hashset.create 128 in
    register
      { stats       = (fun () -> stats set)
      ; elements    = (fun () -> elements set)
      ; hash_data   = (fun () -> hash_data set)
      ; module_name = Module_name.module_name_without_libname X.module_name
      };
    set
  )
  ;;

  let shared_t data =
    let tag = Obj.tag (Obj.repr data) in
    if tag = Obj.lazy_tag
    || tag = Obj.forward_tag
    || tag = Obj.int_tag
    || tag = Obj.out_of_heap_tag
    then data
    else Weak_hashset.merge (force set) data
  ;;

  let unshared_t t = t
end

module Make_binable (X : S) (T : Binable.S with type t = X.t) = struct
  include Binable.Stable.Of_binable.V1 (T) (struct
      type t = X.t
      let to_binable t = t
      let of_binable = X.shared_t
    end)
end

module Make_sexpable (X : S) (T : Sexpable.S with type t = X.t) = struct
  include Sexpable.Stable.Of_sexpable.V1 (T) (struct
      type t = X.t
      let to_sexpable t = t
      let of_sexpable = X.shared_t
    end)
end

module Make_stringable (X : S) (T : Stringable.S with type t = X.t) = struct
  let to_string = T.to_string
  let of_string str = X.shared_t (T.of_string str)
end

module Stable = struct
  module type Shared = sig
    type x
    type t

    val shared_t   : x -> t
    val unshared_t : t -> x

    include Unshared   with type t := t
    include Equal.S    with type t := t
    include Binable.S  with type t := t
    include Sexpable.S with type t := t

    module S : S with type t = t
  end
  module Make_stable_public (X : sig
      include Unshared
      include Binable.S  with type t := t
      include Sexpable.S with type t := t
    end) () = struct
    type t = X.t
    include (X : Unshared with type t := t)
    module S = Make (X) ()
    let shared_t = S.shared_t
    let unshared_t = S.unshared_t
    let equal t1 t2 = compare t1 t2 = 0
    include Make_binable  (S) (X)
    include Make_sexpable (S) (X)
  end
  module Make_stable_private = Make_stable_public
end

let alpha = 65599
let fold_hash hash_accu hash = hash_accu * alpha + hash
let init = 0
let field t hash_a acc field = fold_hash acc (hash_a (Field.get field t))

let container_hash ~length ~fold ~fold_hash container =
  let length = length container in
  let d = length / 8 in
  let filter i = d <= 1 || i mod d = 0 in
  let index = ref (-1) in
  fold container ~init:(Int.hash length)
    ~f:(fun acc elt -> incr index; if filter !index then fold_hash acc elt else acc)
;;

let list_hash hash_a list =
  container_hash
    ~length:List.length
    ~fold:List.fold
    ~fold_hash:(fun acc elt -> fold_hash acc (hash_a elt))
    list
;;

let set_hash hash_a set =
  container_hash
    ~length:Set.length
    ~fold:Set.fold
    ~fold_hash:(fun acc elt -> fold_hash acc (hash_a elt))
    set
;;

let map_hash hash_key hash_data map =
  container_hash
    ~length:Map.length
    ~fold:(fun map ~init ~f ->
      Map.fold map ~init ~f:(fun ~key ~data acc -> f acc (key, data)))
    ~fold_hash:(fun acc (key, data) ->
      fold_hash (fold_hash acc (hash_key key)) (hash_data data))
    map
;;
