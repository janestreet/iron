open Core
open Import

module Locked : sig
  type t = private
    { by           : User_name.t
    ; reason       : string
    ; is_permanent : bool
    ; query        : unit Query.t
    }
  [@@deriving fields, sexp_of]

  include Invariant.S with type t := t

  val create
    :  by           : User_name.t
    -> reason       : string
    -> is_permanent : bool
    -> query        : _ Query.t
    -> t

  val is_locked_by : t -> User_name.t -> bool
  val to_protocol  : t -> Iron_protocol.Feature.Locked.t

  module Compare_by_user : sig
    type nonrec t = t
    [@@deriving compare]
  end

end = struct
  type t =
    { by           : User_name.t
    ; reason       : string
    ; is_permanent : bool
    ; query        : unit Query.t
    }
  [@@deriving fields, sexp_of]

  module Compare_by_user = struct
    type nonrec t = t

    let compare t t' = User_name.compare t.by t'.by
  end

  let create ~by ~reason ~is_permanent ~query =
    { query = Query.with_action query ()
    ; by
    ; reason
    ; is_permanent
    }
  ;;

  let is_locked_by t user = User_name.equal t.by user

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~query:(check (Query.invariant Fn.id))
        ~by:(check User_name.invariant)
        ~reason:ignore
        ~is_permanent:ignore)
  ;;

  let to_protocol { by; reason; query; is_permanent } : Iron_protocol.Feature.Locked.t =
    { by
    ; at     = Query.at query
    ; reason
    ; is_permanent
    }
  ;;
end

type t =
  { mutable feature_path : Feature_path.t
  ; lock_by_name         : Locked.t list Lock_name.Table.t
  }
[@@deriving fields, sexp_of]

let create feature_path =
  { feature_path
  ; lock_by_name = Lock_name.Table.create ()
  }
;;

let set_feature_path t feature_path =
  t.feature_path <- feature_path;
;;

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~feature_path:(check Feature_path.invariant)
      ~lock_by_name:
        (check (Hashtbl.iteri ~f:(fun ~key:lock_name ~data:locks ->
           if List.is_empty locks
           then raise_s [%sexp "empty locks list", (lock_name : Lock_name.t)];
           List.iter locks ~f:Locked.invariant)))
  )
;;

let what_is_locked t =
  Hashtbl.to_alist t.lock_by_name
  |> List.sort ~cmp:[%compare: Lock_name.t * Locked.Compare_by_user.t list]
  |> List.map ~f:(fun (lock_name, locks) ->
    lock_name, (List.map ~f:Locked.to_protocol locks))
;;

let find t lock_name = Hashtbl.find t.lock_by_name lock_name |> Option.value ~default:[]

let check_all_unlocked t =
  match Hashtbl.to_alist t.lock_by_name with
  | [] -> Ok ()
  | (_::_) as locked ->
    error_s
      [%sexp
        "feature is locked",
        { feature_path = (t.feature_path : Feature_path.t)
        ; locked : (Lock_name.t * Locked.t list) list
        }
      ]
;;

let check_unlocked t lock_name =
  match Hashtbl.find t.lock_by_name lock_name with
  | None -> Ok ()
  | Some locks ->
    error_s
      [%sexp
        "feature lock is locked",
        { feature_path = (t.feature_path : Feature_path.t)
        ; lock_name    : Lock_name.t
        ; locks        : Locked.t list
        }
      ]
;;

let is_permanently_locked t lock_name =
  match Hashtbl.find t.lock_by_name lock_name with
  | None -> false
  | Some locks -> List.exists locks ~f:Locked.is_permanent
;;

let set_locks t (lock_name : Lock_name.t) locks =
  match locks with
  | []     -> Hashtbl.remove t.lock_by_name lock_name
  | _ :: _ -> Hashtbl.set t.lock_by_name ~key:lock_name ~data:locks
;;

let lock t ~query ~for_ ~lock_name ~reason ~is_permanent =
  let locked = Locked.create ~by:for_ ~reason ~is_permanent ~query in
  let locks = Option.value (Hashtbl.find t.lock_by_name lock_name) ~default:[] in
  let locks =
    locked
    :: List.filter locks ~f:(fun locked -> not (Locked.is_locked_by locked for_))
  in
  set_locks t lock_name locks;
;;

let unlock t ~for_ ~lock_name ~even_if_permanent =
  match Hashtbl.find t.lock_by_name lock_name with
  | None -> Or_error.errorf "not locked for %s" (Lock_name.to_string_hum lock_name)
  | Some locks ->
    With_return.with_return (fun return ->
      let this_user_had_lock = ref false in
      let locks =
        List.filter locks ~f:(fun locked ->
          let locked_by_this_user = Locked.is_locked_by locked for_ in
          (if locked_by_this_user
           then (
             this_user_had_lock := true;
             if Locked.is_permanent locked
             && not even_if_permanent
             then
               Or_error.error
                 (sprintf "This lock is permanent -- consider using %s"
                    Switch.even_if_permanent)
                 locked [%sexp_of: Locked.t]
               |> return.return));
          not locked_by_this_user)
      in
      if !this_user_had_lock
      then (set_locks t lock_name locks; Ok ())
      else (
        let message =
          sprintf !"not locked for %s by %{User_name}"
            (Lock_name.to_string_hum lock_name)
            for_
        in
        error_s
          [%sexp
            (message : string),
            { locks : Locked.t list
            }
          ]
      ))
;;
