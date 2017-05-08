module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module Worker_obligations = Worker_obligations. Stable
  module Worker_rev_facts   = Worker_rev_facts.   Stable
  module Rev                = Rev.                Stable

  module Status = struct
    module V1 = struct
      type t =
        | Disabled
        | Write_only
        | Read_write
      [@@deriving bin_io, compare, enumerate, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 4baaff045fc76e6ef3816dd67fc460d0 |}]
      ;;
    end
    module Model = V1
  end

  module Properties = struct
    let default_max_items_per_rpc = 20
    module V2 = struct
      type t =
        { max_size          : int
        ; status            : Status.V1.t
        ; max_items_per_rpc : int
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 3508e4a8dcbe7553653c0affb8cb26ef |}]
      ;;

      let to_model t = t
      let of_model m = m
    end

    module V1 = struct
      type t =
        { max_size : int
        ; status   : Status.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 5104789f20ba557c010d89676ccff238 |}]
      ;;

      let to_model { max_size; status } =
        V2.to_model
          { V2.
            max_size
          ; status
          ; max_items_per_rpc = default_max_items_per_rpc
          }
      ;;
    end
    module Model = V2
  end

  module By_rev = struct
    module V1 = struct
      type 'a t = (Rev.V1.t * 'a) list
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: Bin_digest_type_variable.tick_a t];
        [%expect {| 4d213adf0473e27cdfd7fd94f0b4c0f9 |}]
      ;;
    end

    module Model = V1
  end

  module From_server_to_worker = struct
    module V6 = struct
      type t =
        { worker_obligations : Worker_obligations.V5.t By_rev.V1.t
        ; worker_rev_facts   : Worker_rev_facts.V1.t   By_rev.V1.t
        ; properties         : Properties.V2.t
        }
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 4175db6fe1edfc467d818763e089da74 |}]
      ;;

      let empty =
        { worker_obligations = []
        ; worker_rev_facts   = []
        ; properties =
            { max_size          = 1
            ; status            = Disabled
            ; max_items_per_rpc = 1
            }
        }
      ;;
    end
    module Model = V6
  end

  module From_worker_back_to_server = struct
    module V5 = struct
      type t =
        { worker_obligations : Worker_obligations.V5.t By_rev.V1.t
        ; worker_rev_facts   : Worker_rev_facts.V1.t   By_rev.V1.t
        }
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b814da09f0b95060f433772feccf8129 |}]
      ;;

      let empty =
        { worker_obligations = []
        ; worker_rev_facts   = []
        }
      ;;
    end
    module Model = V5
  end
end

open! Core
open! Import

module Status = struct
  module T = Stable.Status.Model
  include T
  let equal t1 t2 = compare t1 t2 = 0

  let doc_for_readme () =
    let switch t = Enum.to_string_hum (module T) t in
    let doc = function
      | Disabled   -> "\
  No cached values are sent to the worker.
  New values computed by the worker are ignored."
      | Write_only -> "\
  No cached values are sent to the worker.
  New values computed by the worker are added to the cache on the server."
      | Read_write -> "\
  Relevant cached values are sent to the worker.
  New values computed by the worker are added to the cache for later use."
    in
    List.map all ~f:(fun t -> concat [ switch t ; ":\n  " ; doc t ; "\n" ])
    |> concat ~sep:"\n"
  ;;
end

module Properties = struct
  type t = Stable.Properties.Model.t =
    { max_size          : int
    ; status            : Status.t
    ; max_items_per_rpc : int
    }
  [@@deriving sexp_of]

  include (Stable.Properties.Model : module type of
             Stable.Properties.Model with type t := t)

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      let non_negative t = assert (t >= 0) in
      Fields.iter
        ~max_size:(check non_negative)
        ~status:ignore
        ~max_items_per_rpc:(check non_negative))
  ;;

  let default =
    { max_size          = 100
    ; status            = Disabled
    ; max_items_per_rpc = Stable.Properties.default_max_items_per_rpc
    }
  ;;
end

module Concise_worker_obligations = struct
  let sexp_of_t t =
    t
    |> List.map ~f:fst
    |> [%sexp_of: Rev.t list]
  ;;
end

module Concise_worker_rev_facts = struct
  let sexp_of_one (rev, worker_rev_facts) =
    ignore rev; (* already printed 3 times in rev_facts *)
    Worker_rev_facts.Concise.sexp_of_t worker_rev_facts
  ;;

  let sexp_of_t list = list |> [%sexp_of: one list]
end

module From_server_to_worker = struct
  include Stable.From_server_to_worker.Model
  module Concise = struct
    type nonrec t = t

    let sexp_of_t { worker_obligations; worker_rev_facts; properties } =
      [%sexp
        { worker_obligations : Concise_worker_obligations.t
        ; worker_rev_facts   : Concise_worker_rev_facts.t
        ; properties         : Properties.t
        }
      ]
    ;;
  end
end

module From_worker_back_to_server = struct
  include Stable.From_worker_back_to_server.Model
  module Concise = struct
    type nonrec t = t

    let sexp_of_t { worker_obligations; worker_rev_facts } =
      [%sexp
        { worker_obligations : Concise_worker_obligations.t
        ; worker_rev_facts   : Concise_worker_rev_facts.t
        }
      ]
    ;;
  end
end

module Worker_session = struct

  open Async

  module Cached = struct
    type 'a t =
      { mutable to_server : (Rev.t * 'a) list
      ; by_rev            : 'a Rev.Compare_by_hash.Table.t
      }
    [@@deriving fields, sexp_of]

    let create by_rev_alist ~f =
      let by_rev =
        let table = Rev.Compare_by_hash.Table.create () in
        List.iter by_rev_alist ~f:(fun (rev, data) ->
          Hashtbl.add_exn table ~key:rev ~data:(f data));
        table
      in
      { to_server = []
      ; by_rev
      }
    ;;

    let to_server t ~f =
      List.fold ~init:Rev.Compare_by_hash.Map.empty t.to_server
        ~f:(fun map (key, data) ->
          (* Only retain the most recent in case of a race between multiple calls to
             [use_or_compute_and_store]. *)
          match Map.find map key with
          | Some _ -> map
          | None -> Map.add map ~key ~data:(f data))
      |> Map.to_alist
    ;;
  end

  type t =
    { worker_obligations : Worker_obligations.t Cached.t
    ; worker_rev_facts   : Worker_rev_facts.t   Cached.t
    ; properties         : Properties.t
    }
  [@@deriving fields, sexp_of]

  let create { From_server_to_worker. worker_obligations; worker_rev_facts; properties } =
    { worker_obligations = Cached.create worker_obligations ~f:Worker_obligations.of_stable
    ; worker_rev_facts   = Cached.create worker_rev_facts   ~f:Fn.id
    ; properties
    }
  ;;

  let back_to_server t =
    let to_server cached ~f =
      match t.properties.status with
      | Disabled -> []
      | Read_write | Write_only ->
        Cached.to_server cached ~f
        |> fun list -> List.take list t.properties.max_items_per_rpc
    in
    { From_worker_back_to_server.
      worker_obligations = to_server t.worker_obligations ~f:Worker_obligations.to_stable
    ; worker_rev_facts   = to_server t.worker_rev_facts   ~f:Fn.id
    }
  ;;

  module Key = struct
    type t_ = t
    type 'a t =
      | Worker_obligations : Worker_obligations.t  t
      | Worker_rev_facts   : Worker_rev_facts.t    t

    let get_cached : type a. t_ -> a t -> a Cached.t = fun t key ->
      match key with
      | Worker_obligations -> t.worker_obligations
      | Worker_rev_facts   -> t.worker_rev_facts
  end

  let use_or_compute_and_store t key rev compute =
    let t = Key.get_cached t key in
    match Hashtbl.find t.by_rev rev with
    | Some result -> return result
    | None ->
      compute rev >>| fun result ->
      t.to_server <- (rev, result) :: t.to_server;
      Hashtbl.set t.by_rev ~key:rev ~data:result;
      result
  ;;

  let find t rev key =
    Hashtbl.find (Key.get_cached t key).by_rev rev
  ;;

  let remove t rev key =
    Hashtbl.remove (Key.get_cached t key).by_rev rev
  ;;
end

module Persist = struct
  module Properties = struct
    include Persistent.Make
        (struct let version = 2 end)
        (Stable.Properties.V2)
    include Register_read_old_version
        (struct let version = 1 end)
        (Stable.Properties.V1)
  end
  let properties_file = Relpath.of_string "properties"
end

module By_rev = Lru_cache.Make (Rev.Compare_by_hash)

type t =
  { mutable properties : Properties.t
  ; worker_obligations : Worker_obligations.On_server.t By_rev.t
  ; worker_rev_facts   : Worker_rev_facts.On_server.t   By_rev.t
  ; serializer         : Serializer.t
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~properties:(check Properties.invariant)
      ~worker_obligations:(check (By_rev.invariant Worker_obligations.On_server.invariant))
      ~worker_rev_facts:(check (By_rev.invariant Worker_rev_facts.On_server.invariant))
      ~serializer:ignore;
    assert (t.properties.max_size = By_rev.max_size t.worker_obligations);
    assert (t.properties.max_size = By_rev.max_size t.worker_rev_facts);
  )
;;

let stats t =
  [%sexp
    { properties         = (t.properties : Properties.t)
    ; worker_obligations = { length = (By_rev.length t.worker_obligations : int) }
    ; worker_rev_facts   = { length = (By_rev.length t.worker_rev_facts   : int) }
    }
  ]
;;

let by_rev_stats by_rev =
  let sexp_of_key rev = rev |> Rev.to_string_40 |> [%sexp_of: string] in
  By_rev.stats ~sexp_of_key by_rev
;;

let stats_with_revs t =
  [%sexp
    { properties         = (t.properties                      : Properties.t)
    ; worker_obligations = (by_rev_stats t.worker_obligations : Sexp.t)
    ; worker_rev_facts   = (by_rev_stats t.worker_rev_facts   : Sexp.t)
    }
  ]
;;

let values_at_rev t ~rev =
  [%sexp
    { worker_obligations
      = (By_rev.find t.worker_obligations rev : Worker_obligations.On_server.t option)
    ; worker_rev_facts
      = (By_rev.find t.worker_rev_facts rev : Worker_rev_facts.On_server.t option)
    }
  ]
;;

let obligations_at_rev t ~rev =
  match By_rev.find t.worker_obligations rev with
  | Some cache -> Worker_obligations.On_server.dump_obligations cache
  | None -> raise_s [%sexp "revision not found in obligations cache", (rev : Rev.t)]
;;

module What_to_dump = struct
  type t =
    [ `Stats
    | `Revs
    | `Values_at_rev      of Rev.t
    | `Obligations_at_rev of Rev.t
    ]
  [@@deriving sexp_of]

  let require_admin_privileges = function
    | `Stats | `Revs -> false
    | `Values_at_rev _ | `Obligations_at_rev _ -> true
  ;;
end

let dump t = function
  | `Stats                  -> stats t
  | `Revs                   -> stats_with_revs t
  | `Values_at_rev rev      -> values_at_rev t ~rev
  | `Obligations_at_rev rev -> obligations_at_rev t ~rev
;;

let deserializer = Deserializer.with_serializer (fun serializer ->
  let open Deserializer.Let_syntax in
  let%map_open () = return ()
  and ({ Properties. max_size; _ } as properties) =
    one ~default:Properties.default (module Persist.Properties)
      ~in_file:Persist.properties_file
  in
  { properties
  ; worker_obligations = By_rev.create ~max_size
  ; worker_rev_facts   = By_rev.create ~max_size
  ; serializer
  }
)
;;

let persist_properties t properties =
  t.properties <- properties;
  Serializer.set_contents t.serializer t.properties (module Persist.Properties)
    ~file:Persist.properties_file
;;

let set_max_size t ~max_size =
  if max_size <> t.properties.max_size
  then (
    let set_max_size by_rev =
      ignore (By_rev.set_max_size by_rev ~max_size : [ `Dropped of int ])
    in
    set_max_size t.worker_obligations;
    set_max_size t.worker_rev_facts;
    persist_properties t { t.properties with max_size });
;;

let set_status t ~status =
  if not (Status.equal status t.properties.status)
  then persist_properties t { t.properties with status }
;;

let set_max_items_per_rpc t ~max_items_per_rpc =
  if max_items_per_rpc < 0
  then raise_s [%sexp "invalid max_items_per_rpc value", (max_items_per_rpc : int)];
  if t.properties.max_items_per_rpc <> max_items_per_rpc
  then persist_properties t { t.properties with max_items_per_rpc }
;;

module Feature_revs = struct
  type t =
    { diff4s_revs    : Rev.Compare_by_hash.Set.t
    ; base           : Rev.t
    ; tip            : Rev.t
    }
  [@@deriving sexp_of]
end

let clear t = function
  | `All ->
    let clear by_rev = ignore (By_rev.clear by_rev : [ `Dropped of int ]) in
    clear t.worker_obligations;
    clear t.worker_rev_facts;
  | `Feature_revs { Feature_revs. diff4s_revs; base; tip } ->
    let remove lru rev   = ignore (By_rev.remove lru rev : [ `Ok | `No_such_key ]) in
    let rev_facts_revs   = Rev.Compare_by_hash.Set.of_list [ base; tip ] in
    let obligations_revs = Set.union rev_facts_revs diff4s_revs in
    Set.iter obligations_revs ~f:(remove t.worker_obligations);
    Set.iter rev_facts_revs ~f:(remove t.worker_rev_facts);
  | `Revs revs ->
    let remove lru rev = ignore (By_rev.remove lru rev : [ `Ok | `No_such_key ]) in
    List.iter revs ~f:(fun rev ->
      remove t.worker_obligations rev;
      remove t.worker_rev_facts   rev;
    );
;;

let augment t { From_worker_back_to_server. worker_obligations; worker_rev_facts } =
  match t.properties.status with
  | Disabled -> ()
  | Write_only | Read_write ->
    let aux table alist ~f =
      List.iter alist ~f:(fun (key, data) -> By_rev.set table ~key ~data:(f data))
    in
    aux t.worker_obligations worker_obligations ~f:Worker_obligations.On_server.of_stable;
    aux t.worker_rev_facts   worker_rev_facts   ~f:Worker_rev_facts.On_server.of_stable;
;;

let send_to_worker t { Feature_revs. diff4s_revs; base; tip } =
  match t.properties.status with
  | Disabled | Write_only -> From_server_to_worker.empty
  | Read_write ->
    let to_worker cached set ~f =
      Set.fold set ~init:[] ~f:(fun acc rev ->
        match By_rev.find cached rev with
        | None -> acc
        | Some data -> (rev, f data) :: acc)
      |> fun list -> List.take list t.properties.max_items_per_rpc
    in
    let rev_facts_revs   = Rev.Compare_by_hash.Set.of_list [ base; tip ] in
    let obligations_revs = Set.union rev_facts_revs diff4s_revs in
    { From_server_to_worker.
      worker_obligations = to_worker t.worker_obligations obligations_revs
                             ~f:Worker_obligations.On_server.to_stable
    ; worker_rev_facts   = to_worker t.worker_rev_facts rev_facts_revs
                             ~f:Worker_rev_facts.On_server.to_stable
    ; properties         = t.properties
    }
;;
