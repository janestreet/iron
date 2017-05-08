open Core
open Async
open! Import

let verbose = Verbose.serializer

let create_todo () = Throttle.create ~continue_on_error:true ~max_concurrent_jobs:1

module Pauser : sig
  type t [@@deriving sexp_of]

  include Invariant.S with type t := t

  val create : unit -> t

  (** [pause_exn t] returns a deferred that becomes determined when [resume_exn t] is
      later called.  [pause_exn t] fails if [t] is paused. *)
  val pause_exn : t -> query:unit Query.t -> with_timeout:Time.Span.t -> unit Deferred.t

  (** [resume_exn t] fails if [t] is not paused. *)
  val resume_exn : t -> unit

  val status : t -> Sexp.t
end = struct
  module Paused = struct
    type t =
      { resume : unit Ivar.t
      ; by     : unit Query.t
      }
    [@@deriving fields, sexp_of]

    let invariant t =
      Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
        let check f = Invariant.check_field t f in
        Fields.iter
          ~resume:(check (fun resume -> assert (Ivar.is_empty resume)))
          ~by:ignore)
    ;;
  end

  module State = struct
    type t =
      | Not_paused
      | Timed_out
      | Paused of Paused.t
    [@@deriving sexp_of]
  end

  type t =
    { mutable state : State.t
    }
  [@@deriving fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~state:(check (function
          | State.Not_paused | Timed_out -> ()
          | Paused paused -> Paused.invariant paused)))
  ;;

  let create () = { state = Not_paused }

  let pause_exn t ~query:by ~with_timeout =
    match t.state with
    | Paused { by; _ } ->
      raise_s [%sexp "serializer is already paused"
                   , By_previous_query (by : unit Query.t)]
    | Not_paused | Timed_out ->
      Deferred.create (fun resume ->
        let paused = { Paused.resume; by } in
        t.state <- Paused paused;
        let timeout_event = Clock.Event.after with_timeout in
        don't_wait_for
          (match%map Clock.Event.fired timeout_event with
           | Aborted  () -> ()
           | Happened () ->
             match t.state with
             | Not_paused
             | Timed_out -> ()
             | Paused ({ resume; _ } as paused') ->
               if phys_equal paused paused'
               then (
                 t.state <- Timed_out;
                 Ivar.fill resume ()));
        don't_wait_for
          (let%map () = Ivar.read resume in
           match Clock.Event.abort timeout_event () with
           | Ok | Previously_aborted () | Previously_happened () -> ()))
  ;;

  let resume_exn t =
    match t.state with
    | Not_paused -> failwith "serializer is not paused"
    | Timed_out  -> failwith "serializer is not paused, pause timed out"
    | Paused { resume; _ } -> t.state <- Not_paused; Ivar.fill resume ();
  ;;

  let status t =
    match t.state with
    | Not_paused -> "serializer is not paused" |> [%sexp_of: string]
    | Timed_out  -> "serializer is not paused. it timed out" |> [%sexp_of: string]
    | Paused { by; _ } ->
      Info.create "serializer is paused by" by [%sexp_of: unit Query.t]
      |> [%sexp_of: Info.t]
  ;;
end

module Shared = struct
  type t =
    { should_do_effects : bool
    ; root_directory    : Abspath.t
    ; todo              : unit Throttle.t
    ; pauser            : Pauser.t
    }
  [@@deriving fields, sexp_of]

  let do_nothing =
    { should_do_effects = false
    ; root_directory    = Abspath.root
    ; todo              = create_todo ()
    ; pauser            = Pauser.create ()
    }
  ;;

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~should_do_effects:ignore
        ~root_directory:ignore
        ~todo:(check (Throttle.invariant ignore))
        ~pauser:(check Pauser.invariant))
end

type t =
  { shared                     : Shared.t sexp_opaque
  ; relative_to_root           : Relpath.t
  ; mutable cache_invalidators : Cached.Invalidator.t list
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~shared:(check Shared.invariant)
      ~relative_to_root:(check Relpath.invariant)
      ~cache_invalidators:(check (List.iter ~f:Cached.Invalidator.invariant)))
;;

let do_nothing =
  { shared             = Shared.do_nothing
  ; relative_to_root   = Relpath.empty
  ; cache_invalidators = []
  }
;;

let root_directory t = t.shared.root_directory

let dir t = Abspath.append t.shared.root_directory t.relative_to_root

let path_to_string t path =
  Abspath.to_string (Abspath.append (dir t) path)
;;

let add_cache_invalidator t invalidator =
  if not (List.mem ~equal:Cached.Invalidator.equal t.cache_invalidators invalidator)
  then (t.cache_invalidators <- invalidator :: t.cache_invalidators)
;;

let prior_changes_synced_to_file_system t = Throttle.prior_jobs_done t.shared.todo

let pause_exn t ~query ~with_timeout =
  if not t.shared.should_do_effects
  then failwith "serializer is not pausable during initialization";
  match Shutdown.shutting_down () with
  | `Yes _ -> failwith "serializer cannot be paused during shutdown"
  | `No ->
    let resume = Pauser.pause_exn t.shared.pauser ~query ~with_timeout in
    Deferred.create (fun paused ->
      don't_wait_for (Throttle.enqueue t.shared.todo (fun () ->
        Ivar.fill paused ();
        resume)));
;;

let resume_exn t = Pauser.resume_exn t.shared.pauser

let pause_status t = Pauser.status t.shared.pauser

let create ?(should_do_effects = true) ~root_directory () =
  let shared =
    { Shared.
      should_do_effects
    ; root_directory
    ; todo              = create_todo ()
    ; pauser            = Pauser.create ()
    }
  in
  let t =
    { shared
    ; relative_to_root   = Relpath.empty
    ; cache_invalidators = []
    }
  in
  if should_do_effects
  then
    Shutdown.at_shutdown (fun () ->
      (try resume_exn t with _ -> ());
      prior_changes_synced_to_file_system t);
  t
;;

let invalidate_dependents t =
  List.iter t.cache_invalidators ~f:Cached.Invalidator.invalidate_dependents;
;;

let enqueue t work =
  invalidate_dependents t;
  if t.shared.should_do_effects
  then don't_wait_for (Throttle.enqueue t.shared.todo work)
;;

let mkdir t path =
  Unix.mkdir ~p:() (path_to_string t path);
;;

let make_parent_dir t path =
  match Relpath.parent path with
  | None -> return ()
  | Some parent -> mkdir t parent
;;

let add_subtree t ~dir =
  enqueue t (fun () ->
    if verbose then Debug.ams [%here] "add_subtree" (t, dir)
                      [%sexp_of: t * Relpath.t];
    mkdir t dir);
;;

let remove_subtree_if_present_internal dir =
  match%map Process.run ~prog:"/bin/rm" ~args:[ "-rf"; "--"; dir ] () with
  | Ok (_stdout : string) -> Ok ()
  | Error _ as e -> e
;;

let remove_subtree_if_present_internal_exn dir =
  match%map remove_subtree_if_present_internal dir with
  | Ok () -> ()
  | Error err -> Error.raise err
;;

let remove_subtree t ~dir =
  enqueue t (fun () ->
    if verbose then Debug.ams [%here] "remove_subtree" dir [%sexp_of: Relpath.t];
    remove_subtree_if_present_internal_exn (path_to_string t dir))
;;

let rename t ~from_ ~to_ =
  enqueue t (fun () ->
    if verbose
    then Debug.ams [%here] "rename" (`from from_, `to_ to_)
           [%sexp_of: [ `from of Relpath.t ] * [ `to_ of Relpath.t ]];
    let%bind () = make_parent_dir t to_ in
    Unix.rename ~src:(path_to_string t from_) ~dst:(path_to_string t to_))
;;

let compressed_dir dir = concat [ dir ; ".tar.xz" ]
;;

let check_destination_status_exn operation_name ~source ~destination =
  let file_exists file =
    match%map Sys.file_exists file with
    | `Yes -> true
    | `No | `Unknown -> false
  in
  match%bind Deferred.both (file_exists source) (file_exists destination) with
  | false, true  -> return `Already_created
  | false, false ->
    raise_s
      [%sexp
        (sprintf "%s: no such directory" operation_name : string),
        { dir = (source : string) }
      ]
  | true, destination_exists ->
    let%map () =
      if destination_exists
      then
        (* The destination might just be stale.  Let's get rid of it and recreate a fresh
           one from the source. *)
        remove_subtree_if_present_internal_exn destination
      else return ()
    in
    `To_be_created
;;

module Compress_or_uncompress = struct
  type t =
    | Compress
    | Uncompress
  [@@deriving sexp_of]

  let operation_name = function
    | Compress   -> "compress_subtree"
    | Uncompress -> "uncompress_subtree"

  type source_and_destination = { source : string; destination : string }

  let source_and_destination operation t dir =
    let uncompressed_dir = path_to_string t dir in
    let compressed_dir = compressed_dir uncompressed_dir in
    match operation with
    | Compress   -> { source      = uncompressed_dir
                    ; destination = compressed_dir
                    }
    | Uncompress -> { source      = compressed_dir
                    ; destination = uncompressed_dir
                    }
  ;;

  let process operation ~source ~destination =
    match operation with
    | Compress ->
      (* here is some timing about the three biggest archived features with default
         compression level:

         {v
             compression    uncompression
         1.     1min            1.3s
         2.      47s            0.7s
         3.      27s            0.45s

         v}

         So uncompression is reasonably fast, but blocking the serializer for 1min seems
         undesirable.  So we use XZ_OPT=-1 instead so the times (at least for the worst
         feature) become:

         {v
             compression    uncompression
         1.      5s             1.6s

         v}

         The size of the compressed feature goes from about 9M to 13M. *)
      Process.run
        ~working_dir:(Filename.dirname source)
        ~env:(`Extend [ "XZ_OPT", "-1" ])
        ~prog:"/bin/tar"
        ~args:[ "-cJf" ; Filename.basename destination
              ; Filename.basename source
              ]
        ()

    | Uncompress ->
      Process.run
        ~working_dir:(Filename.dirname source)
        ~prog:"/bin/tar"
        ~args:[ "-xJf" ; Filename.basename source ]
        ()
  ;;
end

let idempotent_compress_or_uncompress operation t ~dir =
  let operation_name = Compress_or_uncompress.operation_name operation in
  enqueue t (fun () ->
    if verbose
    then
      Debug.eprint_s
        [%sexp
          (operation_name : string),
          [%here],
          { dir : Relpath.t }
        ];
    let { Compress_or_uncompress. source; destination } =
      Compress_or_uncompress.source_and_destination operation t dir
    in
    match%bind check_destination_status_exn operation_name ~source ~destination with
    | `Already_created -> return ()
    | `To_be_created ->
      if verbose
      then
        Debug.eprint_s
          [%sexp
            (operation_name : string),
            [%here],
            { source      : string
            ; destination : string
            ; working_dir = (Filename.dirname source : string)
            }
          ];
      match%bind Compress_or_uncompress.process operation ~source ~destination with
      | Error _ as res ->
        (* Make sure we do not leave a stale destination dir there *)
        let%map res' = remove_subtree_if_present_internal destination in
        ok_exn (Or_error.combine_errors_unit [res; res'])
      | Ok (_stdout : string) ->
        remove_subtree_if_present_internal_exn source
  )
;;

let uncompress_subtree_if_needed = idempotent_compress_or_uncompress Uncompress
let compress_subtree_if_needed   = idempotent_compress_or_uncompress Compress

let append_to (type a) t ~file a (persistent : a Persistent.Writer.t) =
  let module A = (val persistent) in
  enqueue t (fun () ->
    if verbose then Debug.ams [%here] "append_to" file [%sexp_of: Relpath.t];
    let%bind () = make_parent_dir t file in
    Writer.with_file (path_to_string t file) ~append:true ~f:(fun writer ->
      Writer.write_sexp writer ~hum:true (a |> [%sexp_of: A.Persist.t]);
      Writer.write writer "\n";
      return ()))
;;

let set_contents (type a) t ~file a (persistent : a Persistent.Writer.t) =
  let module A = (val persistent) in
  enqueue t (fun () ->
    if verbose then Debug.ams [%here] "set_contents" file [%sexp_of: Relpath.t];
    let%bind () = make_parent_dir t file in
    Writer.save_sexp ~hum:true (path_to_string t file) (a |> [%sexp_of: A.Persist.t]))
;;

let clear_sequence t ~file =
  enqueue t (fun () ->
    if verbose then Debug.ams [%here] "clear_sequence" file [%sexp_of: Relpath.t];
    let%bind () = make_parent_dir t file in
    Writer.save (path_to_string t file) ~contents:"")
;;

let relativize t ~dir =
  let t =
    { shared             = t.shared
    ; relative_to_root   = Relpath.append t.relative_to_root dir
    ; cache_invalidators = t.cache_invalidators
    }
  in
  add_subtree t ~dir:Relpath.empty;
  t
;;
