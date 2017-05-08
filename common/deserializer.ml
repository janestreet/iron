open Core
open Async
open! Import

module T = struct

  let verbose = Verbose.deserializer

  let message s = Verbose.message (concat [ "Deserializer."; s ])

  type _ t =
    | Const           : 'a -> 'a t
    | All_subdirs     : 'a t -> (File_name.t * 'a) list t
    | In_subdir       : Relpath.t * 'a t -> 'a t
    | Apply           : ('a -> 'b) t * 'a t -> 'b t
    | Bind            : 'a t * ('a -> 'b t) -> 'b t
    | One             : 'a Persistent.Reader.t * Relpath.t * 'a option -> 'a t
    | Sequence_of     : 'a Persistent.Reader.t * Relpath.t -> 'a list t
    | Upgrade_one     : 'a Persistent.Reader.t * Relpath.t * 'b Persistent.Reader.t * Relpath.t
                        * ('a -> 'b)-> 'b t
    | With_serializer : (Serializer.t -> 'a t) -> 'a t

  module Sexp_of = struct
    type t =
      | Const
      | All_subdirs of t
      | In_subdir of Relpath.t * t
      | Apply of t * t
      | Bind of t
      | One of Relpath.t
      | Sequence_of of Relpath.t
      | Upgrade_one of Relpath.t * Relpath.t
    [@@deriving sexp_of]
  end

  let rec sexp_of : type a . a t -> Sexp_of.t = function
    | Const _                       -> Const
    | All_subdirs t                 -> All_subdirs (sexp_of t)
    | In_subdir (path, t)           -> In_subdir (path, sexp_of t)
    | Apply (t1, t2)                -> Apply (sexp_of t1, sexp_of t2)
    | Bind (t, _)                   -> Bind (sexp_of t)
    | One (_, path, _)              -> One path
    | Sequence_of (_, path)         -> Sequence_of path
    | Upgrade_one (_, p1, _, p2, _) -> Upgrade_one (p1, p2)
    | With_serializer f             -> sexp_of (f Serializer.do_nothing)
  ;;

  let sexp_of_t _ t = sexp_of t |> [%sexp_of: Sexp_of.t]

  let in_subdir dir t = In_subdir (dir, t)

  let one ?default persistent ~in_file = One         (persistent, in_file, default)
  let sequence_of persistent ~in_file  = Sequence_of (persistent, in_file)

  let one_opt persistent ~in_file =
    one ~default:None (Persistent.Reader.map persistent ~f:Option.return) ~in_file
  ;;

  let upgrade_one ~from_:(p1, r1) ~to_:(p2, r2) f = Upgrade_one (p1, r1, p2, r2, f)

  let with_serializer f = With_serializer f

  let bind t ~f = Bind (t, f)

  let map t ~f = Bind (t, (fun x -> Const (f x)))

  let all_subdirs t = map (All_subdirs t) ~f:File_name.Map.of_alist_exn

  let load t ~root_directory ~serializer =
    let throttle = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:50 in
    let rec load
      : type a .
        (a t
         -> root_directory : Abspath.t
         -> serializer     : Serializer.t
         -> a Deferred.t) =
      fun t ~root_directory ~serializer ->
        if verbose
        then message "load" (t, root_directory, serializer)
               [%sexp_of: _ t * Abspath.t * Serializer.t];
        let extend path = Abspath.append root_directory path in
        let file_exists relpath =
          Abspath.file_exists_exn (extend relpath)
        in
        let one (type a) (persistent : a Persistent.Reader.t) in_file default =
          let load () =
            let module M = (val persistent) in
            Throttle.enqueue throttle (fun () ->
              Reader.load_sexp_exn (Abspath.to_string (extend in_file))
                [%of_sexp: M.Persist.t])
          in
          match default with
          | None -> load ()
          | Some d ->
            match%bind file_exists in_file with
            | true -> load ()
            | false -> Deferred.return d
        in
        match t with
        | Const c -> Deferred.return c
        | With_serializer f -> load (f serializer) ~root_directory ~serializer
        | In_subdir (dir, t) ->
          load t ~root_directory:(extend dir)
            ~serializer:(Serializer.relativize serializer ~dir)
        | One (persistent, in_file, default) -> one persistent in_file default
        | Upgrade_one (p1, f1, p2, f2, upgrade) ->
          let%bind f2_exists = file_exists f2 in
          if f2_exists
          then one p2 f2 None
          else (
            let%map a = one p1 f1 None in
            upgrade a)
        | Sequence_of (persistent, in_file) ->
          let%bind file_exists = file_exists in_file in
          if not file_exists
          then Deferred.return []
          else (
            let module M = (val persistent) in
            Throttle.enqueue throttle (fun () ->
              Reader.load_sexps_exn (Abspath.to_string (extend in_file))
                [%of_sexp: M.Persist.t]))
        | Bind (t, f) ->
          let%bind a = load t ~root_directory ~serializer in
          load (f a) ~root_directory ~serializer
        | Apply (tf, ta) ->
          let%bind f = load tf ~root_directory ~serializer in
          let%map a = load ta ~root_directory ~serializer in
          f a
        | All_subdirs t ->
          let%bind children = Sys.ls_dir (Abspath.to_string root_directory) in
          Deferred.List.map children ~how:`Parallel ~f:(fun child_name ->
            let child_name = File_name.of_string child_name in
            let child_dir = Relpath.of_list [ child_name ] in
            let child = extend child_dir in
            let%bind stat = Unix.stat (Abspath.to_string child) in
            match stat.kind with
            | `Directory ->
              let%map a =
                load t ~root_directory:child
                  ~serializer:(Serializer.relativize serializer ~dir:child_dir)
              in
              child_name, a
            | `File ->
              raise_s [%sexp "expected a directory but got a file", (child : Abspath.t)
                             , (t : _ t)]
            | `Char | `Block | `Link | `Fifo | `Socket ->
              raise_s [%sexp "unexpected file kind", (child : Abspath.t)
                             , (stat.kind : Unix.File_kind.t), (t : _ t)])
    in
    load t ~root_directory ~serializer;
  ;;

  include Applicative.Make (struct
      type nonrec 'a t = 'a t
      let return a = Const a
      let apply f a = Apply (f, a)
      let map = `Custom map
    end)

  include (Monad.Make (struct
             type nonrec 'a t = 'a t
             let return a = Const a
             let bind = bind
             let map = `Custom map
           end) : Monad.S_without_syntax with type 'a t := 'a t)
end
include T

module Let_syntax = struct
  let return = T.return
  module Let_syntax = struct
    include T
    module Open_on_rhs = T
  end
end
