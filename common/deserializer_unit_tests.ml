open! Core
open! Async
open! Import

module M :
  (* This signature constraint is here to remind us to add a unit test whenever the
     interface to [Deserializer] changes. *)
  module type of Deserializer = struct

  open Deserializer

  type nonrec 'a t = 'a t

  module Applicative_infix = Applicative_infix
  module Monad_infix       = Monad_infix
  module Let_syntax        = Let_syntax

  let (>>|)       = (>>|)
  let (>>=)       = (>>=)
  let (<*>)       = (<*>)
  let ( *>)       = ( *>)
  let (<* )       = (<* )
  let all         = all
  let all_subdirs = all_subdirs
  let apply       = apply
  let in_subdir   = in_subdir
  let load        = load
  let map2        = map2
  let map3        = map3
  let map         = map
  let both        = both
  let one         = one
  let one_opt     = one_opt
  let sequence_of = sequence_of
  let sexp_of_t   = sexp_of_t
  let with_serializer = with_serializer

  let%test_unit _ = (* empty deserializer *)
    Thread_safe.block_on_async_exn (fun () ->
      load (return ()) ~root_directory:Abspath.root ~serializer:Serializer.do_nothing)
  ;;

  module Int = struct
    include Persistent.Make
        (struct let version = 1 end)
        (Int)
    include (Int : module type of Int with type t := t)
  end

  let temp_path = File_name.of_string "iron-deserializer-unit-test"

  let%test_unit _ = (* [sequence] of a nonexistent file gives an empty sequence *)
    Thread_safe.block_on_async_exn (fun () ->
      Path.with_temp_dir temp_path ~f:(fun root_directory ->
        Deferred.map
          (load (sequence_of (module Int) ~in_file:(Relpath.of_string "not-present"))
             ~root_directory ~serializer:Serializer.do_nothing)
          ~f:(fun list -> assert (List.is_empty list))))
  ;;

  let%test_unit _ =
    let open Async in
    Thread_safe.block_on_async_exn (fun () ->
      Path.with_temp_dir temp_path ~f:(fun root_directory ->
        let r = ref 0 in
        let file_name () =
          incr r;
          Relpath.of_string (String.concat [ "file_name"; Int.to_string !r ])
        in
        let file_name1 = file_name () in
        let file_name2 = file_name () in
        let file_name3 = file_name () in
        let file_name4 = file_name () in
        let file_name5 = file_name () in
        let file_name6 = file_name () in
        let file_name7 = file_name () in
        let serializer = Serializer.create ~root_directory () in
        let int1 = 13 in
        let ints = [ 14; 15 ] in
        let int2 = 16 in
        Serializer.set_contents serializer ~file:file_name1 int1 (module Int);
        List.iter ints ~f:(fun int ->
          Serializer.append_to serializer ~file:file_name2 int (module Int));
        Serializer.set_contents (Serializer.relativize serializer ~dir:file_name4)
          ~file:file_name5 int2 (module Int);
        let num_children = 10 in
        for i = 1 to num_children do
          let serializer =
            Serializer.relativize serializer
              ~dir:(Relpath.append
                      file_name6
                      (Relpath.of_string (Int.to_string i)))
          in
          Serializer.set_contents serializer ~file:file_name7 i (module Int);
        done;
        Serializer.prior_changes_synced_to_file_system serializer
        >>= fun () ->
        let t =
          let open Deserializer.Let_syntax in
          let%map_open () = return ()
          and int1 = one (module Int) ~in_file:file_name1
          and ints = sequence_of (module Int) ~in_file:file_name2
          and ()   = in_subdir file_name3 (return ())
          and int4 =
            in_subdir file_name4 (map (one (module Int) ~in_file:file_name5) ~f:Fn.id)
          and alist =
            in_subdir file_name6
              (all_subdirs (map (one (module Int) ~in_file:file_name7) ~f:Fn.id))
          in
          (int1, ints, int4, alist)
        in
        load t ~root_directory ~serializer:Serializer.do_nothing
        >>= fun (int1_, ints_, int2_, map) ->
        [%test_result: int] int1_ ~expect:int1;
        [%test_result: int list] ints_ ~expect:ints;
        [%test_result: int] int2_ ~expect:int2;
        [%test_result: (File_name.t * int) list]
          (List.sort (Map.to_alist map) ~cmp:(fun (_, i1) (_, i2) -> Int.compare i1 i2))
          ~expect:(List.init num_children ~f:(fun i ->
            let i = i + 1 in
            (File_name.of_string (Int.to_string i), i)));
        Serializer.prior_changes_synced_to_file_system serializer))
  ;;

  let all_ignore = all_ignore
  let bind = bind
  let ignore_m = ignore_m
  let join = join
  let return = return
  let upgrade_one = upgrade_one
end
