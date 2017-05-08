open Core
open Async
open! Import

open Serializer

let%test_unit _ =
  let t = do_nothing in
  ignore (t |> [%sexp_of: t] : Sexp.t)
;;

let%test_unit _ = invariant do_nothing

let%test_unit _ =
  let path = Abspath.root in
  let t = create ~should_do_effects:false ~root_directory:path () in
  [%test_result: Abspath.t] (root_directory t) ~expect:path
;;

let%test_unit _ =
  let path = Abspath.root in
  let t = create ~should_do_effects:false ~root_directory:path () in
  [%test_result: Relpath.t] (relative_to_root t) ~expect:Relpath.empty;
  let t = relativize t ~dir:Relpath.empty in
  [%test_result: Relpath.t] (relative_to_root t) ~expect:Relpath.empty;
  let dir = Relpath.of_list [ File_name.of_string "foo" ] in
  let t = relativize t ~dir in
  [%test_result: Relpath.t] (relative_to_root t) ~expect:dir
;;

let with_temp ?should_do_effects ~f () =
  Path.with_temp_dir (File_name.of_string "iron-serializer-unit-test")
    ~f:(fun root_directory -> f (create ~root_directory ?should_do_effects ()))
;;

module Int = struct
  include Persistent.Make
      (struct let version = 1 end)
      (Int)
end

let%test_unit _ =
  Thread_safe.block_on_async_exn (fun () ->
    with_temp () ~f:(fun t ->
      Deferred.List.iter
        [ t
        ; relativize t ~dir:(Relpath.of_string "dir" )
        ; relativize t ~dir:(Relpath.of_string "dir1/dir2")
        ]
        ~f:(fun t ->
          let file = Relpath.of_string "foo" in
          let absolute_file =
            Abspath.to_string
              (Abspath.append
                 (Abspath.append (root_directory t) (relative_to_root t))
                 file)
          in
          let int1 = 13 in
          set_contents t ~file int1 (module Int);
          let%bind () = prior_changes_synced_to_file_system t in
          let%bind int1_ = Reader.load_sexp_exn absolute_file [%of_sexp: Int.Persist.t] in
          [%test_result: int] int1_ ~expect:int1;
          let int2 = 14 in
          append_to t ~file int2 (module Int);
          let%bind () = prior_changes_synced_to_file_system t in
          let%bind ints = Reader.load_sexps_exn absolute_file [%of_sexp: Int.Persist.t] in
          [%test_result: int list] ints ~expect:[ int1; int2 ];
          return ())))
;;

let%test_unit _ = (* appending to a file that doesn't exist *)
  Thread_safe.block_on_async_exn (fun () ->
    with_temp () ~f:(fun t ->
      let file = Relpath.of_string "foo" in
      let absolute_file =
        Abspath.to_string
          (Abspath.append
             (Abspath.append (root_directory t) (relative_to_root t))
             file)
      in
      let int1 = 13 in
      let int2 = 14 in
      append_to t ~file int1 (module Int);
      append_to t ~file int2 (module Int);
      let%bind () = prior_changes_synced_to_file_system t in
      let%bind ints =
        Reader.load_sexps_exn absolute_file [%of_sexp: Int.Persist.t]
      in
      [%test_result: int list] ints ~expect:[ int1; int2 ];
      return ()))
;;

let%test_unit _ = (* moving a directory *)
  Thread_safe.block_on_async_exn (fun () ->
    with_temp () ~f:(fun t ->
      let file = Relpath.of_string "foo/bar/baz" in
      let int1 = 13 in
      set_contents t ~file int1 (module Int);
      rename t
        ~from_:(Relpath.of_string "foo/bar")
        ~to_:(Relpath.of_string "zzz");
      let%bind () = prior_changes_synced_to_file_system t in
      let absolute_file =
        Abspath.to_string
          (Abspath.append
             (root_directory t) (Relpath.of_string "zzz/baz"))
      in
      let%bind int2 =
        Reader.load_sexp_exn absolute_file [%of_sexp: Int.Persist.t]
      in
      [%test_result: int] int2 ~expect:int1;
      return ()))
;;

let%test_unit _ = (* no side effects with [~should_do_effects:false] *)
  Thread_safe.block_on_async_exn (fun () ->
    with_temp () ~should_do_effects:false ~f:(fun t ->
      let foo = Relpath.of_string "foo" in
      let dir = foo in
      let file = foo in
      ignore (relativize t ~dir : t);
      add_subtree t ~dir;
      remove_subtree t ~dir;
      append_to t ~file 13 (module Int);
      rename t ~from_:foo ~to_:foo;
      set_contents t ~file 13 (module Int);
      let%bind () = prior_changes_synced_to_file_system t in
      match%bind Sys.ls_dir (Abspath.to_string (root_directory t)) with
      | [] -> return ()
      | files -> raise_s [%sexp "unexpectedly nonempty dir", (files : string list)]))
;;
