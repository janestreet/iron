open! Core
open! Async
open! Import

let sort repo_root ~below =
  let%bind manifest = Hg.manifest repo_root `Dirstate in
  let files =
    List.filter_map manifest ~f:(fun path_in_repo ->
      match Path_in_repo.chop_prefix ~prefix:below path_in_repo with
      | Ok relpath -> Some (path_in_repo, relpath)
      | Error _ -> None)
  in
  Build_order.sort (Ok (repo_root, Program_started_in)) files fst
;;

let command =
  Command.async'
    ~summary:"print files from manifest below current dir in build dependency order"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       let repo_root = ok_exn Repo_root.program_started_in in
       let below = Repo_root.relativize_exn repo_root Abspath.program_started_in in
       let%map files = sort repo_root ~below in
       List.iter files ~f:(fun (_path_in_repo, relpath) ->
         Print.printf "%s\n" (Relpath.to_string relpath))
    )
;;
