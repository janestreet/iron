open! Core
open! Async
open! Import

let rec iter_self_and_ancestors feature_path ~f =
  f feature_path;
  match Feature_path.parent feature_path with
  | Ok feature_path -> iter_self_and_ancestors feature_path ~f
  | Error _ -> ()
;;

let command =
  Command.async'
    ~summary:"complete partial feature path in scripts"
    ~readme:(fun () -> "\
Output on stdout all completion candidates for the given argument,
based on feature path universe read from stdin.")
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and full_name =
       no_arg_flag "-full-name"
         ~doc:" input to complete is assumed to start from the root feature"
     and input =
       anon ("INPUT" %: string)
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map lines = Reader.file_lines "/dev/stdin" in
       let features = Feature_path.Hash_set.create () in
       List.iter lines ~f:(fun line ->
         match String.strip line with
         | "" -> ()
         | line ->
           iter_self_and_ancestors (Feature_path.of_string line)
             ~f:(fun feature_path -> Hash_set.add features feature_path));
       Feature_path.complete
         ~iter_features:(fun ~f -> Hash_set.iter features ~f)
         ~prefix:input
         (if full_name then `Of_full_name else `Of_partial_name)
       |> List.sort ~cmp:String.compare
       |> List.iter ~f:print_endline
    )
;;
