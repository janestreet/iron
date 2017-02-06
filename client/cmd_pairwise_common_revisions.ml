open! Core
open! Async
open! Import

let print_sexp sexp =
  printf "%s\n" (Sexp.to_string_hum sexp)
;;

let command =
  Command.async'
    ~summary:"show revisions common to pairs of features"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and subtrees =
       feature_path_flagged_listed
         ~label:"subtree" ~doc:"FEATURE check descendants of this feature"
     and features = anon_feature_paths
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let subtrees = ok_exn subtrees in
       let features = ok_exn features in
       let%bind repo_root =
         Cmd_workspace.repo_for_hg_operations_use_clone_exn features
       in
       let%bind rev_zero = Hg.create_rev_zero repo_root in
       let%bind { remote_repo_path; features } =
         List_feature_revisions.rpc_to_server_exn { rev_zero; features; subtrees }
       in
       let%bind () =
         Hg.pull ~even_if_unclean:true repo_root ~from:remote_repo_path `All_revs
       in
       let features =
         (* Sorting so that the output is deterministic.  Client side for performance. *)
         List.sort features ~cmp:(fun f1 f2 ->
           let module R = List_feature_revisions.Reaction in
           Feature_path.compare f1.R.feature_path f2.feature_path)
       in
       let module Revs = Rev.Compare_by_hash.Set in
       let%map revs =
         Deferred.List.map features ~how:(`Max_concurrent_jobs 10)
           ~f:(fun { feature_path; base; tip } ->
             let%map revs =
               Hg.create_revs repo_root (Revset.for_feature ~base ~tip)
             in
             (Revs.of_list (ok_exn revs), feature_path))
       in
       let check (revs1, feature_path1) (revs2, feature_path2) =
         let common_revisions = Set.inter revs1 revs2 in
         if not (Set.is_empty common_revisions)
         then
           print_sexp
             [%sexp
               "common_revisions",
               { feature1         = (feature_path1    : Feature_path.t)
               ; feature2         = (feature_path2    : Feature_path.t)
               ; common_revisions = (common_revisions : Revs.t)
               }
             ]
       in
       let rec loop = function
         | [] -> ()
         | f :: fs ->
           List.iter fs ~f:(check f);
           loop fs
       in
       loop revs
    )
;;
