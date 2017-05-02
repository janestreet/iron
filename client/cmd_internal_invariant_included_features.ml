open! Core
open! Async
open! Import

(* For all features F, G s.t. G is an included feature of F, the tip of G should be an
   ancestor of the tip of F.

   Direct release allows a race condition that can break this invariant. *)

module Lost_feature = struct
  let sexp_of_t (t : Released_feature.t) =
    if am_functional_testing
    then [%sexp_of: Feature_path.t] t.feature_path
    else [%sexp_of: Released_feature.t] t
  ;;
end

let check_one_feature_exn feature_path repo_root rev_zero ~how =
  let%bind feature =
    Get_feature.rpc_to_server_exn
      { feature_path
      ; rev_zero = Some rev_zero
      }
  in
  match%map
    Deferred.List.filter ~how feature.included_features ~f:(fun included_feature ->
      Hg.is_ancestor repo_root ~ancestor:included_feature.tip ~descendant:feature.tip
      >>| not)
  with
  | [] -> ()
  | (_ :: _) as lost_features ->
    raise_s
      [%sexp
        (feature.feature_path : Feature_path.t)
      , "has tip", (feature.tip : Hg.Rev.t)
      , "which does not descend from these included features:"
      , (lost_features : Lost_feature.t list)]
;;

let command =
  Command.async'
    ~summary:"check that feature tip descends from every included feature's tip"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind repo_root =
         Cmd_workspace.repo_for_hg_operations_exn feature_path ~use:`Clone
       in
       let%bind rev_zero = Hg.create_rev_zero repo_root in
       let how = `Max_concurrent_jobs 2 in
       check_one_feature_exn feature_path repo_root rev_zero ~how)
;;
