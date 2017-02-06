open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"show review edges for all users"
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
       let%bind { need_diff4s_starting_from; _ } =
         Hydra_worker.rpc_to_server_exn { feature_path; rev_zero; tip = None }
       in
       printf "%s\n"
         (need_diff4s_starting_from
          |> [%sexp_of: (Review_edge.t * User_name.Set.t) list]
          |> Sexp.to_string_hum);
       return ()
    )
;;
