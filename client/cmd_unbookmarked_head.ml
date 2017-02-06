open Core
open Async
open Import

let prune ~root_feature rev =
  if am_functional_testing
  then return ()
  else (
    match%map
      Async_interactive.Job.run !"Pruning head: %{Rev#hum}" rev
        ~f:(fun () ->
          Process.run ()
            ~prog:"/j/office/app/hydra/prod/bin/hydra"
            ~args:[ "prune"
                  ; Rev.to_string_40 rev
                  ; "-repo"; concat [ "iron-"; Feature_name.to_string root_feature ]
                  ])
    with
    | Ok    (_stdout : string)  -> ()
    | Error (_       : Error.t) ->
      (* We don't fail in this case because [hydra prune] often exits nonzero even when
         it succeeds. *)
      ())
;;

let prune_command =
  Command.async' ~summary:"prune an unbookmarked head, so it is no longer a head"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and root_feature = root_feature
     and raw_rev = rev
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind repo_root =
         Cmd_workspace.repo_for_hg_operations_exn
           (Feature_path.of_root root_feature) ~use:`Clone
       in
       let%bind rev = Raw_rev.resolve_exn raw_rev ~in_:(Ok repo_root) in
       prune ~root_feature rev
    )
;;

let command =
  Command.group ~summary:"manage unbookmarked heads"
    [ "prune", prune_command
    ]
;;
