Start test.

  $ start_test

Create a parent feature and a child feature. Run a bunch of commands on the child to make
it (almost) releaseable, except for stuff that needs to happen in the parent.

  $ setup_repo_and_root file
  $ fe create root/parent
  $ fe create root/parent/child
  $ touch f1.ml
  $ hg add f1.ml
  $ hg commit -m "0"
  $ feature_to_server root/parent/child -fake-valid
  $ fe enable-review
  $ fe tools mark-fully-reviewed root/parent/child
  $ fe second -even-though-owner

Start the pipe in the background, redirecting its output.

  $ dump_file="../updates.sexp"
  $ ( fe internal notify-on-feature-updates -id $(fe show root/parent/child -id) ; \
  >   echo "Process Exited" ) >> ${dump_file} &
  $ dump_process_pid=$!
  $ trap 'kill ${dump_process_pid} &>/dev/null || true; exit_trap' EXIT

  $ function cat-then-truncate {
  >   cat ${dump_file}
  >   truncate --size 0 ${dump_file}
  > }
  $ function path-and-next-step {
  >   sexp query "(pipe (variant Updated) (index 1) (cat (field feature_path) (field next_steps)))"
  > }

Have hydra process the parent feature.

  $ feature_to_server root/parent -fake-valid

Check that the pipe printed the child feature, since its next-steps changed.

  $ cat-then-truncate | path-and-next-step
  root/parent/child
  ((In_parent Enable_review))

Show the current event subscriptions.

  $ fe internal event-subscriptions show \
  >   | sexp change '(bottomup (seq (try (rewrite (query @X) (query <query>))) (try (rewrite (opened_at @X) (opened_at <time>)))))'
  (((metric_updates
     ((metric_name_subscriptions ()) (feature_path_subscriptions ())))
    (feature_updates
     ((feature_only_subscriptions
       ((* (1 unix-login-for-testing)))) (glob)
      (feature_and_descendants_subscriptions ()))))
   ((max_subscriptions_global 500) (current_count_global 1)
    (max_subscriptions_per_user 50)
    (current_count_by_user ((unix-login-for-testing 1)))
    (subscriptions
     (((rpc_name notify-on-feature-updates) (rpc_version *) (opened_at <time>) (glob)
       (ticks 1) (query <query>))))))

Create another parent feature and rename the child so as to change its parent. Then check
that the feature next-steps have changed.

  $ fe create root/other-parent
  $ fe rename root/parent/child root/other-parent/child -even-if-locked
  $ cat-then-truncate | path-and-next-step
  root/other-parent/child
  ((In_parent Wait_for_hydra))

Have hydra process the new parent to verify that the pipe now has a dependency on the new
parent.

  $ feature_to_server root/other-parent -fake-valid
  $ cat-then-truncate | path-and-next-step
  root/other-parent/child
  ((In_parent Enable_review))

Archive the child and verify that the pipe closes.

  $ fe archive root/other-parent/child
  $ wait ${dump_process_pid}
  $ cat-then-truncate
  Archived
  Process Exited

Clean up the dump file.

  $ rm -f ${dump_file}

And finally check that the event-subscriptions structure get cleaned up:

  $ fe internal event-subscriptions show
  (((metric_updates
     ((metric_name_subscriptions ()) (feature_path_subscriptions ())))
    (feature_updates
     ((feature_only_subscriptions ())
      (feature_and_descendants_subscriptions ()))))
   ((max_subscriptions_global 500) (current_count_global 0)
    (max_subscriptions_per_user 50) (current_count_by_user ())
    (subscriptions ())))
