Check expected behaviour around the [fe admin server metrics] command.

  $ start_test

Authorize the running user to feed metrics.  In prod this is going to be
reserved to a restricted list of special users.

  $ fe admin users feeding-metrics add unix-login-for-testing

Add stats.

  $ fe admin server metrics add root/child1 -metric metric1 -value 2.24
  $ fe admin server metrics add root        -metric metric2 -value 3.1,3.1,3.1
  $ fe admin server metrics add root/child2 -metric metric1 -value 4.2,2

  $ fe admin server metrics show root -stat count,mean -depth max -show-by-stat
  count
  |------------------------------|
  | feature  | metric1 | metric2 |
  |----------+---------+---------|
  | root     |         |       3 |
  |   child1 |       1 |         |
  |   child2 |       2 |         |
  |------------------------------|
  
  mean
  |------------------------------|
  | feature  | metric1 | metric2 |
  |----------+---------+---------|
  | root     |         |    3.10 |
  |   child1 |    2.24 |         |
  |   child2 |    3.10 |         |
  |------------------------------|
  
  $ fe admin server metrics show -stat count,mean -show-by-stat
  count
  |-----------------------------|
  | feature | metric1 | metric2 |
  |---------+---------+---------|
  | root    |       3 |       3 |
  |-----------------------------|
  
  mean
  |-----------------------------|
  | feature | metric1 | metric2 |
  |---------+---------+---------|
  | root    |    2.81 |    3.10 |
  |-----------------------------|
  
  $ fe admin server metrics show root -metric metric1 -show-by-stat \
  > -depth 0
  mean
  |-------------------|
  | feature | metric1 |
  |---------+---------|
  | root    |    2.81 |
  |-------------------|
  
  min
  |-------------------|
  | feature | metric1 |
  |---------+---------|
  | root    |    2.00 |
  |-------------------|
  
  max
  |-------------------|
  | feature | metric1 |
  |---------+---------|
  | root    |    4.20 |
  |-------------------|
  
  count
  |-------------------|
  | feature | metric1 |
  |---------+---------|
  | root    |       3 |
  |-------------------|
  
  total
  |-------------------|
  | feature | metric1 |
  |---------+---------|
  | root    |    8.44 |
  |-------------------|
  
  $ fe admin server metrics show root -metric metric2
  metric2
  |----------------------------------------------|
  | feature | mean |  min |  max | count | total |
  |---------+------+------+------+-------+-------|
  | root    | 3.10 | 3.10 | 3.10 |     3 |  9.30 |
  |----------------------------------------------|
  
  $ fe admin server metrics show root -metric metric1 -depth 1
  metric1
  |-----------------------------------------------|
  | feature  | mean |  min |  max | count | total |
  |----------+------+------+------+-------+-------|
  | root     |      |      |      |       |       |
  |   child1 | 2.24 | 2.24 | 2.24 |     1 |  2.24 |
  |   child2 | 3.10 | 2.00 | 4.20 |     2 |  6.20 |
  |-----------------------------------------------|
  
  $ fe admin server metrics show root -metric metric1 -stat mean,count \
  > -depth 0
  metric1
  |------------------------|
  | feature | mean | count |
  |---------+------+-------|
  | root    | 2.81 |     3 |
  |------------------------|
  
  $ fe admin server metrics show root -metrics metric1,metric2 -stat \
  > mean,count -depth 0
  metric1
  |------------------------|
  | feature | mean | count |
  |---------+------+-------|
  | root    | 2.81 |     3 |
  |------------------------|
  
  metric2
  |------------------------|
  | feature | mean | count |
  |---------+------+-------|
  | root    | 3.10 |     3 |
  |------------------------|
  
  $ fe admin server metrics show root -metrics ^metric$ -stat mean,count

  $ fe admin server metrics add root/child1 -metric regex1 -value 4.2
  $ fe admin server metrics show root -metrics r.* -stat mean
  regex1
  |-----------------|
  | feature  | mean |
  |----------+------|
  | root     |      |
  |   child1 | 4.20 |
  |-----------------|
  
  $ fe admin server metrics show root -metrics m.* -stat mean,count \
  > -depth 0
  metric1
  |------------------------|
  | feature | mean | count |
  |---------+------+-------|
  | root    | 2.81 |     3 |
  |------------------------|
  
  metric2
  |------------------------|
  | feature | mean | count |
  |---------+------+-------|
  | root    | 3.10 |     3 |
  |------------------------|
  

  $ fe admin server metrics show -metrics m.* -depth 0 -show-by-stat
  mean
  |-----------------------------|
  | feature | metric1 | metric2 |
  |---------+---------+---------|
  | *       |    2.81 |    3.10 |
  |-----------------------------|
  
  min
  |-----------------------------|
  | feature | metric1 | metric2 |
  |---------+---------+---------|
  | *       |    2.00 |    3.10 |
  |-----------------------------|
  
  max
  |-----------------------------|
  | feature | metric1 | metric2 |
  |---------+---------+---------|
  | *       |    4.20 |    3.10 |
  |-----------------------------|
  
  count
  |-----------------------------|
  | feature | metric1 | metric2 |
  |---------+---------+---------|
  | *       |       3 |       3 |
  |-----------------------------|
  
  total
  |-----------------------------|
  | feature | metric1 | metric2 |
  |---------+---------+---------|
  | *       |    8.44 |    9.30 |
  |-----------------------------|
  

  $ fe admin server metrics add root -metric metric.with.dots -value 2
  $ fe admin server metrics show -metrics 'metric\..*' -decimals 0
  metric.with.dots
  |--------------------------------------------|
  | feature | mean | min | max | count | total |
  |---------+------+-----+-----+-------+-------|
  | root    |    2 |   2 |   2 |     1 |     2 |
  |--------------------------------------------|
  

  $ fe admin server metrics add jane -metric metric3 -value 2

  $ fe admin server metrics show -metrics metric3
  metric3
  |----------------------------------------------|
  | feature | mean |  min |  max | count | total |
  |---------+------+------+------+-------+-------|
  | jane    | 2.00 | 2.00 | 2.00 |     1 |  2.00 |
  |----------------------------------------------|
  

  $ completion-test fe admin server metrics show root/child1 -metrics met
  metric.with.dots
  metric1
  metric2
  metric3

  $ fe admin server metrics list
  metric.with.dots
  metric1
  metric2
  metric3
  regex1

  $ fe admin server metrics list root/child1
  metric1
  regex1

  $ fe admin server metrics clear -metric metric1 root -rec
  $ fe admin server metrics show -metrics metric1

Test the subscription API.

  $ function subscribe {
  >   dump_file=updates.sexp
  >   ( fe admin server metrics subscribe $@ ; \
  >     echo "Process Exited" ) >> ${dump_file} 2>/dev/null &
  >   dump_process_pid=$!
  >   trap 'kill ${dump_process_pid} &> /dev/null || true; exit_trap' EXIT
  >   sleep 0.5
  >   fe admin server metrics add root -metric M1 -value 101
  >   fe admin server metrics add jane -metric M1 -value 102
  >   fe admin server metrics add root -metric M2 -value 201
  >   fe admin server metrics add jane -metric M2 -values 202,203
  >   sleep 0.5
  >   fe internal event-subscriptions drop-all-by-users -for unix-login-for-testing
  >    wait ${dump_process_pid}
  > }

Test subscription by feature, metric, and feature & metric.

  $ subscribe root
  $ subscribe -metric M1
  $ subscribe jane -metric M2

Check notifications:

  $ cat ${dump_file}
  ((feature_path root)(metric_name M1)(value 101)(added_at(*))) (glob)
  ((feature_path root)(metric_name M2)(value 201)(added_at(*))) (glob)
  Process Exited
  ((feature_path root)(metric_name M1)(value 101)(added_at(*))) (glob)
  ((feature_path jane)(metric_name M1)(value 102)(added_at(*))) (glob)
  Process Exited
  ((feature_path jane)(metric_name M2)(value 202)(added_at(*))) (glob)
  ((feature_path jane)(metric_name M2)(value 203)(added_at(*))) (glob)
  Process Exited

Create a feature.

  $ hg init repo
  $ cd repo
  $ remote="$PWD"
  $ touch file; hg add file; hg com -m add
  $ fe create root -desc root -remote-repo-path $remote
  $ feature_to_server root -fake-valid

Create a push event.

  $ touch file2; hg add file2; hg com -m add
  $ rev=$(hg log -r . --template '{node|short}')
  $ fe tools hg-hooks post-push -fg 2>/dev/null

The feature is not shown as pending until the first synchronize state RPC
mentioning this revision is received.

  $ fe show | grep "pending for"
  [1]

  $ simple-sync-state root $rev Pending_or_working_on_it
  ((bookmarks_to_rerun ()))

  $ fe show | matches "pending for"

This latency is registered as a metric:

  $ fe admin server metrics show -metrics 'hydra\.synchronize.*' -stats count -depth max
  hydra.synchronize-state-latency
  |-----------------|
  | feature | count |
  |---------+-------|
  | root    |     1 |
  |-----------------|
  
When the update bookmark is received, we store the total latency.

  $ feature_to_server root -fake-valid

  $ fe admin server metrics show -metrics 'hydra\.update.*' -stats count -depth max
  hydra.update-bookmark-latency
  |-----------------|
  | feature | count |
  |---------+-------|
  | root    |     1 |
  |-----------------|
  
Repeat the operation, the results are aggregated over time.

  $ touch file3; hg add file3; hg com -m add
  $ rev2=$(hg log -r . --template '{node|short}')
  $ fe tools hg-hooks post-push -fg 2>/dev/null
  $ simple-sync-state root $rev2 Pending_or_working_on_it
  ((bookmarks_to_rerun ()))
  $ feature_to_server root -fake-valid

  $ fe admin server metrics show -metrics 'hydra\..*' -stats count -depth max
  hydra.synchronize-state-latency
  |-----------------|
  | feature | count |
  |---------+-------|
  | root    |     2 |
  |-----------------|
  
  hydra.update-bookmark-latency
  |-----------------|
  | feature | count |
  |---------+-------|
  | root    |     2 |
  |-----------------|
  
If the update-bookmark comes first, we do not register a synchronize-state
latency.

  $ touch file4; hg add file4; hg com -m add
  $ rev3=$(hg log -r . --template '{node|short}')
  $ fe tools hg-hooks post-push -fg 2>/dev/null
  $ feature_to_server root -fake-valid
  $ simple-sync-state root $rev3 Done
  ((bookmarks_to_rerun ()))

  $ fe admin server metrics show -metrics 'hydra\..*' -stats count -depth max
  hydra.synchronize-state-latency
  |-----------------|
  | feature | count |
  |---------+-------|
  | root    |     2 |
  |-----------------|
  
  hydra.update-bookmark-latency
  |-----------------|
  | feature | count |
  |---------+-------|
  | root    |     3 |
  |-----------------|
  
  $ fe internal push-events show
  ((by_rev_size 10) (user_count 1) (feature_id_count 1) (total_rev_count 3))
  $ fe internal push-events show -values \
  >   | sed -e "s/$rev/\$rev/g" | sed -e "s/$rev2/\$rev2/g" | sed -e "s/$rev3/\$rev3/g"
  ((* (glob)
    ((max_size 10) (length 3)
     (items
      (($rev
        ((query
          ((by unix-login-for-testing) (at (*)))) (glob)
         (used_by_metrics
          (hydra.synchronize-state-latency hydra.update-bookmark-latency))))
       ($rev2
        ((query
          ((by unix-login-for-testing) (at (*)))) (glob)
         (used_by_metrics
          (hydra.synchronize-state-latency hydra.update-bookmark-latency))))
       ($rev3
        ((query
          ((by unix-login-for-testing) (at (*)))) (glob)
         (used_by_metrics (hydra.update-bookmark-latency)))))))))

  $ fe internal push-events show -user as-hydra-worker
  ()

Change some settings on the push-events.

  $ fe internal push-events set-max-size-per-feature 1
  $ fe internal push-events show
  ((by_rev_size 1) (user_count 1) (feature_id_count 1) (total_rev_count 1))
  $ fe internal push-events show -values | sed -e "s/$rev2/\$rev2/g" | sed -e "s/$rev3/\$rev3/g"
  ((* (glob)
    ((max_size 1) (length 1)
     (items
      (($rev3
        ((query
          ((by unix-login-for-testing) (at (*)))) (glob)
         (used_by_metrics (hydra.update-bookmark-latency)))))))))

Clear the events.

  $ fe internal push-events clear | tail -n 1
  One should explicitly specify what to clear or supply -all.
  [1]

  $ fe internal push-events clear -all
  $ fe internal push-events show
  ((by_rev_size 1) (user_count 0) (feature_id_count 0) (total_rev_count 0))
