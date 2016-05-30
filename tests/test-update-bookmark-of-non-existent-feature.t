  $ start_test

  $ fe internal rpc-to-server call update-bookmark <<EOF
  > ((feature_path root)
  >  (feature_id 38e615e0-f54c-3b2e-a6ce-15cc85cf34e6)
  >  (info (Error ""))
  >  (augment_worker_cache (
  >    (worker_obligations ())
  >    (worker_rev_facts   ())
  >  )))
  > EOF
  ()
