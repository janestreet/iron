Start test.

  $ start_test

Create hg repo.

  $ mkdir repo
  $ cd repo
  $ hg init
  $ touch file
  $ hg add file
  $ hg com -m file

Create feature.

  $ fe create root -description 'root' -remote-repo-path $(pwd)
  $ feature_to_server root -fake-valid

  $ REV12=$(hg id | cut -d ' ' -f 1)

  $ fe internal rpc-to-server call synchronize-state <<EOF
  > ((remote_repo_path $PWD)
  >  (bookmarks
  >   (((bookmark root)
  >     (rev_info
  >      ((first_12_of_rev $REV12)
  >       (rev_author_or_error (Ok unix-login-for-testing))))
  >     (status Done)
  >     (continuous_release_status Not_working_on_it)
  >     (compilation_status
  >      ((root
  >        ((finished
  >          ((Working
  >            ((first_12_of_rev 1d4af7f6474e)
  >             (rev_author_or_error (Ok unix-login-for-testing))))))
  >         (pending
  >          (((first_12_of_rev $REV12)
  >            (rev_author_or_error (Ok unix-login-for-testing)))))))))))))
  > EOF
  ((bookmarks_to_rerun ()))

Show it.

  $ fe show root
  root
  ====
  root
  
  |---------------------------------------------------------------------|
  | attribute              | value                                      |
  |------------------------+--------------------------------------------|
  | next step              | add code                                   |
  | compilation status     | 1d4af7f6474e compiles, tip pending for 42s |
  | owner                  | unix-login-for-testing                     |
  | whole-feature reviewer | unix-login-for-testing                     |
  | seconder               | not seconded                               |
  | review is enabled      | false                                      |
  | CRs are enabled        | true                                       |
  | reviewing              | unix-login-for-testing                     |
  | is permanent           | false                                      |
  | tip                    | 04da3968e088                               |
  | base                   | 04da3968e088                               |
  |---------------------------------------------------------------------|

  $ fe show -compilation-status
  ((root
    ((finished
      ((Working
        ((first_12_of_rev 1d4af7f6474e)
         (rev_author_or_error (Ok unix-login-for-testing))))))
     (pending
      (((first_12_of_rev 04da3968e088)
        (rev_author_or_error (Ok unix-login-for-testing))
        (pending_since
         ((human_readable (*)) (glob)
          (int_ns_since_epoch *))))))))) (glob)

Check that if there are no changes affecting the feature during synchronize-state, there
is no spurious cache invalidation happening.

  $ function last_changed_at () {
  >     fe internal dump feature root | sexp query '(field cache_invalidator)(field last_changed_at)'
  > }

  $ CACHE_1=$(last_changed_at)
  $ fe internal rpc-to-server call synchronize-state <<EOF
  > ((remote_repo_path $PWD)
  >  (bookmarks
  >   (((bookmark root)
  >     (rev_info
  >      ((first_12_of_rev $REV12)
  >       (rev_author_or_error (Ok unix-login-for-testing))))
  >     (status Done)
  >     (continuous_release_status Not_working_on_it)
  >     (compilation_status
  >      ((root
  >        ((finished
  >          ((Working
  >            ((first_12_of_rev 1d4af7f6474e)
  >             (rev_author_or_error (Ok unix-login-for-testing))))))
  >         (pending
  >          (((first_12_of_rev $REV12)
  >            (rev_author_or_error (Ok unix-login-for-testing)))))))))))))
  > EOF
  ((bookmarks_to_rerun ()))
  $ CACHE_2=$(last_changed_at)
  $ test ${CACHE_1} -eq ${CACHE_2}

However, compilation-status changes correctly trigger a cache invalidation in the feature.

  $ fe internal rpc-to-server call synchronize-state <<EOF
  > ((remote_repo_path $PWD)
  >  (bookmarks
  >   (((bookmark root)
  >     (rev_info
  >      ((first_12_of_rev $REV12)
  >       (rev_author_or_error (Ok unix-login-for-testing))))
  >     (status Done)
  >     (continuous_release_status Not_working_on_it)
  >     (compilation_status
  >      ((root
  >        ((finished
  >          ((Working
  >            ((first_12_of_rev 2a5549ec7742)
  >             (rev_author_or_error (Ok unix-login-for-testing))))))
  >         (pending
  >          (((first_12_of_rev $REV12)
  >            (rev_author_or_error (Ok unix-login-for-testing)))))))))))))
  > EOF
  ((bookmarks_to_rerun ()))
  $ CACHE_2=$(last_changed_at)
  $ test ${CACHE_1} -eq ${CACHE_2}
  [1]

  $ fe create root/child -desc child1
  $ fe show root -remote-repo-path
  $TESTTMP/repo
  $ fe show root/child -remote-repo-path
  $TESTTMP/repo

  $ fe internal rpc-to-server call synchronize-state <<EOF
  > ((remote_repo_path $PWD)
  >  (bookmarks 
  >   (((bookmark root/child)
  >     (rev_info
  >      ((first_12_of_rev $REV12)
  >       (rev_author_or_error (Ok unix-login-for-testing))))
  >     (status Done) (continuous_release_status Not_working_on_it)
  >     (compilation_status
  >      ((root-release
  >        ((finished
  >          ((Working
  >            ((first_12_of_rev 6f38410a5738)
  >             (rev_author_or_error (Ok unix-login-for-testing))))))
  >         (pending
  >          (((first_12_of_rev $REV12)
  >            (rev_author_or_error (Ok unix-login-for-testing)))))))
  >       (root
  >        ((finished
  >          ((Broken
  >            ((last_working
  >              (((first_12_of_rev $REV12)
  >                (rev_author_or_error (Ok unix-login-for-testing)))))
  >             (first_broken
  >              ((first_12_of_rev 2a5549ec7742)
  >               (rev_author_or_error (Ok unix-login-for-testing))))
  >             (last_broken
  >              ((first_12_of_rev $REV12)
  >               (rev_author_or_error (Ok unix-login-for-testing))))))))
  >         (pending
  >          (((first_12_of_rev $REV12)
  >            (rev_author_or_error (Ok unix-login-for-testing)))))))))))))
  > EOF
  ((bookmarks_to_rerun (root/child)))

  $ fe show -show-full-compilation-status | grep -v '| pending for'
  root/child
  ==========
  child1
  
  |----------------------------------------------------------------------|
  | attribute               | value                                      |
  |-------------------------+--------------------------------------------|
  | next step               | wait for hydra                             |
  | compilation status      |                                            |
  |   root                  | tip broken, retrying for 42s               |
  |   root-release          | 6f38410a5738 compiles, tip pending for 42s |
  | owner                   | unix-login-for-testing                     |
  | whole-feature reviewer  | unix-login-for-testing                     |
  | seconder                | not seconded                               |
  | review is enabled       | false                                      |
  | CRs are enabled         | true                                       |
  | reviewing               | unix-login-for-testing                     |
  | is permanent            | false                                      |
  | tip                     | 04da3968e088                               |
  | base                    | 04da3968e088                               |
  |----------------------------------------------------------------------|

Show it, with the feature id included.

  $ fe show root -show-feature-id | grep '^| id ' | single_space
  | id | * | (glob)

Check that show works both when providing an id or a feature path.

  $ fe show root/child -feature-path
  root/child

  $ id=$(fe show root/child -id)

  $ fe show ${id} -feature-path
  root/child

An error message is provided in case of namespace mismatch.

  $ fe show -archived root/child
  (error
   (get-feature-maybe-archived
    ("namespace mismatch, feature is active"
     ((requested_feature
       ((feature_spec (Feature_path root/child)) (namespace Archived)))
      (actual_namespace Existing) (feature_path root/child)
      (feature_id *))))) (glob)
  [1]

  $ fe show -archived ${id}
  (error
   (get-feature-maybe-archived
    ("namespace mismatch, feature is active"
     ((requested_feature
       ((feature_spec (Feature_id *)) (glob)
        (namespace Archived)))
      (actual_namespace Existing) (feature_path root/child)
      (feature_id *))))) (glob)
  [1]

  $ fe show -existing-or-most-recently-archived root/child -feature-path
  root/child

Check that show still works even though the feature is archived.

  $ fe archive root/child

  $ fe show -archived root/child -feature-path
  root/child

  $ fe show -archived ${id} -feature-path
  root/child

  $ fe show -existing-or-most-recently-archived root/child -feature-path
  root/child

And that regardless whether [-archived] is in the first or last position.

  $ fe show root/child -feature-path -archived
  root/child

  $ fe show ${id} -feature-path -archived
  root/child

Note that when providing the id, the -archived flag is not mandatory in case of
success.

  $ fe show ${id} -feature-path -is-archived
  ((Feature_path root/child) (Is_archived true))

But to avoid a surprising behavior change for users, when the lookup is done by
feature path, the flag [-archived] is mandatory.

  $ fe show root/child
  (error
   (get-feature-maybe-archived
    ("namespace mismatch, no such active feature"
     (requested_feature
      ((feature_spec (Feature_path root/child)) (namespace Existing)))
     (actual_namespace Archived)
     ("archived feature matching"
      (((feature_id *)) (glob)
       ((feature_path root/child) (owners (unix-login-for-testing))
        (archived_at (*)))))))) (glob)
  [1]

However, for feature that are neither archived or active, the error message is
more simple.

  $ fe show root/turlututu/chapeau-pointu
  (error
   (get-feature-maybe-archived
    ("no such feature" (Feature_path root/turlututu/chapeau-pointu))))
  [1]

Check that show gets the correct attributes for archived features.  In
particular, no pending attributes, and the view makes it clear that there is no
next steps, and that CRs and line count are not available.

  $ fe show -archived root/child
  root/child
  ==========
  child1
  
  |---------------------------------------------------------------|
  | attribute              | value                                |
  |------------------------+--------------------------------------|
  | id                     | * | (glob)
  | is archived            | true                                 |
  | owner                  | unix-login-for-testing               |
  | whole-feature reviewer | unix-login-for-testing               |
  | seconder               | not seconded                         |
  | review is enabled      | false                                |
  | CRs are enabled        | true                                 |
  | reviewing              | unix-login-for-testing               |
  | is permanent           | false                                |
  | tip                    | 04da3968e088                         |
  | base                   | 04da3968e088                         |
  |---------------------------------------------------------------|
  
  ("not showing crs" "CRs unavailable for archived features")
  
  ("not showing line counts" "Line counts unavailable for archived features")

Check that show still works after re-unarchiving.

  $ fe unarchive root/child -id ${id}

  $ fe show root/child -feature-path
  root/child

Check that the archived flag correctly shows the archived vs live feature when
the feature path is ambiguous.

  $ fe archive root/child

  $ fe create root/child -desc child2
  $ id2=$(fe show root/child -id)

  $ fe show root/child -omit-attribute-table
  root/child
  ==========
  child2

  $ fe show -archived root/child -omit-attribute-table
  root/child
  ==========
  child1
  
  ("not showing crs" "CRs unavailable for archived features")
  
  ("not showing line counts" "Line counts unavailable for archived features")

Check that -is-archived outputs true when feature is archived and false otherwise.

  $ fe show ${id2} -is-archived
  false

  $ fe show ${id} -is-archived
  true

  $ fe show -archived ${id} -is-archived
  true

Check that partial path completion is fine for live and archived features.

  $ fe show root/chi -omit-attribute-table
  root/child
  ==========
  child2

  $ fe show -archived root/chi -omit-attribute-table
  root/child
  ==========
  child1
  
  ("not showing crs" "CRs unavailable for archived features")
  
  ("not showing line counts" "Line counts unavailable for archived features")

  $ fe show -existing-or-most-recently-archived root/chi -omit-attribute-table
  root/child
  ==========
  child2

Check that show fails when there are multiple archived features of the same name
with the ids of those features.

  $ most_recently_archived_id=$(fe show -id root/child)
  $ fe archive root/child

  $ fe show -archived root/child -omit-attribute-table
  (error
   (get-feature-maybe-archived
    ("multiple archived features matching"
     ((((feature_id *)) (glob)
       ((feature_path root/child) (owners (unix-login-for-testing))
        (archived_at (*)))) (glob)
      (((feature_id *)) (glob)
       ((feature_path root/child) (owners (unix-login-for-testing))
        (archived_at (*)))))))) (glob)
  [1]

  $ fe show -existing-or-most-recently-archived root/child -id \
  >   | sed "s;$most_recently_archived_id;\$most_recently_archived_id;"
  $most_recently_archived_id

  $ fe show -archived ${id} -omit-attribute-table
  root/child
  ==========
  child1
  
  ("not showing crs" "CRs unavailable for archived features")
  
  ("not showing line counts" "Line counts unavailable for archived features")

  $ fe show -archived ${id2} -omit-attribute-table
  root/child
  ==========
  child2
  
  ("not showing crs" "CRs unavailable for archived features")
  
  ("not showing line counts" "Line counts unavailable for archived features")

Check that things still work when we can't find the feature protocol in the cache.

  $ fe-server stop
  $ fe-server start

  $ fe show -archived ${id} -omit-attribute-table
  root/child
  ==========
  child1
  
  ("not showing crs" "CRs unavailable for archived features")
  
  ("not showing line counts" "Line counts unavailable for archived features")

  $ fe show -archived ${id2} -omit-attribute-table
  root/child
  ==========
  child2
  
  ("not showing crs" "CRs unavailable for archived features")
  
  ("not showing line counts" "Line counts unavailable for archived features")

Check that the cache operations work properly.

  $ fe-server stop
  $ fe-server start

  $ fe internal archived-features-cache show
  ((max_size 500) (length 0) (keys ()))

  $ fe show -archived ${id} -feature-path
  root/child

  $ fe internal archived-features-cache show
  ((max_size 500) (length 1) (keys (*))) (glob)

  $ fe show -archived ${id2} -feature-path
  root/child

  $ fe internal archived-features-cache show
  ((max_size 500) (length 2)
   (keys
    (*))) (glob)

  $ fe internal archived-features-cache show -feature-paths
  ((* root/child) (glob)
   (* root/child)) (glob)

  $ fe internal archived-features-cache set-max-size 1

  $ fe internal archived-features-cache show
  ((max_size 1) (length 1) (keys (*))) (glob)

  $ fe internal archived-features-cache clear -all

  $ fe internal archived-features-cache show
  ((max_size 1) (length 0) (keys ()))

Check persist properties of the cache.

  $ fe internal archived-features-cache set-max-size 42
  $ fe show -archived ${id2} -omit-attribute-table > /dev/null
  $ fe internal archived-features-cache show
  ((max_size 42) (length 1) (keys (*))) (glob)

  $ fe-server stop
  $ fe-server start

  $ fe internal archived-features-cache show
  ((max_size 42) (length 0) (keys ()))
