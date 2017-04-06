(define_scrutiny normal
  ((min_file_reviewers 0)
   (max_file_reviewers 10)
   (level 50)
   (read_by_whole_feature_reviewers true)
   (description "\

Normal, every-day code, not critical-path.  Normal-scrutiny files do
not have to have file reviewers (although they can); in order to make
a change it is sufficient for the owner of a feature and the
whole-feature reviewer to agree.

")))

(users user1 user2)
