(define_scrutiny high
  ((min_file_reviewers 2)
   (max_file_reviewers 10)
   (level 85)
   (read_by_whole_feature_reviewers true)
   (description "High-config_group code")
   (color red)))

(define_scrutiny ignore
  ((min_file_reviewers 0)
   (max_file_reviewers 10)
   (level 0)
   (read_by_whole_feature_reviewers true)
   (description "I don't want to hear about these files.")
   (color green)))

(users user1 user2 user3)
