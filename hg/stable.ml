module Brain                        = Brain.                        Stable
module Catch_up_session             = Catch_up_session.             Stable
module Compilation_status           = Hydra_state_for_bookmark.Stable.Compilation_status
module Cr_comment                   = Cr_comment.                   Stable
module Cr_soon                      = Cr_soon.                      Stable
module Cr_soon_multiset             = Cr_soon_multiset.             Stable
module Cr_soons                     = Cr_soons.                     Stable
module Diff2                        = Diff2.                        Stable
module Diff2s                       = Diff2s.                       Stable
module Diff4                        = Diff4.                        Stable
module Diff4_in_session             = Diff4_in_session.             Stable
module Diff4_to_catch_up            = Diff4_to_catch_up.            Stable
module Hydra_state_for_bookmark     = Hydra_state_for_bookmark.     Stable
module Latest_release               = Latest_release.               Stable
module Maybe_archived_feature_spec  = Maybe_archived_feature_spec.  Stable
module Next_base_update             = Next_base_update.             Stable
module Raw_rev                      = Raw_rev.                      Stable
module Released_feature             = Released_feature.             Stable
module Rename                       = Rename.                       Stable
module Rev                          = Rev.                          Stable
module Rev_facts                    = Rev_facts.                    Stable
module Review_edge                  = Review_edge.                  Stable
module Review_session               = Review_session.               Stable
module Worker_cache                 = Worker_cache.                 Stable
module Worker_obligations           = Worker_obligations.           Stable
module Worker_rev_facts             = Worker_rev_facts.             Stable

module Hg = struct
  module Tag = Hg.Tag.Stable
end

module Node_hash = struct
  module First_12 = Node_hash.First_12. Stable
  include Node_hash.                    Stable
end
