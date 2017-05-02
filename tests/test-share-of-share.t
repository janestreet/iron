We sometimes call [Iron_hg.with_temp_share] recursively. Check that it's safe to
[hg share PREV NEXT] where [PREV] is itself a share.

  $ mkdir repo
  $ hg --config='extensions.share=' init --cwd repo
  $ hg --config='extensions.share=' share repo share-1 -q
  $ hg --config='extensions.share=' share share-1 share-2 -q
  $ diff share-1/.hg/sharedpath share-2/.hg/sharedpath
