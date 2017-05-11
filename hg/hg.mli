open! Core
open! Async
open! Import

(** A 40-char hg node id, which uniquely identifies a revision. *)
module Node_hash : sig

  (** The first 12 characters of a node hash.  We prefer to use the full 40-char node id,
      but hydra only has the 12-char hash. *)
  module First_12 : sig
    type t [@@deriving sexp_of]

    include Invariant.S        with type t := t
    include Comparable.S_plain with type t := t
    include Hashable.S_plain   with type t := t

    val of_string : string -> t
    val to_string : t -> string

    module Stable : sig
      module V1 : Stable_without_comparator with type t = t
    end
  end

  type t [@@deriving sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S         with type t := t

  val to_string : t -> string
  val to_file_name : t -> File_name.t
  val to_first_12 : t -> First_12.t

  module Stable : sig
    module V1 : Stable_without_comparator with type t = t
  end
end

(** A node hash and an optional human-readable name. *)
module Rev : sig

  type t [@@deriving sexp_of]

  include Invariant. S with type t := t

  module Compare_by_hash : sig
    type nonrec t = t [@@deriving compare, sexp_of]

    include Invariant.S        with type t := t
    include Comparable.S_plain with type t := t
    include Hashable.S_plain   with type t := t
  end

  val compare : [ `Do_not_use ]
  val equal   : [ `Do_not_use ]

  val equal_node_hash : t -> t -> bool
  val has_prefix : t -> Node_hash.First_12.t -> bool

  val node_hash     : t -> Node_hash.t
  val to_first_12   : t -> Node_hash.First_12.t
  val to_string_12  : t -> string
  val to_string_40  : t -> string
  val to_string_hum : ?lossy:bool -> t -> string

  val of_string_40 : string -> t

  val with_human_readable    : t -> human_readable:string -> t
  val without_human_readable : t -> t
  val human_readable         : t -> string option

  module Stable : sig
    module V1 : Stable_without_comparator with type t = t
  end
end

module Tag : sig
  type t [@@deriving sexp_of]

  include Comparable.S_plain with type t := t
  include Stringable with type t := t

  val is_nice_name : t -> for_tree_rooted_at:Feature_name.t -> bool

  module Stable : sig
    module V1 : Stable_without_comparator with type t = t
  end
end

module Bookmark : sig
  type t =
    | Feature of Feature_path.t
    | Release of Feature_path.t
  [@@deriving sexp_of]
end

module Revset : sig
  type t [@@deriving sexp_of]

  val of_string : string -> t
  val to_string : t -> string

  (** singletons *)
  val dot         : t
  val of_rev      : Rev.t          -> t
  val feature_tip : Feature_path.t -> t
  val bookmark    : Bookmark.t     -> t
  val tag         : Tag.t          -> t
  val all_tags    : t

  (** [for_feature ~base ~tip] returns the revisions that are, under the simple
      feature-tree model, unique to the feature with the given [base] and [tip].  I.e.
      [difference (ancestors tip) (ancestors base)]. *)
  val for_feature : base:Rev.t -> tip:Rev.t -> t

  val range : from:Rev.t -> to_:Rev.t -> t

  val ancestors   : t -> t
  val descendants : t -> t

  (** boolean operations *)
  val and_       : t list -> t
  val or_        : t list -> t
  val difference : t -> t -> t
  val not        : t -> t

  val first_greatest_common_ancestor : Rev.t list -> t

  val max : t -> t
  val min : t -> t

  val merge : t

  val limit : t -> limit:int -> t

  val reverse : t -> t
end

val create_rev
  :  ?human_readable:string
  -> Repo_root.t
  -> Revset.t
  -> Rev.t Or_error.t Deferred.t

val create_rev_zero : Repo_root.t -> Rev.t Deferred.t

val create_revs
  :  ?restrict_to:Path_in_repo.t
  -> Repo_root.t
  -> Revset.t
  -> Rev.t list Or_error.t Deferred.t

(** [active_bookmark] and [current_bookmark] correspond to Mercurial's notions of the
    same name.  Specifically:

    {v
      active_bookmark  <--> hg active-bookmark
      current_bookmark <--> .hg/bookmarks.current
    v}

    A bookmark is active if it is current and points to the revision of the working copy.
    So, if B is active, then B is current.  Also, if B1 is current and B2 is active, then
    B1 = B2.  [active_bookmark] and [current_bookmark] can only differ if a bookmark is
    current but there is no active bookmark.  This can happen if you pull and the active
    bookmark is moved forward -- after this there would be no active bookmark, but the
    previously active bookmark would still be current.

    [current_bookmark] is significantly faster, since it is just looking a file, while
    [active_bookmark] calls [hg], and takes about .3s.
*)
val active_bookmark  : Repo_root.t -> string Or_error.t Deferred.t
val current_bookmark : Repo_root.t -> string Or_error.t Deferred.t

val bookmark_exists : Repo_root.t -> Bookmark.t -> bool Deferred.t

val phase
  : Repo_root.t
  -> [ `Rev     of Rev.t
     | `Feature of Feature_path.t
     ]
  -> [ `Public
     | `Draft
     | `Secret
     ] Deferred.t

val cat_one
  :  Repo_root.t
  -> Rev.t
  -> Path_in_repo.t
  -> string Or_error.t Deferred.t

val cat
  :  Repo_root.t
  -> Rev.t
  -> Path_in_repo.t list
  -> dir:Abspath.t
  -> Abspath.t Path_in_repo.Map.t Deferred.t

module Scaffold : sig
  val resolve_revision
    :  Remote_repo_path.t
    -> revision:string
    -> scaffold_requires_global_tag_or_rev_hash:bool
    -> string Or_error.t Deferred.t
end

(** [cat_from_scaffold repo_root path ~scaffold_requires_global_tag_or_rev_hash] reads
    [repo_root ^ "/scaffold.sexp"], finds the revision and location of the outermost
    repository, and then 'hg cat's [path] from that revision in that repository. *)
val cat_from_scaffold
  :  Repo_root.t
  -> Path_in_repo.t
  -> scaffold_requires_global_tag_or_rev_hash:bool
  -> [ `No_scaffold     of Relpath.t
     | `Scaffold_exists of string Or_error.t
     ] Deferred.t

(** The [key, value] pairs of [metadata] will be added to the metadata in the changeset,
    which is accessible from python hg extensions and viewable with, e.g., [hg log
    --debug]. *)
val commit
  :  ?metadata:string String.Map.t
  -> Repo_root.t
  -> message:string
  -> unit Or_error.t Deferred.t

val first_greatest_common_ancestor : Repo_root.t -> Rev.t -> Rev.t -> Rev.t Deferred.t

val greatest_common_ancestor  : Repo_root.t -> Rev.t -> Rev.t -> Rev.t      Deferred.t
val greatest_common_ancestors : Repo_root.t -> Rev.t -> Rev.t -> Rev.t list Deferred.t

val is_ancestor : Repo_root.t -> ancestor:Rev.t -> descendant:Rev.t -> bool Deferred.t

module Cleanliness_witness : sig
  (** A [Cleanliness_witness.t] is received for a repo when it has an empty [hg status].
      If you have a [Cleanliness_witness.t] and only interact with the repo via the
      functions in this module, the hg status will probably continue to be empty.

      A [Cleanliness_witness.t] is not a perfect guarantee that the status is empty since:

      1. Someone else could interact with the repo.
      2. Functions in this module (e.g., [rebase]) could dirty the status while running,
      although they should leave it clean at the end.
      3. You could use a [Cleanliness_witness.t] generated from one repo with another
      repo.
  *)
  type t = private Repo_is_clean
end

(** [Ok witness] is clean; [Error] if unclean; raise if check fails. *)
val status_cleanliness
  :  ?repo_is_clean:Cleanliness_witness.t
  -> Repo_root.t
  -> Cleanliness_witness.t Or_error.t Deferred.t

val diff
  :  Repo_root.t
  -> from:Rev.t
  -> to_:[ `Working_copy | `Rev of Rev.t ]
  -> args:string list
  -> string Or_error.t Deferred.t

val distclean
  :  Repo_root.t
  -> unit Or_error.t Deferred.t

val get_remote_rev
  : Remote_repo_path.t
  -> [ `Numeric_rev of int
     | `Feature of Feature_path.t
     ]
  -> Node_hash.First_12.t Deferred.t

val ensure_can_access_remote_repo : Remote_repo_path.t -> unit Deferred.t

val ensure_local_repo_is_in_family
  :  ?remote_rev_zero:Rev.t
  -> Repo_root.t
  -> Remote_repo_path.t
  -> unit Deferred.t

val grep_conflicts_exn
  : ?below:Path_in_repo.t  (** default is [Path_in_repo.root] *)
  -> Repo_root.t
  -> grep_display_option:string
  -> string Deferred.t

val is_conflict_free_exn : Repo_root.t -> bool Deferred.t

val infer_tag
  :  Repo_root.t
  -> root : Feature_name.t
  -> Rev.t
  -> Rev.t option Deferred.t

val list_bookmarks
  :  Repo_root.t
  -> string list Deferred.t

val files
  :  ?include_ : Path_in_repo.t
  -> Repo_root.t
  -> Path_in_repo.t list Deferred.t

val log
  :  ?args     : string list
  -> ?template : string
  -> Repo_root.t
  -> Revset.t
  -> string Or_error.t Deferred.t

val manifest
  :  Repo_root.t
  -> [ `Dirstate | `Revset of Revset.t ]
  -> Path_in_repo.t list Deferred.t

val outgoing
  :  Repo_root.t
  -> [ `Feature of Feature_path.t
     ]
  -> to_ : Remote_repo_path.t
  -> [ `Nothing | `Hg_out of string ] Or_error.t Deferred.t

val parent : Repo_root.t -> Rev.t Deferred.t

val pull
  :  ?even_if_unclean:bool  (** default is [false] *)
  -> ?repo_is_clean:Cleanliness_witness.t
  -> Repo_root.t
  -> from:Remote_repo_path.t
  -> [ `Rev       of Rev.t
     | `Revs      of Rev.t list
     | `Revset    of Revset.t
     | `Bookmarks of Bookmark.t list
     | `Feature   of Feature_path.t
     | `Features  of Feature_path.t list
     | `All_revs
     ]
  -> unit Deferred.t

val push
  :  Repo_root.t
  -> Bookmark.t list
  -> to_                : Remote_repo_path.t
  -> overwrite_bookmark : bool
  -> unit Or_error.t Deferred.t

val rebase
  :  ?merge_tool    : Merge_tool.t  (** default is [/usr/bin/merge -A] *)
  -> ?repo_is_clean : Cleanliness_witness.t
  -> ?abort_on_merge_conflicts: bool
  -> ?post_merge_validation_hook:(unit -> unit Or_error.t Deferred.t)
  -> Repo_root.t
  -> Feature_path.t
  -> feature_id     : Feature_id.t  (** for commit metadata *)
  -> old_tip        : Rev.t
  -> old_base       : Rev.t
  -> new_base       : Rev.t
  -> unit Or_error.t Deferred.t

val rename
  :  ?repo_is_clean:Cleanliness_witness.t
  -> Repo_root.t
  -> Remote_repo_path.t
  -> Rename.t list
  -> unit Deferred.t

val revs_exist : Repo_root.t -> Rev.t list -> bool Deferred.t
val rev_exists : Repo_root.t -> Rev.t      -> bool Deferred.t

val delete_bookmarks
  :  Repo_root.t
  -> Bookmark.t list
  -> [ `Do_not_push
     | `Push_to of Remote_repo_path.t
     ]
  -> unit Deferred.t

val set_bookmark
  :  Repo_root.t
  -> Bookmark.t
  -> to_:[ `Rev     of Rev.t
         | `Feature of Feature_path.t
         ]
  -> [ `Do_not_push
     | `Push_to_and_advance   of Remote_repo_path.t
     | `Push_to_and_overwrite of Remote_repo_path.t
     ]
  -> unit Deferred.t

(** [share] and [clone] create their destination atomically *)

val share
  :  Repo_root.t
  -> dst_repo_root_abspath__delete_if_exists : Abspath.t
  -> Repo_root.t Or_error.t Deferred.t

val with_temp_share :
  ?in_dir : Abspath.t
  -> Repo_root.t
  -> f:(Repo_root.t -> 'a Deferred.t)
  -> 'a Deferred.t

(** [hg clone -U]. *)
val clone
  :  Remote_repo_path.t
  -> dst_repo_root_abspath__delete_if_exists : Abspath.t
  -> Repo_root.t Or_error.t Deferred.t

val create_bookmark_and_update_to_it
  :  ?repo_is_clean:Cleanliness_witness.t
  -> Repo_root.t
  -> Remote_repo_path.t
  -> Feature_path.t
  -> Rev.t
  -> unit Deferred.t

val tags : Repo_root.t -> Rev.t -> Tag.t list Or_error.t Deferred.t

module Status : sig
  type t =
    | Added    of Path_in_repo.t
    | Removed  of Path_in_repo.t
    | Modified of Path_in_repo.t
    | Copied   of copied
  and copied = { src : Path_in_repo.t; dst : Path_in_repo.t }
  [@@deriving sexp_of]

  val src_path_in_repo : t list -> Path_in_repo.t list
  val dst_path_in_repo : t list -> Path_in_repo.t list

  module Changed : sig
    type t =
      | Between of
          { src : Rev.t
          ; dst : [ `Working_copy | `Rev of Rev.t ]
          }
      | Changed_by of Rev.t
  end
end

(** Doesn't show missing, unknown and ignored files. *)
val status
  :  Repo_root.t
  -> Status.Changed.t
  -> Status.t list Deferred.t

module Shelve_description : sig
  type t [@@deriving sexp_of]
end

val list_shelves_exn : Repo_root.t -> Shelve_description.t list Deferred.t

module Clean_after_update : sig
  (** When a user updates a tree across a change to the .hgignore (for instance when there
      is a directory renaming), files that used to be ignored but aren't anymore end up
      polluting the [hg status] as untracked files, causing subsequent commands to fail if
      they require the tree to be in a clean state.  To avoid this, if the [update] is
      executed from a clean state, it is safe to clean the repository after the update to
      get rid of these untracked files (assuming no one is changing files
      concurrently). *)
  type t =
    | Yes of Cleanliness_witness.t
    | No
end

(** When updating to [`Rev], no bookmark will be active, so make sure [`Feature] is not
    what you want. *)
val update
  :  ?discard_uncommitted_changes : bool  (** default is [false] *)
  -> clean_after_update:Clean_after_update.t
  -> Repo_root.t
  -> [ `Rev of Rev.t
     | `Revset  of Revset.t
     | `Feature of Feature_path.t
     ]
  -> unit Deferred.t

val whats_new
  : Repo_root.t -> from:Rev.t -> to_:Rev.t -> args:string list -> unit Deferred.t

val hg_path_command : Command.t

val hg_command : Command.t
