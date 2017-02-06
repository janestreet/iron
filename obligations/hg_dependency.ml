open Core
open Async
open Import

module type S = sig
  val cat_from_scaffold
    :  Repo_root.t
    -> Path_in_repo.t
    -> scaffold_requires_global_tag_or_rev_hash:bool
    -> [ `No_scaffold     of Relpath.t
       | `Scaffold_exists of string Or_error.t
       ] Deferred.t
end
