open! Core.Std
open! Async.Std
open! Import

(* Given a revision of a file with conflict markers, parses the conflict markers to find
   the revisions of the old base, new base, and old tip.
*)
val find_diamond
  :  ?verbose:bool
  -> repo_root:Repo_root.t
  -> path_in_repo:Path_in_repo.t
  -> new_tip_rev:Hg.Rev.t
  -> unit
  -> Hg.Rev.t Diamond.t Deferred.Option.t

(* If you know the revision of the "merged" result, you can use this function to walk
   backwards to find the revision with conflict markers, and use it to solve for the
   other three.
*)
val walk_to_find_diamond
  :  ?verbose:bool
  -> repo_root:Repo_root.t
  -> path_in_repo:Path_in_repo.t
  -> new_tip_rev:Hg.Rev.t
  -> unit
  -> Hg.Rev.t Diamond.t Deferred.Option.t

(* If you know the revision of a file which has conflict markers, you can use this
   function to find the first version that comes after it with the conflict markers
   resolved. *)
val find_merged_rev
  :  ?verbose:bool
  -> repo_root:Repo_root.t
  -> path_in_repo:Path_in_repo.t
  -> rev:Hg.Rev.t
  -> unit
  -> Hg.Rev.t Deferred.Option.t
