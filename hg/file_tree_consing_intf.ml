open Core
open Import

module type S = sig

  (** [t] is a type used on the server to aggregate data for files within a given
      directory.  This module uses a hash consing on the path in repos and directory
      contents in order to obtain memory sharing across multiple values of type
      [Obligations.t] or [CRs] sent by the workers.  The goal is to limit the memory
      growth of the server cache and still allow the number of revisions/features cached
      to be significant. *)

  type t [@@deriving sexp_of]
  type data

  include Invariant.S with type t := t

  val module_name : string

  val of_alist : (Path_in_repo.t * data) list -> t
  val to_alist : t -> (Path_in_repo.t * data) list

  val fold : t -> init:'a -> f:(key:Path_in_repo.t -> data:data -> 'a -> 'a) -> 'a
end

type cr_comments = Cr_comment.t list
type cr_soons    = Cr_soon.t    list

module type File_tree_consing = sig
  module Cr_comments : S with type data := cr_comments
  module Cr_soons    : S with type data := cr_soons
  module Obligations : S with type data := Review_attributes.t
end
