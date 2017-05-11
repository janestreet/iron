open! Core
open! Async
open! Import

module Feature : sig
  type t =
    { feature_id   : Feature_id.t
    ; feature_path : Feature_path.t
    }
  [@@deriving compare, sexp_of]
end

module Kind : sig
  type t =
    [ `Feature of Feature.t
    | `Satellite_repo
    | `Clone
    | `Fake_for_testing
    ]
  [@@deriving compare, sexp_of]
end

type t =
  { repo_root        : Repo_root.t
  ; remote_repo_path : Remote_repo_path.t
  ; kind             : Kind.t
  }
[@@deriving sexp_of]

(** [save t] writes the hgrc in [t.repo_root]. *)
val save : t -> unit Deferred.t

(** [extract_root_feature_from_hgrc repo_root], if possible, extracts a root feature name
    from a line in the the hgrc of [repo_root] that looks like:

    {[
      default = ssh://hg//hg/$ROOT/submissions
    ]}
*)
val extract_root_feature_from_hgrc : Repo_root.t -> Feature_name.t Or_error.t Deferred.t

module Info : sig
  type t = private
    { generated_by     : string (** Version_util.version *)
    ; remote_repo_path : Remote_repo_path.t
    ; kind             : Kind.t
    }
  [@@deriving compare, sexp_of]
end

val extract_info : Repo_root.t -> Info.t Or_error.t Deferred.t
