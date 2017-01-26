module Unstable = struct
  open! Import
  module Hash_consing       = Hash_consing
end

module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module V2 = struct
    module Unshared = struct
      type t =
        { build_projections                  : Build_projection_name.V1.Set.t
        ; tags                               : Tag.V1.Set.t
        ; fewer_than_min_reviewers           : bool
        ; followers                          : User_name.V1.Set.t
        ; is_read_by_whole_feature_reviewers : bool
        ; more_than_max_reviewers            : bool
        ; owner                              : User_name.V1.t
        ; review_obligation                  : Review_obligation.V1.t
        ; scrutiny_level                     : Scrutiny_level.V1.t
        ; scrutiny_name                      : Scrutiny_name.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let module_name = "Iron_obligations.Review_attributes"

      let hash t =
        let module Hash_consing = Unstable.Hash_consing in
        let hash field = Hash_consing.field t field in
        let bool b = Hashtbl.hash (b : bool) in
        Fields.fold
          ~init:Hash_consing.init
          ~build_projections:(hash Build_projection_name.V1.Set.hash)
          ~tags:(hash Tag.V1.Set.hash)
          ~fewer_than_min_reviewers:(hash bool)
          ~followers:(hash User_name.V1.Set.hash)
          ~is_read_by_whole_feature_reviewers:(hash bool)
          ~more_than_max_reviewers:(hash bool)
          ~owner:(hash User_name.V1.hash)
          ~review_obligation:(hash Review_obligation.V1.hash)
          ~scrutiny_level:(hash Scrutiny_level.V1.hash)
          ~scrutiny_name:(hash Scrutiny_name.V1.hash)
      ;;
    end
    include Unshared
    include Hash_consing.Make_stable_public (Unshared) ()

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| fb5829d095a3824b442650ef88e0ec09 |}]
    ;;
  end
  module Model = V2
end

open! Core
open! Import

module T = struct
  type t = Stable.Model.t =
    { build_projections                  : Build_projection_name.Set.t
    ; tags                               : Tag.Set.t [@sexp_drop_if Set.is_empty]
    ; fewer_than_min_reviewers           : bool
    ; followers                          : User_name.Set.t
    ; is_read_by_whole_feature_reviewers : bool
    ; more_than_max_reviewers            : bool
    ; owner                              : User_name.t
    ; review_obligation                  : Review_obligation.t
    ; scrutiny_level                     : Scrutiny_level.t
    ; scrutiny_name                      : Scrutiny_name.t
    }
  [@@deriving compare, fields, sexp_of]
end
include T
include Comparable.Make_plain (T)

let shared_t    = Stable.Model.shared_t
let hash        = Stable.Model.hash
let module_name = Stable.Model.module_name

let create
      ~build_projections
      ~tags
      ~fewer_than_min_reviewers
      ~followers
      ~is_read_by_whole_feature_reviewers
      ~more_than_max_reviewers
      ~owner
      ~review_obligation
      ~scrutiny_level
      ~scrutiny_name
  =
  shared_t
    { build_projections
    ; tags
    ; fewer_than_min_reviewers
    ; followers
    ; is_read_by_whole_feature_reviewers
    ; more_than_max_reviewers
    ; owner
    ; review_obligation
    ; scrutiny_level
    ; scrutiny_name
    }
;;

let with_review_obligation t ~review_obligation = shared_t { t with review_obligation }

let attribute_table t =
  let rows =
    let rows = ref [] in
    let f to_string field =
      let row = Field.name field, Field.get field t |> to_string in
      rows := row :: !rows
    in
    let set_to_string f set =
      set
      |> Core.Set.to_list
      |> List.map ~f
      |> String.concat ~sep:", "
    in
    let f_sexp sexp_of = f (fun t -> Sexp.to_string (sexp_of t)) in
    Fields.iter
      ~build_projections:(f (set_to_string Build_projection_name.to_string))
      ~tags:(f (set_to_string Tag.to_string))
      ~fewer_than_min_reviewers:(f Bool.to_string)
      ~followers:(f (set_to_string User_name.to_string))
      ~is_read_by_whole_feature_reviewers:(f Bool.to_string)
      ~more_than_max_reviewers:(f Bool.to_string)
      ~owner:(f User_name.to_string)
      ~review_obligation:(f_sexp [%sexp_of: Review_obligation.t])
      ~scrutiny_level:(f Scrutiny_level.to_string_hum)
      ~scrutiny_name:(f Scrutiny_name.to_string)
    ;
    List.rev !rows
    |> List.filter ~f:(fun (_, v) -> not (String.is_empty v))
    |> List.sort ~cmp:(fun (a, _) (b, _) -> String.compare a b)
  in
  let columns =
    Ascii_table.Column.(
      [ string ~header:"attribute" (cell fst)
      ; string ~header:"value"     (cell snd)
      ])
  in
  Ascii_table.create ~columns ~rows
;;

let for_testing =
  let scrutiny = Scrutiny.for_testing in
  let reviewers = List.map ~f:User_name.of_string ["jdoe"; "fbar"] in
  let min_file_reviewers = scrutiny.min_file_reviewers in
  let max_file_reviewers = scrutiny.max_file_reviewers in
  let more_than_max_reviewers  = Int.O.(List.length reviewers > max_file_reviewers) in
  let fewer_than_min_reviewers = Int.O.(List.length reviewers < min_file_reviewers) in
  create
    ~build_projections:Build_projection_name.Set.empty
    ~tags:Tag.Set.empty
    ~fewer_than_min_reviewers
    ~followers:User_name.Set.empty
    ~is_read_by_whole_feature_reviewers:scrutiny.read_by_whole_feature_reviewers
    ~more_than_max_reviewers
    ~owner:(User_name.of_string "jdoe")
    ~review_obligation:(Review_obligation.all_of (User_name.Set.of_list reviewers))
    ~scrutiny_level:scrutiny.level
    ~scrutiny_name:scrutiny.name
;;
