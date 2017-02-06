open! Core
open! Async
open! Iron_common.Std

let default_view_configuration =
  Diff4_class.Map.of_alist_exn
    [ `b1_f2    , [ `feature_ddiff ]
    ; `b2_f2    , [ `feature_ddiff ]
    ; `f1_f2    , [ `feature_ddiff ]
    ; `conflict , [ `feature_ddiff ]
    ]
;;

let make_view_configuration () =
  default_view_configuration
  |> Map.to_alist
  |> Diff4_class.Table.of_alist_exn
;;

let dot_patdiff  = ".patdiff"
let dot_patdiff4 = ".patdiff4"

module Dot_patdiff = struct

  type t =
    { diff_config : Format_rules.t
    }
  [@@deriving fields]

  let default_diff_config = Format_rules.inner_default
  ;;

  let get () =
    let diff_config =
      let module Patdiff_config = Patdiff_lib.Configuration in
      let config =
        Option.try_with (fun () ->
          Option.bind (Core.Sys.getenv "HOME") ~f:(fun home ->
            match Core.Sys.file_exists (home ^/ dot_patdiff4) with
            | `Yes | `Unknown -> None
            | `No ->
              Patdiff_config.load ~quiet_errors:true (home ^/ dot_patdiff)))
        |> Option.join
      in
      match config with
      | Some config -> config.rules
      | None -> default_diff_config
    in
    { diff_config }
  ;;
end

module M = struct
  let always_loaded_if_present = []
  let home_basename = dot_patdiff4

  type t =
    { mutable diff_config        : Format_rules.t
    ; mutable ddiff_inner_config : Format_rules.t
    ; mutable ddiff_outer_config : Format_rules.t
    ; mutable view_configuration : Diff_algo_id.t list Diff4_class.Table.t
    }
  [@@deriving fields]

  module Statement = struct
    type t =
      [ `set_diff_config        of Format_rules.t
      | `set_ddiff_inner_config of Format_rules.t
      | `set_ddiff_outer_config of Format_rules.t
      | `view_configuration of Diff4_class.t * Diff_algo_id.t list
      ]
    [@@deriving of_sexp]
  end

  let create () =
    let dot_patdiff = Dot_patdiff.get () in
    { diff_config           = Dot_patdiff.diff_config dot_patdiff
    ; ddiff_inner_config    = Format_rules.inner_default
    ; ddiff_outer_config    = Format_rules.outer_default
    ; view_configuration    = make_view_configuration ()
    }
  ;;

  let update t = function
    | `set_diff_config        rules -> t.diff_config        <- rules
    | `set_ddiff_inner_config rules -> t.ddiff_inner_config <- rules
    | `set_ddiff_outer_config rules -> t.ddiff_outer_config <- rules
    | `view_configuration (diff4_class, diff_algo_ids) ->
      Hashtbl.set t.view_configuration ~key:diff4_class ~data:diff_algo_ids
  ;;
end

include M
include Iron_common.Std.Make_client_config.Make(M)

let view_configuration t =
  t.view_configuration
  |> Hashtbl.to_alist
  |> Diff4_class.Map.of_alist_exn
;;
