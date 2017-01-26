open! Core
open Iron_common.Std

type t

(** diff and ddiff format and layout *)
val diff_config        : t -> Format_rules.t
val ddiff_inner_config : t -> Format_rules.t
val ddiff_outer_config : t -> Format_rules.t

val view_configuration : t -> Diff_algo_id.t list Diff4_class.Map.t
val default_view_configuration : Diff_algo_id.t list Diff4_class.Map.t

include Iron_common.Std.Make_client_config.S with type t := t
