open! Core.Std
open! Import

include Validated_string.Make_regex (struct
    let module_name = "Iron_common.Unresolved_name"
    let regex = Regex.user_name
  end) ()

let ignored_file_virtuser_fe_compatibility =
  of_string "ignored-file-virtuser-fe-compatibility"
;;
