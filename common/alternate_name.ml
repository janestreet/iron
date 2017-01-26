open! Core
open! Import

include Validated_string.Make_regex (struct
    let module_name = "Iron_common.Alternate_name"
    let regex = Regex.user_name
  end) ()

let to_unresolved_name t = Unresolved_name.of_string (to_string t)
