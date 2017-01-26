open! Core
open! Import

include Validated_string.Make_regex (struct
    let module_name = "Iron_common.Build_projection_name"
    let regex = Regex.user_name
  end) ()
