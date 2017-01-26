open! Core
open Import

include Validated_string.Make_regex (struct
    let module_name = "Iron_common.Group_name"
    let regex = Regex.create_exn "^[a-zA-Z0-9_][a-zA-Z0-9._-]*$"
  end) ()
