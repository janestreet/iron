open! Core
open Import

include Validated_string.Make_regex (struct
    let module_name = "Iron_common.Scrutiny_name"
    let regex = Regex.create_exn ".*"
  end) ()

let ignored = of_string "ignore"
