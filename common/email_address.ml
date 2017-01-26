open! Core
open Import

include Validated_string.Make_regex (struct
    let module_name = "Iron_common.Email_address"
    let regex = Regex.user_name
  end) ()

let of_user_name user_name = of_string (User_name.to_string user_name)
