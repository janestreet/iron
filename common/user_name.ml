open! Core.Std
open Import

include Validated_string.Make_regex (struct
    let module_name = "Iron_common.User_name"
    let regex = Regex.user_name
  end) ()

let unix_login =
  of_string
    (if am_functional_testing
     then (
       match Sys.getenv "IRON_USER" with
       | None -> "unix-login-for-testing"
       | Some s -> s)
     else Core.Std.Unix.getlogin ())
;;

let missing_file_owner = of_string "missing-file-owner"

let synthetic_whole_feature_reviewer = of_string "synthetic-whole-feature-reviewer"

let to_file_name t = File_name.of_string (to_string t)

let to_unresolved_name t = Unresolved_name.of_string (to_string t)
