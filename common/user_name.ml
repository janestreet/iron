module User_name = Validated_string.Make_regex (struct
    let module_name = "Iron_common.User_name"
    let regex = Import.Regex.user_name
  end) ()

module Stable = struct

  module Or_all = struct
    module V1 = struct
      type t =
        [ `All_users
        | `User of User_name.Stable.V1.t
        ]
      [@@deriving bin_io, compare, sexp]
    end

    module Model = V1
  end

  module Or_all_or_all_but = struct
    module V1 = struct
      type t =
        [ `All_users_but of User_name.Stable.V1.Set.t
        | Or_all.V1.t
        ]
      [@@deriving bin_io, compare, sexp]
    end

    module Model = V1
  end

  include User_name.Stable
end

open! Core
open! Import

include (User_name : module type of struct include User_name end
         with module Stable := Stable)

let unix_login =
  of_string
    (if am_functional_testing
     then (
       match Sys.getenv "IRON_USER" with
       | None -> "unix-login-for-testing"
       | Some s -> s)
     else Core.Unix.getlogin ())
;;

let missing_file_owner = of_string "missing-file-owner"

let synthetic_whole_feature_reviewer = of_string "synthetic-whole-feature-reviewer"

let to_file_name t = File_name.of_string (to_string t)

let to_unresolved_name t = Unresolved_name.of_string (to_string t)

module Or_all = struct
  include Stable.Or_all.Model

  let all_as_str = "all"

  let of_string str =
    if String.equal all_as_str str
    then `All_users
    else `User (User_name.of_string str)
  ;;

  let to_string = function
    | `All_users -> all_as_str
    | `User user -> User_name.to_string user
  ;;

  let arg_doc = "USER|" ^ to_string `All_users

  let arg_type ~complete_user_name =
    Command.Arg_type.create of_string ~complete:(fun hmap ~part ->
      (if String.is_prefix ~prefix:part all_as_str
       then [ all_as_str ]
       else [])
      @ complete_user_name hmap ~part)
  ;;
end

module Or_all_or_all_but = struct
  include Stable.Or_all_or_all_but.Model

  let mem t user_name =
    match (t : t) with
    | `All_users         -> true
    | `All_users_but set -> not (Set.mem set user_name)
    | `User u            -> User_name.equal u user_name
  ;;
end
