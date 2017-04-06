open! Core
open! Import

module Regex = Re2.Regex

module T = struct
  type t = Regex.t
  [@@deriving bin_io, compare]

  let of_string s = Regex.create (String.concat ["\\A"; s; "\\z"]) |> ok_exn

  let to_string re =
    let s = Regex.pattern re in
    String.slice s 2 ((String.length s) - 2)
  ;;

  let module_name = "Iron_obligations.Whole_regexp"

  let hash r = String.hash (to_string r)
end

include T
include Identifiable.Make
    (struct
      include T
      include Sexpable.Of_stringable (T)
    end)

let matches = Regex.matches
let rewrite = Regex.rewrite
let valid_rewrite_template = Regex.valid_rewrite_template
