open Core
open Import

let fnmatch = Core.Unix.fnmatch

module T = struct
  type t = string
  [@@deriving bin_io, compare]

  let of_string glob =
    let fail msg = raise_s [%sexp (msg : string), (glob : string)] in
    (if String.contains glob '/'
     then fail "glob may not contain / char."
     else if String.contains glob '\000'
     then fail "glob may not contain nul char."
     (* Syntax-check the glob pattern by trying it in a trivial match and seeing if it
        throws an error. *)
     else (
       try ignore (fnmatch ~pat:glob "" : bool)
       with exn ->
         raise_s [%sexp "invalid glob", (glob : string), (exn : exn)]));
    glob
  ;;

  let to_string = Fn.id
  let module_name = "Iron_obligations.Simple_glob"
  let hash = String.hash
end

include T
include Identifiable.Make
    (struct
      include T
      include Sexpable.Of_stringable (T)
    end)

let star = of_string "*"

let matches pat s = fnmatch ~pat (File_name.to_string s) ~flags:[ `Period ]
