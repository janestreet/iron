open! Core
open Import

(* Dashes are fine, but not at the front of the name, otherwise [hg bookmark $book]
   wouldn't mean the right thing.  Also disallowing dots at the front, so we can create
   files starting with the name of the feature without having hidden files.  In theory,
   we could still have troubles because a bookmark d901bd97ca00 shadows the node with
   the same name.  In practice, when using 40 characters nodes, we're fine unless
   someone is intentionally trying to break fe.  The proper way of handling that would
   be to wrap nodes with [id("...")] everywhere where a revset is accepted. *)
let unanchored_regex_pattern = "[a-zA-Z0-9_][a-zA-Z0-9._-]*"

let regex = Regex.create_exn (concat [ "^"; unanchored_regex_pattern; "$" ])

include Validated_string.Make_regex (struct
    let module_name = "Iron_common.Feature_name"
    let regex = regex
  end) ()

let%test_unit _ =
  List.iter
    [ "foo-bar"
    ; "Core.plop"
    ]
    ~f:(fun string ->
      [%test_pred: t Or_error.t] is_ok
        (Or_error.try_with (fun () -> of_string string)))
;;

let to_file_name t = File_name.of_string (to_string t)

(* TEST = not (matches "-bar")
 * TEST =
 * TEST = not (matches ".plop")
 * TEST = not (matches "a\\")
 * TEST = not (matches "") *)
