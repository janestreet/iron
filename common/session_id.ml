open! Core
open! Import

include Iron_uuid

let no_session_error ~supplied:supplied_id =
  Error.create_s [%sexp "no current session", { supplied_id : t }]
;;

let check ~actual ~supplied =
  if equal actual supplied
  then Ok ()
  else
    error_s
      [%sexp
        "incorrect review-session id",
        { actual   : t
        ; supplied : t
        }
      ]
;;
