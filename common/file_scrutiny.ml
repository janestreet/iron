module Stable = struct
  open! Core.Core_stable

  module Unstable = struct
    module Scrutiny_level = Scrutiny_level
    module Scrutiny_name  = Scrutiny_name
  end

  module Scrutiny_level = Scrutiny_level.Stable
  module Scrutiny_name = Scrutiny_name.Stable

  module V1 = struct
    module Unshared = struct
      type t =
        { name  : Scrutiny_name.V1.t
        ; level : Scrutiny_level.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let module_name = "Iron_common.File_scrutiny"
      let hash { name; level } =
        Unstable.Scrutiny_level.hash level lxor Unstable.Scrutiny_name.hash name
      ;;
    end
    include Hash_consing.Stable.Make_stable_private (Unshared) ()

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 29e83e23aee13880d7cc45405949d110 |}]
    ;;

    (* The compare function on [t] does not distinguish names so that only name changes
       are not required to be reviewed if those are the only changes. *)
    let compare t1 t2 =
      Scrutiny_level.V1.compare (unshared_t t1).level (unshared_t t2).level
    ;;
  end
end

open! Core
open! Import

include Stable.V1
include Comparable.Make (Stable.V1)

let create name level = shared_t { name; level }

let to_string_hum t = Scrutiny_name.to_string (unshared_t t).name

let ignored =
  shared_t { name = Scrutiny_name.ignored; level = Scrutiny_level.ignored }
;;
