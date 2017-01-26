module Stable_format = struct
  open Core.Core_stable

  module V1 = Uuid.V1
end

open Core
open! Import

module T : sig
  type t = private Uuid.t [@@deriving sexp_of]
  val unshared_t : t -> Uuid.t
  val shared_t   : Uuid.t -> t
  include Comparable  with type t := t
  include Hashable    with type t := t
end = struct
  module Unshared = struct
    include Uuid
    let module_name = "Iron_common.Uuid"
  end
  include Hash_consing.Make (Unshared) ()
  (* We do this instead of recreating Hashable and Comparable from V1 in case there are
     some optimization in Uuid to speed up these operations. *)
  include (Uuid : sig
             include Comparable  with type t := t
             include Hashable    with type t := t
           end)
end

include T

let invariant t    = Uuid.invariant (unshared_t t)
let to_string t    = Uuid.to_string (unshared_t t)
let of_string str  = Uuid.of_string str |> shared_t
let create ()      = Uuid.create () |> shared_t
let to_file_name t = File_name.of_string (to_string t)

let length_of_string_repr =
  String.length (Core.Uuid.to_string Core.Uuid.Stable.V1.for_testing)
;;

module Stable = struct
  module V1 = struct
    include Make_stable.Of_stable_format.V1 (Stable_format.V1) (struct
        type nonrec t = t [@@deriving compare]
        let to_stable_format = T.unshared_t
        let of_stable_format = T.shared_t
      end)

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
    ;;
  end
end
