open Core
open Import

include Validated_string_intf

module Make (M : sig
    val module_name : string
    val check_valid : string -> unit Or_error.t
  end) () : S = struct

  open M

  module Stable = struct
    module V1 = struct

      module T = struct
        module Unshared = struct
          type t = string [@@deriving bin_io, compare, sexp]
          let hash = String.hash
          let module_name = module_name
        end
        include Hash_consing.Stable.Make_stable_private (Unshared) ()

        let to_string (t : t) = (t :> string)

        let of_string string =
          match check_valid string with
          | Ok () -> shared_t string
          | Error error ->
            raise_s [%sexp "invalid", (module_name : string)
                           , (string : string), (error : Error.t)];
        ;;
      end

      module Unstable = Identifiable.Make (struct
          include T
          include Binable. Of_stringable (T)
          include Sexpable.Of_stringable (T)
        end)

      module T_with_comparator = struct
        include T
        type comparator_witness = Unstable.comparator_witness
        let comparator = Unstable.comparator
      end

      include T
      module Comparable = Core.Core_stable.Comparable.V1.Make(T_with_comparator)
      module Map = struct
        include Comparable.Map
        let hash hash_data = Hash_consing.map_hash T.hash hash_data
      end
      module Set = struct
        module T = struct
          include Comparable.Set
          let module_name = M.module_name ^ ".Set"
          let hash (t:t) = Hash_consing.set_hash T.hash t
        end
        include T
        include Hash_consing.Stable.Make_stable_public (T) ()
      end
      include Core.Core_stable.Hashable.V1.Make(T_with_comparator)
    end
  end

  include Stable.V1.T
  include Stable.V1.Unstable

  let invariant (t : t) = assert (is_ok (check_valid (t :> string)))

end

module Make_regex (M : sig
    val module_name : string
    val regex : Regex.t
  end) () =
  Make (struct

    include M

    let check_valid string =
      if Regex.matches regex string
      then Ok ()
      else error "does not match regex" regex [%sexp_of: Regex.t]
    ;;
  end) ()

module For_testing = struct

  include Make_regex (struct
      let module_name = "For_testing"
      let regex = Regex.create_exn ""
    end) ()

  (* This test can't go into the Stable module above, because that's in the functor and
     expect tests must be run in the file where they are defined. *)
  let%expect_test _ =
    print_endline [%bin_digest: t];
    [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
  ;;

end
