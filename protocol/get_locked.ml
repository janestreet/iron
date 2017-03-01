module Stable = struct

  open! Import_stable

  module Locked = Feature.Stable.Locked

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 82e9dd83ba682394b6a7532a1bdf9e67 |}]
      ;;

      let to_model t = t
    end

    module Model = V1
  end

  module Reaction = struct
    module V5 = struct
      type t = (Lock_name.V3.t * Locked.V2.t list) list
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f3da784e2752d80f71838448f07b267c |}]
      ;;

      let of_model m = m
    end

    module V4 = struct
      type t = (Lock_name.V2.t * Locked.V2.t list) list
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| dab5bace62ecebe12a03fee8540ac78e |}]
      ;;

      open! Core
      open! Import

      let of_model m =
        List.filter_map (V5.of_model m) ~f:(fun (lock_name, locks) ->
          Option.map (Lock_name.Stable.V2.of_v3 lock_name) ~f:(fun lock -> lock, locks))
      ;;
    end

    module V3 = struct
      type t = (Lock_name.V1.t * Locked.V2.t list) list
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| cc1252937498d7815d0fbd1eca2f9e0d |}]
      ;;

      open! Core
      open! Import

      let of_model m =
        List.filter_map (V4.of_model m) ~f:(fun (lock_name, locks) ->
          Option.map (Lock_name.Stable.V1.of_v2 lock_name) ~f:(fun lock -> lock, locks))
      ;;
    end

    module V2 = struct
      type t = (Lock_name.V1.t * Locked.V1.t list) list
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| bc9fe92038843321a05ef0e7877df030 |}]
      ;;

      let of_model m =
        List.map (V3.of_model m) ~f:(fun (lock_name, locks) ->
          lock_name, List.map locks ~f:Locked.V1.of_v2)
      ;;
    end

    module Model = V5
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "get-locked" end)
    (struct let version = 5 end)
    (Stable.Action.V1)
    (Stable.Reaction.V5)

include Register_old_rpc
    (struct let version = 4 end)
    (Stable.Action.V1)
    (Stable.Reaction.V4)

include Register_old_rpc
    (struct let version = 3 end)
    (Stable.Action.V1)
    (Stable.Reaction.V3)

include Register_old_rpc
    (struct let version = 2 end)
    (Stable.Action.V1)
    (Stable.Reaction.V2)

open! Core
open! Import

module Locked   = Feature.Stable.Locked. Model
module Action   = Stable.Action.         Model

module Reaction = struct
  include Stable.Reaction.Model

  let sort t =
    t
    |> List.sort ~cmp:(fun (lock_name1, _) (lock_name2, _) ->
      Lock_name.compare lock_name1 lock_name2)
    |> List.map ~f:(fun (lock_name, locks) ->
      lock_name, List.sort locks ~cmp:(fun (locked1 : Locked.t) locked2 ->
        User_name.compare locked1.by locked2.by))
  ;;
end
