module Stable = struct

  open! Import_stable

  module Action = struct
    module V4 = struct
      type t =
        { feature_path      : Feature_path.V1.t
        ; for_              : User_name.V1.t
        ; lock_names        : Lock_name.V3.t list
        ; even_if_permanent : bool
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ec4e58665ea03af8ca660acba1563c24 |}]
      ;;

      let to_model t = t
    end

    module V3 = struct
      type t =
        { feature_path      : Feature_path.V1.t
        ; for_              : User_name.V1.t
        ; lock_names        : Lock_name.V2.t list
        ; even_if_permanent : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 55f8e60cd5f73564afa848d4afc75e68 |}]
      ;;

      let to_model { feature_path
                   ; for_
                   ; lock_names
                   ; even_if_permanent
                   } =
        V4.to_model
          { V4.
            feature_path
          ; for_
          ; lock_names = List.map lock_names ~f:Lock_name.V2.to_v3
          ; even_if_permanent
          }
      ;;
    end

    module V2 = struct
      type t =
        { feature_path      : Feature_path.V1.t
        ; for_              : User_name.V1.t
        ; lock_names        : Lock_name.V1.t list
        ; even_if_permanent : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 846f97cc2a68788b923ffba9546cb57b |}]
      ;;

      let to_model { feature_path
                   ; for_
                   ; lock_names
                   ; even_if_permanent
                   } =
        V3.to_model
          { V3.
            feature_path
          ; for_
          ; lock_names = List.map lock_names ~f:Lock_name.V1.to_v2
          ; even_if_permanent
          }
      ;;
    end

    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; for_         : User_name.V1.t
        ; lock_names   : Lock_name.V1.t list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f8f9ca14b4c067d234e72e68f778b5a4 |}]
      ;;

      let to_model { feature_path
                   ; for_
                   ; lock_names
                   } =
        V2.to_model
          { V2.
            feature_path
          ; for_
          ; lock_names
          ; even_if_permanent = false
          }
      ;;
    end

    module Model = V4
  end

  module Reaction = struct
    module V3 = struct
      type t = (Lock_name.V3.t * unit Or_error.V2.t) list
      [@@deriving bin_io, sexp]

      let of_model t = t
    end

    module V2 = struct
      type t = (Lock_name.V2.t * unit Or_error.V1.t) list
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 1b81894aede6396f94500462b05ac68b |}]
      ;;

      open! Core
      open! Import

      let of_model m =
        List.map (V3.of_model m) ~f:(fun (lock_name, result) ->
          let lock_name =
            match Lock_name.Stable.V2.of_v3 lock_name with
            | Some lock_name -> lock_name
            | None -> assert false (* reaction inconsistent with action *)
          in
          lock_name, result)
      ;;
    end

    module V1 = struct
      type t = (Lock_name.V1.t * unit Or_error.V1.t) list
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 20da5a7a85af7de038884d3287cd3bc1 |}]
      ;;

      open! Core
      open! Import

      let of_model m =
        List.map (V2.of_model m) ~f:(fun (lock_name, result) ->
          let lock_name =
            match Lock_name.Stable.V1.of_v2 lock_name with
            | Some lock_name -> lock_name
            | None -> assert false (* reaction inconsistent with action *)
          in
          lock_name, result)
      ;;
    end

    module Model = V3
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "unlock-feature" end)
    (struct let version = 4 end)
    (Stable.Action.V4)
    (Stable.Reaction.V3)

include Register_old_rpc
    (struct let version = 3 end)
    (Stable.Action.V3)
    (Stable.Reaction.V2)

include Register_old_rpc
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
