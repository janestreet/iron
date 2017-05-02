module Stable = struct

  open! Core.Core_stable
  open! Import_stable

  module First_12 = Node_hash.First_12.Stable

  module Rev_info = struct
    module V1 = struct
      type t =
        { first_12_of_rev     : First_12.V1.t
        ; rev_author_or_error : User_name.V1.t Or_error.V2.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 9328f6880f732cb029b0f0eecf487008 |}]
      ;;
    end
    module Model = V1
  end

  module Broken_info = struct
    module V1 = struct
      type t =
        { last_working : Rev_info.V1.t option
        ; first_broken : Rev_info.V1.t
        ; last_broken  : Rev_info.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| e8f4f7d624a6d870c86276d677a99281 |}]
      ;;
    end
    module Model = V1
  end

  module Finished_compilation = struct
    module V1 = struct
      type t =
        | Working of Rev_info.V1.t
        | Broken  of Broken_info.V1.t
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b5002cb2d9234e107dc8ab74969d36d8 |}]
      ;;
    end
    module Model = V1
  end

  module Hydra_compilation_status_for_repo_controller = struct
    module V1 = struct
      type t =
        { finished : Finished_compilation.V1.t option
        ; pending  : Rev_info.V1.t list
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 10892c034923c936e01c33202711d0e1 |}]
      ;;
    end
    module Model = V1
  end

  module Hydra_compilation_status = struct
    module V2 = struct
      type one = Hydra_compilation_status_for_repo_controller.V1.t =
        { finished : Finished_compilation.V1.t option
        ; pending  : Rev_info.V1.t list
        }
      and t = one Repo_controller_name.V1.Map.t
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| e0048e28ddf30942f146fa239bdbfbf9 |}]
      ;;
    end

    module V1 = struct
      type t = (string * Hydra_compilation_status_for_repo_controller.V1.t) list
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 155c7429e86d37591536a600c7b6ea1b |}]
      ;;
    end
    module Model = V2
  end

  module Compilation_status = struct
    module Pending_rev = struct
      module V1 = struct
        type t =
          { first_12_of_rev     : First_12.V1.t
          ; rev_author_or_error : User_name.V1.t Or_error.V2.t
          ; pending_since       : Time.V1_round_trippable.t
          }
        [@@deriving bin_io, compare, fields, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 68395b34acca4288b2a852b6b4a72d83 |}]
        ;;
      end
      module Model = V1
    end
    module V1 = struct
      type one =
        { finished : Finished_compilation.V1.t option
        ; pending  : Pending_rev.V1.t list
        }
      and t = one Repo_controller_name.V1.Map.t
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f7ad1a1586ea5c3cbe3b0d3a017c420e |}]
      ;;
    end
    module Model = V1
  end

  module V3 = struct
    type t =
      { bookmark                  : string
      ; rev_info                  : Rev_info.V1.t
      ; status                    : [ `Done              | `Pending_or_working_on_it ]
      ; continuous_release_status : [ `Not_working_on_it | `Pending_or_working_on_it ]
      ; compilation_status        : Hydra_compilation_status.V2.t
      }
    [@@deriving bin_io, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 32e753b898bda08b59523a86def9f9e0 |}]
    ;;
  end

  module V2 = struct
    type t =
      { bookmark                  : string
      ; rev_info                  : Rev_info.V1.t
      ; status                    : [ `Done              | `Pending_or_working_on_it ]
      ; continuous_release_status : [ `Not_working_on_it | `Pending_or_working_on_it ]
      ; compilation_status        : Hydra_compilation_status.V1.t
      }
    [@@deriving bin_io, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 930d214dd7eb41403530b00042f5f89a |}]
    ;;

    open! Core
    open! Import

    let to_v3 { bookmark
              ; rev_info
              ; status
              ; continuous_release_status
              ; compilation_status
              } =
      { V3.
        bookmark
      ; rev_info
      ; status
      ; continuous_release_status
      ; compilation_status =
          List.map ~f:(fun (repo_controller, compilation_status) ->
            Repo_controller_name.of_string repo_controller, compilation_status)
            compilation_status
          |> Repo_controller_name.Map.of_alist_reduce ~f:(fun first _ -> first)
      }
    ;;

    let of_v3 { V3.
                bookmark
              ; rev_info
              ; status
              ; continuous_release_status
              ; compilation_status
              } =
      { bookmark
      ; rev_info
      ; status
      ; continuous_release_status
      ; compilation_status =
          Map.to_alist compilation_status
          |> List.map ~f:(fun (repo_controller, compilation_status) ->
            Repo_controller_name.to_string repo_controller, compilation_status)
      }
    ;;

  end

  module V1 = struct
    type t =
      { bookmark            : string
      ; first_12_of_rev     : First_12.V1.t
      ; rev_author_or_error : User_name.V1.t Or_error.V1.t
      ; status              : [ `Done | `Pending_or_working_on_it ]
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 628d0b4b48a6047e9c10b74763eb770c |}]
    ;;

    let to_v2 { bookmark
              ; first_12_of_rev
              ; rev_author_or_error
              ; status
              } =
      { V2.
        bookmark
      ; rev_info                  = { first_12_of_rev; rev_author_or_error }
      ; status
      ; continuous_release_status = `Not_working_on_it
      ; compilation_status        = []
      }
    ;;

    let of_v2 { V2.
                bookmark
              ; rev_info = { first_12_of_rev; rev_author_or_error }
              ; status
              ; continuous_release_status = _
              ; compilation_status        = _
              } =
      { bookmark
      ; first_12_of_rev
      ; rev_author_or_error
      ; status
      }
    ;;
  end

  module Model = V3
end

open! Core
open! Import

module Rev_info = struct
  include Stable.Rev_info.Model

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~first_12_of_rev:(check Node_hash.First_12.invariant)
        ~rev_author_or_error:(check (Or_error.invariant User_name.invariant)))
  ;;
end

module Broken_info = struct
  include Stable.Broken_info.Model

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~last_working:(check (Option.invariant Rev_info.invariant))
        ~first_broken:(check Rev_info.invariant)
        ~last_broken: (check Rev_info.invariant))
  ;;
end

module Finished_compilation = struct
  include Stable.Finished_compilation.Model

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      match t with
      | Working rev_info -> Rev_info.invariant rev_info
      | Broken broken_info -> Broken_info.invariant broken_info)
  ;;
end

module Hydra_compilation_status = Stable.Hydra_compilation_status.Model

module Compilation_status = struct
  include Stable.Compilation_status.Model

  module Pending_rev = struct
    include Stable.Compilation_status.Pending_rev.Model

    let invariant t =
      Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
        let check f = Invariant.check_field t f in
        Fields.iter
          ~first_12_of_rev:(check Node_hash.First_12.invariant)
          ~rev_author_or_error:(check (Or_error.invariant User_name.invariant))
          ~pending_since:ignore)
    ;;
  end

  let invariant one =
    Invariant.invariant [%here] one [%sexp_of: one] (fun () ->
      let check f = Invariant.check_field one f in
      Fields_of_one.iter
        ~finished:(check (Option.invariant Finished_compilation.invariant))
        ~pending:(check (List.iter ~f:Pending_rev.invariant)))
  ;;

  let equal = [%compare.equal: one]
end

include Stable.Model
