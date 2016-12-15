module Stable = struct

  open! Import_stable

  module Rev_info = struct
    module V1 = struct
      type t =
        { first_12_of_rev     : Node_hash.First_12.V1.t
        ; rev_author_or_error : User_name.V1.t Or_error.V2.t
        }
      [@@deriving bin_io, sexp]

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
      [@@deriving bin_io, sexp]

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
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b5002cb2d9234e107dc8ab74969d36d8 |}]
      ;;
    end
    module Model = V1
  end

  module Compilation_status = struct
    module V1 = struct
      type t =
        { finished : Finished_compilation.V1.t option
        ; pending  : Rev_info.V1.t list
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 10892c034923c936e01c33202711d0e1 |}]
      ;;
    end
    module Model = V1
  end

  module V2 = struct
    type t =
      { bookmark                  : string
      ; rev_info                  : Rev_info.V1.t
      ; status                    : [ `Done              | `Pending_or_working_on_it ]
      ; continuous_release_status : [ `Not_working_on_it | `Pending_or_working_on_it ]
      ; compilation_status        : (string * Compilation_status.V1.t) list
      }
    [@@deriving bin_io, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 930d214dd7eb41403530b00042f5f89a |}]
    ;;
  end

  module V1 = struct
    type t =
      { bookmark            : string
      ; first_12_of_rev     : Node_hash.First_12.V1.t
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

  module Model = V2
end

open! Core.Std
open! Import

module Broken_info          = Stable.Broken_info          .Model
module Compilation_status   = Stable.Compilation_status   .Model
module Finished_compilation = Stable.Finished_compilation .Model
module Rev_info             = Stable.Rev_info             .Model

include Stable.Model
