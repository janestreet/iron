open! Core
open! Async

module String      = Iron_string
module Time        = Iron_time
module Clock       = Clock_ns
module Clock_ns    = struct end

include Stable_module_types

module Make_stable = Core.Core_stable.Make_stable

let concat = String.concat

let program_started_at = Time.now ()

let print_elapsed =
  fun here ->
    if false then
      Debug.eprint_s
        [%sexp
          [ "elapsed"
          ; (Source_code_position.to_string here        : string)
          ; (Time.diff (Time.now ()) program_started_at : Time.Span.t)
          ]
        ]
;;

let sec f = Time.Span.of_sec f

include Int.Replace_polymorphic_compare

module Regex = struct
  include Re2.Regex

  let user_name = create_exn "^[a-zA-Z0-9_][a-zA-Z0-9._-]*$"
end

(* For use with custom_printf *)
let sh = Filename.quote (* shell escape *)

module type Key = sig
  type t [@@deriving compare, sexp_of]
  val hash : t -> int
end

let error_string = Or_error.error_string

let stdin  = Reader.stdin
let stdout = Writer.stdout
let stderr = Writer.stderr

let am_functional_testing = is_some (Sys.getenv "IRON_FUNCTIONAL_TESTING")

let how_long ~since = Time.diff (Time.now ()) since

module Int = struct
  include Int
  let to_string_hum t = to_string_hum t ~delimiter:','
end

let alist_of_iter iter =
  let accum = ref [] in
  iter ~f:(fun a b -> accum := (a, b) :: !accum);
  List.rev !accum
;;

let list_of_iter iter =
  let accum = ref [] in
  iter ~f:(fun a -> accum := a :: !accum);
  List.rev !accum
;;

let lazy_deferred f =
  let lazy_def = Lazy_deferred.create f in
  fun () -> Lazy_deferred.force_exn lazy_def
;;

module Sexp = struct
  include Sexp

  let list_of_string_conv_exn str conv =
    Sexp.of_string_conv_exn (String.concat [ "("; str; ")" ]) conv
  ;;
end

module Structurally_comparable = struct
  module type S = sig
    type t
    module Structural_compare : sig
      type nonrec t = t [@@deriving sexp_of, compare]
      include Equal.S with type t := t
    end
    val compare : [ `Compare_is_too_fragile ]
  end

  (* Hides the compare function for a type and instead makes it available in a submodule.
     The idiom is that if the comparison for type calls a structural comparison for an
     other type, this functor should be called on the larger type.  This is used when a
     comparison function can be defined structurally, but it is fragile (for instance
     comparing list where the order might change depending on how the value was built) so
     it should be checked at the call site whether the comparison makes sense in this
     context. *)
  module Make(T : sig type t [@@deriving sexp_of, compare] end)
    : S with type t := T.t = struct
    module Structural_compare = struct
      include T
      let equal t1 t2 = compare t1 t2 = 0
    end
    let compare = `Compare_is_too_fragile
  end
end

let (>>>) = `Deprecated_in_iron__Use_let_syntax_instead
let exit  = `Deprecated_in_iron__Use_shutdown_dot_exit_instead
let failwiths = `Deprecated_in_iron__Use_raise_s

let () = print_elapsed [%here]
