module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module Rev = Hg.Rev.Stable

  module V1 = struct
    type t =
      | Rev    of Rev.V1.t
      | String of string
    [@@deriving bin_io, compare, sexp, variants]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 9ecef85b2fd1735810b045f268d2f1a8 |}]
    ;;
  end

  module Model = V1
end

open! Core
open! Async
open! Import
open Hg

include Stable.Model

let resolve t ~in_:repo_root =
  match t with
  | Rev rev       -> return (Ok rev)
  | String string -> Hg.create_rev (ok_exn repo_root) (Revset.of_string string)
;;

let resolve_exn t ~in_ =
  resolve t ~in_ |> Deferred.map ~f:ok_exn
;;

let resolve_opt_exn t_opt ~in_ =
  match t_opt with
  | None   -> return None
  | Some t -> let%map rev = resolve_exn t ~in_ in Some rev
;;
