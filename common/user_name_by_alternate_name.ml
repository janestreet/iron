module Stable = struct
  module User_name = User_name.Stable
  module Alternate_name = Alternate_name.Stable

  module V1 = struct
    type t = User_name.V1.t Alternate_name.V1.Map.t [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| aa41ee0fc9442ac7cd1b3ce2e5196e91 |}]
    ;;
  end
end

open! Core
open! Import

include Stable.V1

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    Map.iteri t ~f:(fun ~key:alternate_name ~data:user_name ->
      Alternate_name.invariant alternate_name;
      User_name.invariant user_name))
;;

let not_available = Alternate_name.Map.empty

let alternate_name_of_unresolved_name unresolved_name =
  Alternate_name.of_string (Unresolved_name.to_string unresolved_name)
;;

let user_name_of_unresolved_name unresolved_name =
  User_name.of_string (Unresolved_name.to_string unresolved_name)
;;

let to_user_name_opt t unresolved_name =
  Map.find t (alternate_name_of_unresolved_name unresolved_name)
;;

let to_user_name t unresolved_name =
  match to_user_name_opt t unresolved_name with
  | None           -> user_name_of_unresolved_name unresolved_name
  | Some user_name -> user_name
;;

let remove_if_present t ~alternate_name =
  if Map.mem t alternate_name
  then Some (Map.remove t alternate_name)
  else None
;;

let no_duplicate_exn alternate_name u u' ~on_error =
  if User_name.(<>) u u'
  then never_returns (on_error alternate_name [ u; u' ]);
;;

let add_exn t ~alternate_name ~user_name ~on_error =
  match Map.find t alternate_name with
  | Some user_name' ->
    no_duplicate_exn alternate_name user_name user_name' ~on_error;
    t
  | None -> Map.add t ~key:alternate_name ~data:user_name
;;

let merge_exn t t' ~on_error =
  Map.merge t t' ~f:(fun ~key:alternate_name data ->
    match data with
    | `Left user_name | `Right user_name -> Some user_name
    | `Both (user_name, user_name') ->
      no_duplicate_exn alternate_name user_name user_name' ~on_error;
      Some user_name)
;;

let iteri = Map.iteri
