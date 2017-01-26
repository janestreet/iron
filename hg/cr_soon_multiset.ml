module Pre_stable = struct
  open! Import_stable

  module Cr_soon = Cr_comment.Stable.Cr_soon

  module Cr_soon_in_feature = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; cr_soon      : Cr_soon.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| a54865d3b4ab69b23c53348182415c5c |}]
      ;;
    end
  end
end

open! Core
open! Import

module Cr_soon_in_feature = struct

  module T = struct
    include Pre_stable.Cr_soon_in_feature.V1

    let hash t    = Cr_soon.Compare_ignoring_minor_text_changes.hash t.cr_soon

    let compare t1 t2 =
      Cr_soon.Compare_ignoring_minor_text_changes.compare t1.cr_soon t2.cr_soon
    ;;
  end

  include T

  let invariant (t : t) =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~feature_path:(check Feature_path.invariant)
        ~cr_soon:(check Cr_soon.invariant))
  ;;

  let assignee   t = Cr_soon.assignee   t.cr_soon
  let cr_comment t = Cr_soon.cr_comment t.cr_soon
  let path       t = Cr_soon.path       t.cr_soon
  let start_line t = Cr_soon.start_line t.cr_soon

  let family t = Feature_path.root t.feature_path

  let active_in t =
    if Feature_path.is_root t.feature_path
    then None
    else Some t.feature_path
  ;;

  module For_sorted_output = struct

    type nonrec t = t

    let compare t1 t2 =
      let c = Feature_name.compare (family t1) (family t2) in
      if c <> 0
      then c
      else Cr_soon.For_sorted_output.compare t1.cr_soon t2.cr_soon
    ;;
  end

  include Comparable.Make_plain (T)
  include Hashable.  Make_plain (T)

  let rename_feature ({ cr_soon; feature_path } as t) ~from ~to_ =
    if Feature_path.equal feature_path from
    then { cr_soon; feature_path = to_ }
    else t
  ;;
end

module Cr_soon_map = Cr_soon_in_feature.Map

type t = int Cr_soon_map.t [@@deriving compare, sexp_of]

let invariant (t : t) : unit =
  Map.iteri t ~f:(fun ~key:cr_soon ~data:count ->
    Cr_soon_in_feature.invariant cr_soon;
    assert (count > 0))
;;

let empty = Cr_soon_map.empty

let is_empty = Cr_soon_map.is_empty

let of_alist_exn = Cr_soon_map.of_alist_exn

let to_alist = Map.to_alist

let of_list list =
  Cr_soon_map.of_alist_fold (List.map list ~f:(fun x -> (x, ())))
    ~init:0 ~f:(fun n () -> n + 1)
;;

let create cr_soons feature_path =
  of_list
    (List.map cr_soons ~f:(fun cr_soon ->
       { Cr_soon_in_feature. cr_soon; feature_path }))
;;

let to_list t =
  List.concat_map (to_alist t) ~f:(fun (cr_soon_in_feature, n) ->
    List.init n ~f:(fun _ -> cr_soon_in_feature))
;;

let iter t ~f = Map.iteri t ~f:(fun ~key ~data:num_occurrences -> f key ~num_occurrences)

let choose_feature_path t =
  with_return_option (fun r ->
    iter t ~f:(fun cr_soon_in_feature ~num_occurrences:_ ->
      r.return cr_soon_in_feature.Cr_soon_in_feature.feature_path))
;;

let rename_feature t ~from ~to_ =
  to_alist t
  |> List.map
       ~f:(fun (cr_soon_in_feature, n) ->
         (Cr_soon_in_feature.rename_feature cr_soon_in_feature ~from ~to_, n))
  |> of_alist_exn
;;

let filter t ~f =
  to_alist t
  |> List.filter ~f:(fun (cr_soon_in_feature, _) -> f cr_soon_in_feature)
  |> of_alist_exn
;;

let equal t1 t2 = Map.equal Int.equal t1 t2

let diff t1 t2 =
  Map.merge t1 t2 ~f:(fun ~key:_ -> function
    | `Left n -> Some n
    | `Right _ -> None
    | `Both (n1, n2) ->
      let d = n1 - n2 in
      if d <= 0 then None else Some d)
;;

let union t1 t2 =
  Map.merge t1 t2 ~f:(fun ~key:_ -> function
    | `Left n -> Some n
    | `Right n -> Some n
    | `Both (n1, n2) -> Some (n1 + n2))
;;

(* runs in O(n lg n) time, rather than O(n^2) time as [List.fold] with [union] does
   (because [Map.merge m1 m2] runs in O(size m1 + size m2)) *)
let unions ts =
  match ts with
  | [] -> empty
  | hd :: tl ->
    List.fold tl ~init:hd ~f:(fun acc next ->
      Map.fold next ~init:acc ~f:(fun ~key:cr ~data:count acc ->
        Map.update acc cr ~f:(function
          | None -> count
          | Some prev_count -> count + prev_count)))
;;

let partition_by_assignee t alternate_names =
  List.map (Map.to_alist t) ~f:(fun (cr_soon, count) ->
    let assignee = Cr_soon_in_feature.assignee cr_soon in
    (User_name_by_alternate_name.to_user_name alternate_names assignee, (cr_soon, count)))
  |> User_name.Map.of_alist_multi
  |> Map.map ~f:of_alist_exn
;;

module Stable = struct

  include Pre_stable

  module V1 = struct
    include Make_stable.Of_stable_format.V1
        (struct
          type t = (Cr_soon_in_feature.V1.t * int) list [@@deriving bin_io, sexp]
        end)
        (struct
          type nonrec t = t [@@deriving compare]
          let of_stable_format = of_alist_exn
          let to_stable_format x = to_alist x
        end)

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 6a97a4c3fe9fa833dcf1d598e874eea3 |}]
    ;;
  end
  ;;
end
