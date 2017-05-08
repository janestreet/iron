open! Core
open! Import

type rev =
  | Tip
  | Non_tip of Node_hash.First_12.t
[@@deriving sexp_of]

(* The use of [all_of_rev] here is just for testing purposes. *)
let all_of_rev = [ Tip; Non_tip (Node_hash.First_12.of_string "123456789012") ]
;;

(* The use of [enumerate] here is just for testing purposes. *)
type finished =
  | Compiles of rev
  | Broken   of rev
[@@deriving enumerate, sexp_of]

type pending = Time.Span.t * rev
[@@deriving sexp_of]

(* The use of [all_of_pending] here is just for testing purposes. *)
let all_of_pending = List.map all_of_rev ~f:(fun rev -> Time.Span.of_min 1.26489, rev)

type status =
  { pending  : pending option
  ; finished : finished option
  }
[@@deriving enumerate, sexp_of]

type t =
  | Empty
  | Named     of status Repo_controller_name.Map.t
  | Main_only of status
[@@deriving sexp_of]

let to_rev first_12 ~tip =
  if Node_hash.First_12.equal first_12 (Rev.to_first_12 tip)
  then Tip
  else Non_tip first_12
;;

let to_status tip { Compilation_status. finished; pending } =
  { pending =
      (match List.find pending ~f:(fun t ->
         Node_hash.First_12.equal t.first_12_of_rev (Rev.to_first_12 tip)) with
       | None -> List.last pending
       | (Some _) as value -> value)
      |> Option.map ~f:(fun t ->
        let pending_for =
          if am_functional_testing
          then sec 42.  (* for printing a constant in tests *)
          else Time.diff (Time.now ()) t.pending_since
        in
        pending_for, to_rev t.first_12_of_rev ~tip)
  ; finished =
      Option.map finished ~f:(function
        | Working working -> Compiles (to_rev working.first_12_of_rev ~tip)
        | Broken { last_broken; first_broken = _; last_working = _ } ->
          Broken (to_rev last_broken.first_12_of_rev ~tip))}
;;

let of_compilation_status compilation_status feature_path ~feature_tip:tip
      ~show_full_compilation_status =
  let submissions = Repo_controller_name.submissions feature_path in
  if show_full_compilation_status
  then (
    match Map.find compilation_status submissions with
    | Some value when Map.length compilation_status = 1 -> Main_only (to_status tip value)
    | _ -> Named (Map.map compilation_status ~f:(to_status tip)))
  else (
    let continuous_release = Repo_controller_name.continuous_release feature_path in
    match
      ( Map.find compilation_status submissions
      , Map.find compilation_status continuous_release )
    with
    | None, None -> Empty
    | Some value, None -> Main_only (to_status tip value)
    | None, Some value ->
      Named (Repo_controller_name.Map.singleton continuous_release (to_status tip value))
    | Some submissions_value, Some continuous_release_value ->
      Named
        (Repo_controller_name.Map.of_alist_exn
           [ submissions       , to_status tip submissions_value
           ; continuous_release, to_status tip continuous_release_value
           ]))
;;

let span_to_string = Time.Span.to_short_string

let status_row { pending; finished } =
  let show color msg = color, msg in
  let to_string = Node_hash.First_12.to_string in
  match pending with
  | None ->
    (match finished with
     | None                ->
       show `Red   (Next_step.to_string_hum Report_iron_bug)
     | Some (Compiles Tip) -> show `Default "tip compiles"
     | Some (Broken   Tip) -> show `Red   "tip broken"
     | Some (Compiles (Non_tip rev)) -> show `Yellow (to_string rev ^ " compiles")
     | Some (Broken   (Non_tip rev)) -> show `Yellow (to_string rev ^ " broken"))
  | Some (pending_for, Tip) -> (* tip is pending; normal situation during development *)
    let for_ = " for " ^ span_to_string pending_for in
    (match finished with
     | None                ->
       show `Yellow ("tip pending" ^ for_)
     | Some (Compiles Tip) -> show `Default  "tip compiles" (* accidental retry *)
     | Some (Broken   Tip) -> show `Yellow ("tip broken, retrying" ^ for_)
     | Some (Compiles (Non_tip rev)) ->
       show `Default (to_string rev ^ " compiles, tip pending" ^ for_)
     | Some (Broken   (Non_tip rev)) ->
       show `Yellow (to_string rev ^ " broken, tip pending" ^ for_))
  | Some (pending_for, (Non_tip pending_rev)) -> (* bookmark update is delayed *)
    let pending_for = " pending for " ^ span_to_string pending_for in
    (match finished with
     | None -> show `Yellow (to_string pending_rev ^ pending_for)
     | Some (Compiles Tip) ->
       show `Default ("tip compiles, " ^ to_string pending_rev ^ pending_for)
     | Some (Broken   Tip) ->
       show `Yellow ("tip broken, " ^ to_string pending_rev ^ pending_for)
     | Some (Compiles (Non_tip rev)) ->
       show `Yellow (to_string rev ^ " compiles, " ^ to_string pending_rev ^ pending_for)
     | Some (Broken   (Non_tip rev)) ->
       show `Yellow (to_string rev ^ " broken, " ^ to_string rev ^ pending_for))
;;

let%expect_test "How compilation status is shown" =
  List.iter all_of_status ~f:(fun status ->
    let (color, text) = status_row status in
    Printf.printf "%s\n<%s> %s\n\n"
      (Sexp.to_string_hum [%sexp (status : status)])
      (color |> [%sexp_of: [ `Red | `Yellow | `Default ] ] |> Sexp.to_string)
      text);
  [%expect {|
    ((pending ()) (finished ()))
    <Red> report Iron bug

    ((pending ((1.26489m Tip))) (finished ()))
    <Yellow> tip pending for 1.2m

    ((pending ((1.26489m (Non_tip 123456789012)))) (finished ()))
    <Yellow> 123456789012 pending for 1.2m

    ((pending ()) (finished ((Compiles Tip))))
    <Default> tip compiles

    ((pending ((1.26489m Tip))) (finished ((Compiles Tip))))
    <Default> tip compiles

    ((pending ((1.26489m (Non_tip 123456789012)))) (finished ((Compiles Tip))))
    <Default> tip compiles, 123456789012 pending for 1.2m

    ((pending ()) (finished ((Compiles (Non_tip 123456789012)))))
    <Yellow> 123456789012 compiles

    ((pending ((1.26489m Tip))) (finished ((Compiles (Non_tip 123456789012)))))
    <Default> 123456789012 compiles, tip pending for 1.2m

    ((pending ((1.26489m (Non_tip 123456789012))))
     (finished ((Compiles (Non_tip 123456789012)))))
    <Yellow> 123456789012 compiles, 123456789012 pending for 1.2m

    ((pending ()) (finished ((Broken Tip))))
    <Red> tip broken

    ((pending ((1.26489m Tip))) (finished ((Broken Tip))))
    <Yellow> tip broken, retrying for 1.2m

    ((pending ((1.26489m (Non_tip 123456789012)))) (finished ((Broken Tip))))
    <Yellow> tip broken, 123456789012 pending for 1.2m

    ((pending ()) (finished ((Broken (Non_tip 123456789012)))))
    <Yellow> 123456789012 broken

    ((pending ((1.26489m Tip))) (finished ((Broken (Non_tip 123456789012)))))
    <Yellow> 123456789012 broken, tip pending for 1.2m

    ((pending ((1.26489m (Non_tip 123456789012))))
     (finished ((Broken (Non_tip 123456789012)))))
    <Yellow> 123456789012 broken, 123456789012 pending for 1.2m |}]
;;

let show_compilation_status_row row_name status =
  let (color, msg) = status_row status in
  let style =
    match color with
    | ( `Red | `Yellow ) as color -> [ color ]
    | `Default -> []
  in
  row_name, (style, msg)
;;

let title = "compilation status"

let to_ascii_table_rows = function
  | Empty -> []
  | Main_only status -> [ show_compilation_status_row title status ]
  | Named compilation_status ->
    if Map.is_empty compilation_status
    then [ title, ([ ], "none available") ]
    else (
      (title, ([ ], ""))
      :: List.map (Map.to_alist compilation_status)
           ~f:(fun (repo_controller_name, compilation_status) ->
             show_compilation_status_row
               ("  " ^ Repo_controller_name.to_string repo_controller_name)
               compilation_status))
;;
