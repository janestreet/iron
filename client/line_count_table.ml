open! Core
open! Import

let create
      ~show_completed_review
      (line_count_by_user : (User_name.t * Line_count.t) list)
  =
  let rows =
    line_count_by_user
    |> List.filter ~f:(fun (_user, line_count) ->
      Line_count.is_shown line_count ~show_completed_review)
    |> Line_count.sort_decreasing_review
  in
  let lines = Ascii_table.Column.int ~show_zero:false ~show:`If_not_empty in
  let review =
    Ascii_table.Column.string ~show_zero:false ~show:`If_not_empty ~align:Right
  in
  let columns =
    Ascii_table.Column.(
      let f f = cell (fun (_name, (line_count : Line_count.t)) -> f line_count) in
      let f' f = attr_cell (fun (_name, (line_count : Line_count.t)) -> f line_count) in
      [ user (cell (fun (name, _) -> name))
      ; review ~header:"review" (f (fun line_count ->
          line_count
          |> Line_count.to_review_column_shown
          |> Review_or_commit.to_string_hum))
      ; lines  ~header:"follow"    (f (fun line_count ->
          line_count.review.follow))
      ; lines  ~header:"catch-up"  (f (fun line_count ->
          Line_count.Catch_up.total line_count.catch_up))
      ; lines  ~header:"completed" (f' (fun line_count ->
          [ `Dim ], if show_completed_review then line_count.completed else 0))
      ])
  in
  Ascii_table.create ~columns ~rows
;;
