open Core
open Import

let%test_module _ =
  (module struct

    let test_double_pdiff ?verbose
          ~old_base:b1 ~new_base:b2 ~old_tip:f1 ~new_tip:f2 ~expect () =
      let context = 0 in
      let lines_required_to_separate_ddiff_hunks = 0 in
      let rev_names = Diamond.pretty_short_rev_names_const in
      let contents = { Diamond.b1 ; b2 ; f1 ; f2 } in
      [%test_result: Slice.t Diamond.t list]
        (Segments.of_files ?verbose ~force_should_split_files_in_hunks_for_tests:true
           ~context ~lines_required_to_separate_ddiff_hunks ~contents ~rev_names ()
         |> List.map ~f:(fun (segment : Segment.t) ->
           (* Check for thrown exceptions when doing a normal diff with every
              combination of diff algorithm. *)
           List.iter Diff4_class.Shown_class.all ~f:(fun diff4_class ->
             List.iter (Diff_algo.select_algos_for_review diff4_class) ~f:(fun diff_algo ->
               Diff_algo.apply diff_algo
                 ~include_hunk_breaks:true
                 ~diff4_class:segment.diff4_class
                 ~context segment.slice
               |> (ignore : Diff_algo.View.t -> unit)));
           segment.slice
         ))
        ~expect;
    ;;

    let of_tagged_lines old_base new_base old_tip new_tip =
      let old_base_start, old_base_lines = old_base in
      let new_base_start, new_base_lines = new_base in
      let old_tip_start,  old_tip_lines  = old_tip  in
      let new_tip_start,  new_tip_lines  = new_tip  in
      let f (line, lines, source) = Slice.create ~source line lines in
      { Diamond.
        b1 = f (old_base_start, old_base_lines, "old base")
      ; b2 = f (new_base_start, new_base_lines, "new base")
      ; f1 = f (old_tip_start , old_tip_lines , "old tip" )
      ; f2 = f (new_tip_start , new_tip_lines , "new tip" )
      }
    ;;

    let ints xs =
      String.concat ~sep:"\n" (List.map ~f:Int.to_string xs)
    ;;

    let%test_unit _ =
      test_double_pdiff
        ~old_base: (ints [ 0; 1; 2; 3; 4; 5 ])
        ~new_base: (ints [ 0; 1; 2; 3; 4; 5 ])
        ~old_tip:  (ints [ 0; 1; 2; 3; 4; 5 ])
        ~new_tip:  (ints [ 0; 1; 2; 3; 4; 5 ])
        ~expect:[]
        ()
    ;;

    let%test_unit "single-line" =
      test_double_pdiff
        ~old_base: (ints [ 0; 1; 2; 0; 4; 5 ])
        ~new_base: (ints [ 0; 1; 2; 0; 4; 5 ])
        ~old_tip:  (ints [ 0; 1; 2; 0; 4; 5 ])
        ~new_tip:  (ints [ 0; 1; 2; 3; 4; 5 ])
        ~expect:
          [ of_tagged_lines
              (3, ["0"])
              (3, ["0"])
              (3, ["0"])
              (3, ["3"])
          ]
        ()
    ;;

    let%test_unit "wide" =
      test_double_pdiff
        ~old_base: (ints [ 0; 1; 2; 0; 4; 5 ])
        ~new_base: (ints [ 0; 1; 1; 0; 4; 5 ])
        ~old_tip:  (ints [ 0; 1; 2; 0; 4; 5 ])
        ~new_tip:  (ints [ 0; 1; 2; 3; 4; 5 ])
        ~expect:
          [ of_tagged_lines
              (2, ["2"; "0"])
              (2, ["1"; "0"])
              (2, ["2"; "0"])
              (2, ["2"; "3"])
          ]
        ()
    ;;

    let%test_unit "multiple different" =
      test_double_pdiff
        ~old_base: (ints [ 0; 10; 2; 11; 4; 12; 6 ])
        ~new_base: (ints [ 0; 20; 2; 21; 4; 22; 6 ])
        ~old_tip:  (ints [ 0; 30; 2; 31; 4; 32; 6 ])
        ~new_tip:  (ints [ 0; 40; 2; 41; 4; 42; 6 ])
        ~expect:
          [ of_tagged_lines
              (1, ["10"])
              (1, ["20"])
              (1, ["30"])
              (1, ["40"])

          ; of_tagged_lines
              (3, ["11"])
              (3, ["21"])
              (3, ["31"])
              (3, ["41"])

          ; of_tagged_lines
              (5, ["12"])
              (5, ["22"])
              (5, ["32"])
              (5, ["42"])
          ]
        ()
    ;;

    let%test_unit "multi-line, with gap" =
      test_double_pdiff
        ~old_base: (ints [ 0; 11; 20; 0; 30; 40; 0 ])
        ~new_base: (ints [ 0; 10; 21; 0; 30; 40; 0 ])
        ~old_tip:  (ints [ 0; 10; 20; 0; 31; 40; 0 ])
        ~new_tip:  (ints [ 0; 10; 20; 0; 30; 41; 0 ])
        ~expect:
          [ of_tagged_lines
              (1, ["11";"20"])
              (1, ["10";"21"])
              (1, ["10";"20"])
              (1, ["10";"20"])

          ; of_tagged_lines
              (4, ["30";"40"])
              (4, ["30";"40"])
              (4, ["31";"40"])
              (4, ["30";"41"])
          ]
        ()
    ;;

    let%test_unit "pairwise equal" =
      test_double_pdiff
        ~old_base: (ints [ 0; 10; 20; 0; 31; 41; 0 ])
        ~new_tip:  (ints [ 0; 10; 20; 0; 31; 41; 0 ])
        ~new_base: (ints [ 0; 11; 21; 0; 30; 40; 0 ])
        ~old_tip:  (ints [ 0; 11; 21; 0; 30; 40; 0 ])
        ~expect:
          [ of_tagged_lines
              (1, ["10";"20"])
              (1, ["11";"21"])
              (1, ["11";"21"])
              (1, ["10";"20"])

          ; of_tagged_lines
              (4, ["31";"41"])
              (4, ["30";"40"])
              (4, ["30";"40"])
              (4, ["31";"41"])
          ]
        ()
    ;;

    let%test_unit "pairwise equal not shown b1_f1__b2_f2" =
      test_double_pdiff
        ~old_base: (ints [ 0; 10; 20; 0; 31; 41; 0 ])
        ~old_tip:  (ints [ 0; 10; 20; 0; 31; 41; 0 ])
        ~new_base: (ints [ 0; 11; 21; 0; 30; 40; 0 ])
        ~new_tip:  (ints [ 0; 11; 21; 0; 30; 40; 0 ])
        ~expect:[]
        ()
    ;;

    let%test_unit "pairwise equal not shown b1_b2__f1_f2" =
      test_double_pdiff
        ~old_base: (ints [ 0; 10; 20; 0; 31; 41; 0 ])
        ~new_base: (ints [ 0; 10; 20; 0; 31; 41; 0 ])
        ~old_tip:  (ints [ 0; 11; 21; 0; 30; 40; 0 ])
        ~new_tip:  (ints [ 0; 11; 21; 0; 30; 40; 0 ])
        ~expect:[]
        ()
    ;;

    let%test_unit "different lengths" =
      test_double_pdiff
        ~old_base: (ints [ ])
        ~new_base: (ints [ 0   ])
        ~old_tip:  (ints [ 0;1  ])
        ~new_tip:  (ints [ 0;1;2 ])
        ~expect:
          [ of_tagged_lines
              (0, [])
              (0, ["0"])
              (0, ["0";"1"])
              (0, ["0";"1";"2"])
          ]
        ()
    ;;

    let%test_unit "empty hunks" =
      test_double_pdiff
        ~old_base: (ints [ 1; 0; 1; 10; 91; 92; 93; 94; 20; 4; 1       ])
        ~new_base: (ints [ 2; 0;    10; 91; 92; 93; 94; 20; 2; 3; 4; 2 ])
        ~old_tip:  (ints [ 3; 0; 1; 10; 91; 92; 93; 94; 20; 2; 3; 4; 3 ])
        ~new_tip:  (ints [ 4; 0; 1; 10; 91; 92; 93; 94; 20; 3; 4; 4    ])
        ~expect:
          [ of_tagged_lines
              (0, ["1"])
              (0, ["2"])
              (0, ["3"])
              (0, ["4"])

          ; of_tagged_lines
              (2, ["1"])
              (2, [   ])
              (2, ["1"])
              (2, ["1"])

          ; of_tagged_lines
              (9, [])
              (8, ["2";"3"])
              (9, ["2";"3"])
              (9, ["3"])

          ; of_tagged_lines
              (10, ["1"])
              (11, ["2"])
              (12, ["3"])
              (11, ["4"])
          ]
        ()
  end)
