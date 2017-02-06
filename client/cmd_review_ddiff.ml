open! Core
open! Async
open! Import

let command =
  let read_diff4s_from_sexp_file_switch = "-read-diff4s-from-sexp-file" in
  Command.async'
    ~summary:"Locally compute and review the feature ddiff of the given revs diamond."
    ~readme:(fun () -> concat ["\
The command synthesizes a review session containing all the requested changes, and
starts the interactive review client loop in the terminal. It has two uses:

- debugging, for example in an attempt to re-run a review client for a particular revs
  diamond even though no such session exists on the server anymore

- trying to understand a merge, allowing to see a diff the way fe would have shown it

Because obligations are not taken into account, the ddiff shown can differ from what fe
would show.  On the plus side, you can see ddiffs despite obligations being broken,
including if the repo is not even managed by fe.

For example:

  $ fe tools review -base jane-113.12 -tip jane-113.12+01

Or, staging the computation of ddiffs:

  $ fe internal diffs jane-113.12 jane-113.12 jane-113.12 jane-113.12+01 \\
      > /tmp/diff4s.txt

  $ fe tools review -read-diff4s-from-sexp-file /tmp/diff4s.txt

"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and context = context ()
     and lines_required_to_separate_ddiff_hunks =
       lines_required_to_separate_ddiff_hunks_with_default
     and read_diff4s_from_sexp_file =
       flag read_diff4s_from_sexp_file_switch (optional file)
         ~doc:"/path/to/diff4s.sexp Read diff4s from a sexp file \
               instead of computing them based on some given revisions"
     and ddiff_diamond =
       Diamond.param (optional string) ~doc:(sprintf "rev the hg revision for the %s")
       |> map ~f:(fun d -> lazy (
         if Diamond.for_all d ~f:Option.is_none
         then None
         else Some (Diamond.map d ~f:(function
           | Some rev -> rev
           | None -> failwith "either supply all 4 revisions of the diamond \
                               -{old,new}-{base,tip} or none of them"
         ))))
     and diff_diamond =
       let%map base =
         flag "-base" (optional string) ~doc:"rev the hg revision for the base"
       and tip =
         flag "-tip"  (optional string) ~doc:"rev the hg revision for the tip"
       in
       lazy
         (match base, tip with
          | None     , None     -> None
          | Some base, Some tip -> Some (Diamond.of_one_edge base tip)
          | None     , Some _
          | Some _   , None     ->
            failwith "either supply both -base and -tip or none of them")
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let what_to_diff =
         match read_diff4s_from_sexp_file, force ddiff_diamond, force diff_diamond with
         | Some file , None         , None         -> `Read_diff4s_from_sexp_file file
         | None      , Some diamond , None         -> `Rev_diamond diamond
         | None      , None         , Some diamond -> `Rev_diamond diamond
         | _ , _ , _ ->
           failwith (concat [ "specify what to review, using exactly one of:\n"
                            ; "  -> a ddiff with 4 revisions (b1, f1, b2, f2)\n"
                            ; "  -> a diff using 2 revisions (base, tip)\n"
                            ; "  -> some pre-computed diff4s using " ;
                              read_diff4s_from_sexp_file_switch
                            ])
       in
       let repo_root = ok_exn Repo_root.program_started_in in
       let%bind diff4s =
         match what_to_diff with
         | `Read_diff4s_from_sexp_file sexp_file ->
           let%map stdin = Reader.file_contents sexp_file in
           Sexp.of_string_conv_exn stdin
             [%of_sexp: Diff4.Stable.V2.t list]

         | `Rev_diamond rev_diamond ->
           Diff4s_for_diamond.create_using_fake_obligations
             repo_root
             (Diamond.map rev_diamond ~f:Raw_rev.string)
             ~lines_required_to_separate_ddiff_hunks
           >>| ok_exn
       in
       let%map () =
         Path.with_temp_dir (File_name.of_string "fe_cat_") ~f:(fun temp_dir ->
           let%bind files =
             let%map files =
               Cmd_review.create_files_for_review
                 ~temp_dir
                 ~repo_root
                 ~diff4s
                 ~reviewer:`Whole_diff_plus_ignored
                 ~context
                 ~lines_required_to_separate_ddiff_hunks
             in
             List.zip_exn diff4s files
           in
           let module M = struct
             type t = Diff4.t
             let reviewed (_:t list) = Deferred.unit
             let path_in_repo = Diff4.path_in_repo_at_f2
             let num_lines_in_diff = None
             let always_open_file_in_emacs = false
             let open_file_in_emacs = None
             let may_commit_session = false
           end in
           let%map res = Review_main.files (module M) files in
           printf "%s\n"
             (Sexp.to_string ([%sexp_of: [ `Reviewed | `Commit_session | `Quit ]] res));
         )
       in
       ()
    )
;;
