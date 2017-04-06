open! Core
open! Async
open! Import

let warn text ~subject users =
  let%bind () =
    Async_interactive.printf "\
Sending mail

------
Subject: %s
%s------
to the following users: \n"
      subject text
  in
  let%bind () =
    Deferred.List.iter (Set.to_list users)
      ~f:(fun u -> Async_interactive.printf "  %s\n" (User_name.to_string u))
  in
  Async_interactive.print_string "\n"
;;

(* We have to parse out the subject line because we pass the whole message through
   the text editor. *)
let parse_subject text =
  let lines = String.split ~on:'\n' text in
  let subject_start = "Subject: " in
  let pre_subject, subject_and_later =
    List.split_while lines ~f:(fun s -> not (String.is_prefix s ~prefix:subject_start))
  in
  match subject_and_later with
  | [] -> failwith "Can't find subject line"
  | subj_line :: post_subject ->
    let subject = String.chop_prefix_exn ~prefix:subject_start subj_line in
    let body = String.concat ~sep:"\n" (pre_subject @ post_subject) in
    (subject, body)
;;

let%test_module _ =
  (module struct
    let%test _ =
      let text = "\
foo
Subject: subject
bar"
      in
      let (subject, body) = parse_subject text in
      String.equal subject "subject"
      && String.equal body "foo\nbar"
    ;;

    let%test _ =
      let text = "\
Subject: subject

foo"
      in
      let (subject, body) = parse_subject text in
      String.equal subject "subject"
      && String.equal body "\nfoo"
    ;;

    let%test _ =
      let text = "foo" in
      try
        ignore (parse_subject text);
        false
      with
      | _ -> true
    ;;
  end)

let command =
  Command.async'
    ~summary:"email a code review reminder"
    ~readme:(fun () -> concat ["\
When run interactively the command will show the remind message and give the user the
opportunity to edit it.  Otherwise, the email is sent without requiring a confirmation.
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and () = interactive
     and display_ascii = display_ascii
     and users = users_option ~switch:"users"
     and cc = users_option ~switch:"cc"
     and just_print_recipients =
       no_arg_flag "-just-print-recipients-and-exit"
         ~doc:" print the recipients list and exit 0"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       (* Re-encode [-users user,...] flag to go out over the wire: *)
       let users : Remind.Action.active_user_set =
         match users with
         | None       -> All_active
         | Some users -> Some_active users
       in
       let%bind { description
                ; line_count_by_user
                ; users_with_review_session_in_progress
                ; users_with_unclean_workspaces
                ; users
                ; cr_summary
                ; next_bookmark_update
                } =
         Remind.rpc_to_server_exn { feature_path; users }
       in
       let no_email_to_send = Set.is_empty users in
       let recipients =
         Set.to_list users
         |> List.map ~f:User_name.to_string
       in
       let cc =
         Option.value_map cc ~default:[] ~f:Set.to_list
         |> List.map ~f:User_name.to_string
       in
       if just_print_recipients
       then (
         let recipients =
           if no_email_to_send
           then []
           else
             recipients @ cc
             |> List.sort ~cmp:String.compare
         in
         List.iter recipients ~f:print_endline;
         return ())
       else if no_email_to_send
       then (
         print_string "all users are up-to-date.  there is no email to send!\n";
         Deferred.unit)
       else (
         let rec loop_until_sent_or_quit draft_msg =
           let (subject, body) = parse_subject draft_msg in
           let%bind () = warn body ~subject users in
           match%bind
             if not !Async_interactive.interactive
             then return `Yes
             else (
               let%bind () =
                 match next_bookmark_update with
                 | No_update_expected_due_to_iron_bug _
                 | No_update_expected -> return ()
                 | Update_expected_since since ->
                   Async_interactive.printf "\
Warning: a bookmark update is expected since %s.\n"
                     (Time.Span.to_short_string (how_long ~since));
               in
               let module Choice = Async_interactive.Choice in
               Async_interactive.ask_dispatch_with_help "Send mail?"
                 [ Choice.create 'y' `Yes  "send the remind mail"
                 ; Choice.create 'n' `No   "do not send the remind mail"
                 ; Choice.create 'e' `Edit "edit the remind mail"
                 ])
           with
           | `No   ->
             print_string "Aborted\n";
             Deferred.unit
           | `Yes  ->
             let me = User_name.unix_login |> User_name.to_string in
             let module Sendmail = Core_extended.Std.Sendmail.Deprecated_use_async_smtp_std_simplemail in
             Sendmail.send ~subject ~recipients ~cc:(me::cc) body;
             print_string "Email sent.\n";
             Deferred.unit
           | `Edit ->
             let%bind draft_msg =
               Editor.invoke_editor draft_msg
             in
             loop_until_sent_or_quit (ok_exn draft_msg)
         in
         let draft_msg =
           let cr_table = Cr_comment.Summary.to_ascii_table cr_summary in
           let feature_path = Feature_path.to_string feature_path in
           let string_of_table table =
             if Ascii_table.is_empty table
             then ""
             else "\n" ^ Ascii_table.to_string
                           ?force_unicode_bars:(if display_ascii then None else Some ())
                           ~display_ascii:true
                           ~max_output_columns:90
                           table
           in
           let string_of_table_opt = function
             | None -> ""
             | Some table -> string_of_table table
           in
           String.concat
             [ "Subject: reminder for Iron feature "; feature_path; "\n"
             ; string_of_table_opt cr_table
             ; string_of_table
                 (Line_count_table.create ~show_completed_review:true line_count_by_user)
             ; (match users_with_review_session_in_progress with
                | Error _ -> ""
                | Ok users_with_review_session_in_progress ->
                  string_of_table
                    (Cmd_show.review_sessions_in_progress_table
                       users_with_review_session_in_progress))
             ; string_of_table
                 (Cmd_show.unclean_workspaces_table users_with_unclean_workspaces)
             ; "\n"
             ; feature_path; "\n"
             ; String.make (String.length feature_path) '='; "\n"
             ; description
             ; "\n"
             ]
         in
         loop_until_sent_or_quit draft_msg))
;;
