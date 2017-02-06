open! Core
open! Async
open! Import

module Reason_for_waiting = struct
  type tips =
    { in_fe : Hg.Rev.t
    ; in_hg : Node_hash.First_12.t
    }
  [@@deriving sexp_of]

  type t =
    [ `Different_tips of tips
    | `Expecting_bookmark_update of Next_bookmark_update.t
    | `Initial_wait
    ]
  [@@deriving sexp_of]
end

let check_remote { Feature.
                   feature_path
                 ; next_bookmark_update
                 ; tip
                 ; remote_repo_path
                 ; _
                 } =
  (* If a user just pushed and before hydra notifies the bookmark change, Iron is
     not expecting a bookmark update.  We cover this race here by comparing what
     Iron thinks the tip of the feature is with its revision on the remote
     repo. *)
  assert (not (Next_bookmark_update.am_expecting_bookmark_update next_bookmark_update));
  let%map remote_feature_tip =
    Hg.get_remote_rev remote_repo_path (`Feature feature_path)
  in
  let same_tip_in_fe_and_hg =
    Node_hash.First_12.equal (Rev.to_first_12 tip) remote_feature_tip
  in
  if not same_tip_in_fe_and_hg
  then `Different_tips { Reason_for_waiting. in_fe = tip; in_hg = remote_feature_tip }
  else `Finished { Fe.Wait_for_hydra.Reaction. tip; remote_repo_path }
;;

let wait_status (feature : Feature.t) =
  if Next_bookmark_update.am_expecting_bookmark_update feature.next_bookmark_update
  then return (`Expecting_bookmark_update feature.next_bookmark_update)
  else check_remote feature
;;

let check_cleanliness_and_update ~feature_path ~remote_repo_path
      ~actually_update_if_update_requested = function
  | `No_update -> Deferred.unit
  | `Update ->
    let%bind repo_root =
      Cmd_workspace.repo_for_hg_operations_exn feature_path ~use:`Share
    in
    let%bind waiting_on_current_bookmark =
      match%map Hg.current_bookmark repo_root with
      | Ok b ->
        if String.equal b (Feature_path.to_string feature_path)
        then Ok ()
        else
          error_s
            [%sexp
              "not waiting for the current bookmark",
              { current_bookmark = (b : string)
              ; waiting_on       = (feature_path : Feature_path.t)
              }
            ]
      | Error _ as error -> error
    in
    match waiting_on_current_bookmark with
    | Error err -> raise_s [%sexp "won't update the repo", (err : Error.t)]
    | Ok () ->
      let%bind repo_is_clean = Hg.status_cleanliness repo_root in
      let repo_is_clean = ok_exn repo_is_clean in
      if not actually_update_if_update_requested
      then Deferred.unit
      else (
        let%bind () =
          Hg.pull ~repo_is_clean repo_root
            ~from:remote_repo_path (`Feature feature_path)
        in
        let%bind () =
          Hg.update repo_root (`Feature feature_path)
            ~clean_after_update:(Yes repo_is_clean)
        in
        Cmd_workspace.If_enabled.update_satellite_repos ~center_repo_root:repo_root)
;;

let pipe_find pipe ~f =
  let r = ref `End_of_pipe in
  let%map () =
    Pipe.iter' pipe ~f:(fun queue ->
      match%map f queue with
      | None -> ()
      | Some result -> Pipe.close_read pipe; r := `Found result)
  in
  !r
;;

module Waiting_status = struct
  type t =
    { waiting_since      : Time.t
    ; reason_for_waiting : Reason_for_waiting.t
    }
  [@@deriving sexp_of]

  let now reason_for_waiting =
    { waiting_since = Time.now ()
    ; reason_for_waiting
    }
end

let main { Fe.Wait_for_hydra.Action.
           feature_path
         ; rev_zero
         ; timeout
         ; whether_to_update
         } =
  let%bind feature =
    Iron_protocol.Get_feature.rpc_to_server_exn
      { feature_path
      ; rev_zero
      }
  in
  let feature_id = feature.feature_id in
  let remote_repo_path = feature.remote_repo_path in
  let%bind () =
    check_cleanliness_and_update
      ~feature_path ~remote_repo_path
      ~actually_update_if_update_requested:false
      whether_to_update
  in
  let waiting_status = ref (Waiting_status.now `Initial_wait) in
  match%bind
    Clock.with_timeout timeout (
      (* explicitly add a value to the pipe, so that we call [wait_status] if the
         feature is already processed before the pipe opens *)
      let pipe, pipe_writer = Pipe.create () in
      Pipe.write_without_pushback pipe_writer (`Updated feature);
      let start_feature_updates_subscription =
        lazy (don't_wait_for (
          let%bind feature_updates =
            Iron_protocol.Notify_on_feature_updates.rpc_to_server_exn
              { feature_id; when_to_first_notify = Now }
          in
          let%map () =
            Pipe.transfer feature_updates pipe_writer ~f:(fun update ->
              match ok_exn update with
              | `Updated feature -> `Updated feature
              | `Archived -> `Archived)
          in
          Pipe.close pipe_writer))
      in
      let last_non_pending_feature = ref None in
      let is_last_non_pending_feature feature =
        match !last_non_pending_feature with
        | None -> false
        | Some feature' -> phys_equal feature feature'
      in
      let maybe_check_remote_again feature =
        don't_wait_for (
          let%map () = Clock.after (sec 5.) in
          if is_last_non_pending_feature feature
          && not (Pipe.is_closed pipe_writer)
          then Pipe.write_without_pushback pipe_writer
                 (`Check_remote_again feature))
      in
      let process_event_queue event_queue =
        let event =
          Queue.fold ~init:`Skip event_queue ~f:(fun acc event ->
            match event with
            | `Archived ->
              raise_s
                [%sexp
                  "feature was archived while waiting for hydra",
                  { waiting_for = (feature_path : Feature_path.t)
                  ; feature_id                  : Feature_id.t
                  }
                ]
            | `Updated feature -> `Updated feature
            | `Check_remote_again feature ->
              match acc with
              | `Updated _ -> acc
              | `Skip | `Check_remote_again _ ->
                if is_last_non_pending_feature feature
                then `Check_remote_again feature
                else acc)
        in
        match event with
        | `Skip -> return (`Wait (!waiting_status).reason_for_waiting)
        | `Check_remote_again feature ->
          (match%map check_remote feature with
           | `Different_tips _ as reason_to_wait ->
             maybe_check_remote_again feature;
             `Wait reason_to_wait
           | `Finished _ as finished -> finished)

        | `Updated (feature : Feature.t) ->
          last_non_pending_feature := None;
          (if not (Feature_path.equal feature_path feature.feature_path)
           then
             raise_s
               [%sexp
                 "feature was renamed while waiting for hydra",
                 { waiting_for = (feature_path         : Feature_path.t)
                 ; renamed_to  = (feature.feature_path : Feature_path.t)
                 ; feature_id                          : Feature_id.t
                 }
               ]);
          (match%map wait_status feature with
           | `Expecting_bookmark_update _ as reason_to_wait ->
             `Wait reason_to_wait
           | `Finished _ as finished -> finished
           | `Different_tips _ as reason_to_wait ->
             last_non_pending_feature := Some feature;
             maybe_check_remote_again feature;
             `Wait reason_to_wait)
      in
      match%map
        pipe_find pipe ~f:(fun event_queue ->
          match%map process_event_queue event_queue with
          | `Finished x -> Some x
          | `Wait reason ->
            force start_feature_updates_subscription;
            waiting_status := Waiting_status.now reason;
            None
        )
      with
      | `End_of_pipe -> failwith "connection to server lost with feature still pending"
      | `Found feature -> feature)
  with
  | `Timeout ->
    raise_s
      [%sexp (sprintf "timed out after waiting %s for hydra to process %s"
                (Time.Span.to_short_string timeout)
                (Feature_path.to_string feature_path) : string)
           , (waiting_status : Waiting_status.t ref)
      ]
  | `Result result ->
    let%map () =
      check_cleanliness_and_update
        ~feature_path ~remote_repo_path
        ~actually_update_if_update_requested:true
        whether_to_update
    in
    result
;;

let command =
  let timeout_switch = "-timeout" in
  let timeout_default = Time.Span.of_min 5. in
  let line_in_ferc = "(add_flag_to wait-for-hydra -update-local-repo)" in

  Command.async'
    ~summary:"wait until Iron hydra is not processing the supplied feature"
    ~readme:(fun () ->
      concat [ "\
While Iron is waiting for hydra to process a feature, it is shown as [pending].
This command waits and returns when the feature is no longer pending.
There is a default timeout of " ; Time.Span.to_string timeout_default ;
               " (use [" ; timeout_switch ; "] to change it).

If the flag [" ;  Switch.update_local_repo
             ; "] is supplied, the command will also
update the local repository to the tip of the feature just before returning.

One can also add the following line to one's [.ferc] to force the update by default:\n
  " ; line_in_ferc ; "

When using the latter, the switch [" ; Switch.do_not_modify_local_repo
             ; "] acts as a punctual
override.  Otherwise it is implied.
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and timeout =
       flag timeout_switch (optional_with_default timeout_default time_span)
         ~doc:"SPAN abort if the feature is still pending after SPAN"
     and whether_to_update =
       let%map () = return ()
       and do_not_modify_local_repo =
         no_arg_flag Switch.do_not_modify_local_repo
           ~doc:" overrides [.ferc]'s -update-local-repo"
       and update_local_repo = update_local_repo
       in
       Lazy.from_fun (fun () ->
         match do_not_modify_local_repo, update_local_repo with
         | true , false -> `No_update
         | false, true  -> `Update
         | false, false ->
           if Client_config.(get () |> Cmd.Wait_for_hydra.update_local_repo)
           then `Update
           else `No_update
         | true , true  ->
           failwithf "The switches [%s] and [%s] are mutually exclusive"
             Switch.do_not_modify_local_repo Switch.update_local_repo ())
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let whether_to_update = force whether_to_update in
       match%map main { feature_path; rev_zero = None; timeout; whether_to_update } with
       | { tip = _; remote_repo_path = _} -> ()
    )
;;
