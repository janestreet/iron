open! Core
open! Async
open! Import

module Unclean_workspace_row = struct
  let empty_column = ""

  type t =
    { feature : string
    ; reason  : string
    }
  [@@deriving fields]

  let empty =
    { feature = empty_column
    ; reason  = empty_column
    }
  ;;
end

let unclean_workspaces_columns_and_rows unclean_workspaces =
  let rows =
    Feature_table.create unclean_workspaces
      Unclean_workspace.feature_path
      (fun ~feature reaction ->
         match reaction with
         | None -> { Unclean_workspace_row.empty with feature }
         | Some { feature_path = _; reason } ->
           let reason = Unclean_workspace_reason.to_ascii_table_column_text reason in
           { Unclean_workspace_row.feature; reason })
  in
  let columns =
    Ascii_table.Column.(
      [ string ~header:"feature" (cell Unclean_workspace_row.feature)
      ; string ~header:"reason"  (cell Unclean_workspace_row.reason)
      ])
  in
  columns, rows
;;

module Workspaces_status = struct
  type t =
    { unclean_workspaces : Unclean_workspace.t list
    ; clean_workspaces   : Feature_path.t list
    }

  let compute ?(workspace_not_found_is_an_error = false) features : t Deferred.t =
    let clean_workspaces = Feature_path.Hash_set.create () in
    let%map unclean_workspaces =
      Deferred.List.map ~how:(`Max_concurrent_jobs 10) features ~f:(fun feature_path ->
        let%map status =
          match%bind Workspace.find feature_path with
          | Some share -> Workspace.unclean_status share
          | None ->
            return
              (if workspace_not_found_is_an_error
               then
                 (Workspace.Unclean_status.Unclean
                    (Unclean_workspace_reason.error
                       (Error.create "workspace not found"
                          feature_path [%sexp_of: Feature_path.t])))
               else Workspace.Unclean_status.Clean)
        in
        match status with
        | Clean -> Hash_set.add clean_workspaces feature_path; None
        | Unclean reason -> Some { Unclean_workspace. feature_path; reason })
    in
    { unclean_workspaces = List.filter_opt unclean_workspaces
    ; clean_workspaces   = clean_workspaces |> Hash_set.to_list
    }
  ;;

  let compute_all () =
    let%bind shares = Workspace.list () in
    shares
    |> List.map ~f:Workspace.feature_path
    |> compute
  ;;
end

let list_command =
  Command.async' ~summary:"find unclean workspaces on the local machine and show them"
    ~readme:(fun () ->
      concat [ "\
This command is slow because walks all of one's workspaces.  It is not intended to be used
directly, but rather just occasionally for example for trouble shooting.  There is a set
of background jobs that one can set up to cache this information in the server, and access
it directly with:

  $ fe todo -unclean-workspaces
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and () = interactive
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     and name_only =
       no_arg_flag "-name-only" ~doc:"show only the feature names, one per line"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map { unclean_workspaces; clean_workspaces = _ } =
         Workspaces_status.compute_all ()
       in
       if name_only
       then (
         unclean_workspaces
         |> List.map ~f:Unclean_workspace.feature_path
         |> Feature_path.Set.of_list
         |> Set.to_list
         |> List.iter ~f:(fun feature_path ->
           print_endline (Feature_path.to_string feature_path)))
       else (
         let my_host = Unix.gethostname () in
         let table =
           let columns, rows = unclean_workspaces_columns_and_rows unclean_workspaces in
           Ascii_table.to_string
             (Ascii_table.create ~columns ~rows)
             ~display_ascii
             ~max_output_columns
         in
         if not (List.is_empty unclean_workspaces)
         then (
           print_endline (sprintf "Unclean workspaces on %s:" my_host);
           print_endline table)))
;;

let compute_and_update_server_exn ~for_ which_features =
  let machine_name = Unix.gethostname () |> Machine_name.of_string in
  match which_features with
  | `All_features ->
    let%bind { unclean_workspaces; clean_workspaces = _ } =
      Workspaces_status.compute_all ()
    in
    Update_unclean_workspaces.rpc_to_server_exn
      { for_
      ; machine_name
      ; unclean_workspaces
      ; clean_workspaces   = `Complement_of_those_listed_as_unclean
      }

  | `Features features ->
    let%bind { unclean_workspaces; clean_workspaces } =
      Workspaces_status.compute features
    in
    Update_unclean_workspaces.rpc_to_server_exn
      { for_
      ; machine_name
      ; unclean_workspaces
      ; clean_workspaces = `At_least_these clean_workspaces
      }
;;

let update_server_command =
  Command.async' ~summary:"compute unclean workspaces statuses on the local machine \
                           and communicate this information to Iron"
    ~readme:(fun () ->
      concat [ "\
A workspace is considered unclean if it has uncommitted changes, outgoing changes, or if
an error occurred while trying to get this very information.

This command walks all of one's workspaces, compute unclean workspaces statuses and
communicate the information to Iron to be displayed in one's [fe todo].

A user may run this command periodically in the background.  Typically it is called by
the script [/j/office/app/fe/prod/etc/update-workspaces] which it called hourly under the
standard setup.

One can disable this functionality by having the following line in one's ferc:

  (workspaces (
    (unclean_workspaces_detection_is_enabled false)
  ))

When this is disabled the command fails, unless " ; Switch.do_nothing_if_not_enabled ;
               " is supplied.
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and () = interactive
     and for_ = for_
     and which_features =
       which_features
         ~allow_rec_flag:false
         ~allow_unexisting_features:true
         ~allow_empty_selection:false
         ~default_to_current_bookmark:true ()
     and do_nothing_if_not_enabled =
       no_arg_flag Switch.do_nothing_if_not_enabled
         ~doc:" exit 0 if the functionality is not enabled by the user"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let client_config = Client_config.get () in
       if not (Client_config.Workspaces.unclean_workspaces_detection_is_enabled
                 client_config)
       then
         if do_nothing_if_not_enabled
         then
           Async_interactive.printf "Unclean workspaces detection is not enabled, and \
                                     the switch %s was supplied.\nExiting with code 0\n"
             Switch.do_nothing_if_not_enabled
         else (
           Client_config.Workspaces.are_enabled_exn client_config;
           failwith
             (sprintf "Unclean workspaces detection is not enabled.\n\
                       Consider enabling the functionality via your [.ferc], \
                       or supply %s"
                Switch.do_nothing_if_not_enabled))
       else (
         let%bind which_features = force which_features in
         (* Does the -for test in the client to fail early, since the walk is lengthy *)
         if not am_functional_testing
         && not (User_name.equal for_ User_name.unix_login)
         then
           raise_s
             [%sexp
               "unauthorized use of [-for] in production",
               { requested_by  = (User_name.unix_login : User_name.t)
               ; requested_for = (for_                 : User_name.t)
               }
             ];
         let which_features =
           match which_features with
           | All_features -> `All_features
           | Features features ->
             `Features
               (List.map features ~f:(fun { feature_path; include_descendants } ->
                  assert (not include_descendants); (* allow_rec_flag is set to false *)
                  feature_path))
         in
         compute_and_update_server_exn ~for_ which_features))
;;

let check_command =

  Command.async' ~summary:"fail if your workspaces have pending changes"
    ~readme:(fun () -> concat [ "\
Pending changes are unpushed or uncommitted changes in your workspaces' repositories.
If a feature path is provided it will check the associated workspace or fail if you have
no workspace set up for that feature.  If no feature path is given the flag [-all] is
required, and the command will check all existing workspaces.

Note that the hgrc of a workspace repository is configured for the bookmark matching the
feature path [1], so effectively this command only checks for outgoing changes with that
bookmark name.

[1] from .hg/hgrc:

[defaults]
outgoing = -r FEATURE_PATH
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and () = interactive
     and which_features =
       which_features
         ~allow_rec_flag:false
         ~allow_unexisting_features:true
         ~allow_empty_selection:false
         ~default_to_current_bookmark:true
         ()
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind which_features = force which_features in
       let%map { unclean_workspaces; clean_workspaces = _ } =
         match which_features with
         | All_features -> Workspaces_status.compute_all ()
         | Features features ->
           Workspaces_status.compute
             ~workspace_not_found_is_an_error:true
             (List.map features ~f:(fun { feature_path; include_descendants } ->
                assert (not include_descendants); (* allow_rec_flag is set to false *)
                feature_path))
       in
       if not (List.is_empty unclean_workspaces)
       then raise_s [%sexp "errors", (unclean_workspaces : Unclean_workspace.t list)]
    )
;;

let show_command =
  Command.async'
    ~summary:"output the unclean workspaces state on the server"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and user_or_all = for_or_all_default_me in
     fun () ->
       Cmd_dump.dump (Unclean_workspaces user_or_all)
    )
;;

let remove_machine_command =
  Command.async'
    ~summary:"clear unclean workspaces associated with a pair \
              USER * MACHINE on the server"
    ~readme:(fun () -> "\
This command updates the set of workspaces known to be unclean by the server for a given
USER: It removes from the unclean state all workspaces located on the given MACHINE.

Running the command does not affect local workspaces.  It has no effect on the local disk
on the host it is run, even if this host is indeed the MACHINE in question.

The command needs to be run by the USER, otherwise admin privileges are required.
")
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and for_ = for_
     and machine = anon ("MACHINE" %: (Arg_type.create Machine_name.of_string))
     in
     fun () ->
       With_unclean_workspaces.rpc_to_server_exn (Remove_machine (for_, machine)))
;;

let command =
  Command.group
    ~summary:"manage unclean workspaces"
    [ "check"         , check_command
    ; "list"          , list_command
    ; "remove-machine", remove_machine_command
    ; "show"          , show_command
    ; "update-server" , update_server_command
    ]
;;
