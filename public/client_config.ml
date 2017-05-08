open! Core
open! Async
open! Import
open! Iron_common.Std
open! Iron_hg.Std

module Make_client_config = Iron_common.Std.Make_client_config

(* Example of .ferc file:

   {[
     (add_flag_to "todo"   -do-not-show-cr-soons)
     (add_flag_to "review" -do-not-modify-local-repo)
     (add_flag_to "*"      -context 12)
   ]}
*)


module Cmd_create = struct
  type t =
    { mutable reviewing : [ `Whole_feature_reviewers
                          | `First_owner
                          ] option
    }

  let update t =
    let open Make_client_config.Utils in
    empty
    +> no_arg Switch.set_reviewing_whole_feature_only
         (function () -> t.reviewing <- Some `Whole_feature_reviewers)
    +> no_arg Switch.set_reviewing_first_owner_only
         (function () -> t.reviewing <- Some `First_owner)
  ;;

  let create () =
    { reviewing = None
    }
  ;;
end

module Cmd_list = struct
  type t =
    { mutable depth : int option
    }

  let update t =
    let open Make_client_config.Utils in
    empty
    +> flag "-depth" (function Atom "max" -> Int.max_value | s -> [%of_sexp: int] s)
      (fun i -> t.depth <- Some i)
  ;;

  let create () =
    { depth = None
    }
  ;;
end

module Cmd_obligations_show = struct
  type t =
    { mutable display_in_table : bool
    }

  let update t =
    let open Make_client_config.Utils in
    empty
    +> no_arg "-display-in-table"
         (fun () -> t.display_in_table <- true)
  ;;

  let create () =
    { display_in_table = false
    }
  ;;
end

module Cmd_rebase = struct
  type t =
    { mutable merge_tool : Merge_tool.t option
    }

  let update t =
    let open Make_client_config.Utils in
    empty
    +> flag "-merge-tool" [%of_sexp: Merge_tool.t]
         (fun m -> t.merge_tool <- Some m)
  ;;

  let create () = { merge_tool = None }
end

module Cmd_review = struct
  type t =
    { mutable do_not_modify_local_repo : bool
    ; mutable emacs                    : bool
    ; mutable sort_build_order         : bool
    }

  let update t =
    let open Make_client_config.Utils in
    empty
    +> no_arg "-do-not-modify-local-repo"
         (fun () -> t.do_not_modify_local_repo <- true)
    +> no_arg "-emacs"
         (fun () -> t.emacs <- true)
    +> no_arg "-sort-build-order"
         (fun () -> t.sort_build_order <- true)
  ;;

  let create () =
    { do_not_modify_local_repo = false
    ; emacs                    = false
    ; sort_build_order         = false
    }
  ;;
end

module Cmd_show = struct
  type t =
    { mutable omit_completed_review         : bool
    ; mutable omit_unclean_workspaces_table : bool
    ; mutable show_compilation_status       : bool
    ; mutable show_feature_id               : bool
    ; mutable show_full_compilation_status  : bool
    ; mutable show_inheritable_attributes   : bool
    ; mutable show_lock_reasons             : bool
    }

  let update t =
    let open Make_client_config.Utils in
    empty
    +> no_arg "-show-compilation-status" (fun () -> t.show_compilation_status <- true)
    +> no_arg "-show-completed-review" (fun () -> t.omit_completed_review <- false)
    +> no_arg "-omit-completed-review" (fun () -> t.omit_completed_review <- true)
    +> no_arg "-show-full-compilation-status"
         (fun () -> t.show_full_compilation_status <- true)
    +> no_arg "-omit-unclean-workspaces-table"
         (fun () -> t.omit_unclean_workspaces_table <- true)
    +> no_arg "-show-feature-id" (fun () -> t.show_feature_id       <- true)
    +> no_arg "-show-inheritable-attributes"
         (fun () -> t.show_inheritable_attributes <- true)
    +> no_arg "-show-lock-reasons" (fun () -> t.show_lock_reasons     <- true)
  ;;

  let create () =
    { omit_completed_review         = false
    ; omit_unclean_workspaces_table = false
    ; show_compilation_status       = false
    ; show_feature_id               = false
    ; show_full_compilation_status  = false
    ; show_inheritable_attributes   = false
    ; show_lock_reasons             = false
    }
  ;;
end

module Cmd_star = struct
  type t =
    { mutable context : int option
    }

  let update t =
    let open Make_client_config.Utils in
    empty
    +> flag "-context" [%of_sexp: int] (fun int -> t.context <- Some int)
  ;;

  let create () =
    { context = None
    }
  ;;
end

module Cmd_todo = struct
  type t =
    { mutable do_not_show_cr_soons           : bool
    ; mutable do_not_show_unclean_workspaces : bool
    }

  let update t =
    let open Make_client_config.Utils in
    empty
    +> no_arg Switch.do_not_show_cr_soons
         (fun () -> t.do_not_show_cr_soons <- true)
    +> no_arg Switch.do_not_show_unclean_workspaces
         (fun () -> t.do_not_show_unclean_workspaces <- true)
  ;;

  let create () =
    { do_not_show_cr_soons           = false
    ; do_not_show_unclean_workspaces = false
    }
  ;;
end

module Cmd_wait_for_hydra = struct
  type t =
    { mutable update_local_repo : bool
    }

  let update t =
    let open Make_client_config.Utils in
    empty
    +> no_arg "-do-not-modify-local-repo"
         (fun () -> t.update_local_repo <- false)
    +> no_arg "-update-local-repo"
         (fun () -> t.update_local_repo <- true)
  ;;

  let create () =
    { update_local_repo = false
    }
  ;;
end

let unix_wordexp_resolve string =
  match Core.Unix.wordexp with
  | Error _ -> string
  | Ok wordexp ->
    match wordexp string with
    | [| string |] -> string
    | _ -> string
;;

module Workspace_config = struct
  module Are_enabled = struct
    type t =
      [ `default
      | `set_to of bool
      ]
    [@@deriving of_sexp]

    let t_of_sexp : Sexp.t -> t = function
      | Atom "true"  -> `set_to true
      | Atom "false" -> `set_to false
      | sexp -> t_of_sexp sexp
    ;;
  end

  module Statement = struct
    type t =
      [ `are_enabled      of Are_enabled.t
      | `auto_update_clean_workspaces_is_enabled of bool
      | `basedir          of string (* resolved to an [Abspath.t] during [update] *)
      | `do_not_distclean of Feature_path.Stable.V1.Set.t
      | `do_not_auto_update of Feature_path.Stable.V1.Set.t
      | `unclean_workspaces_detection_is_enabled of bool
      | `unclean_workspaces_detection_max_concurrent_jobs of int
      | `unclean_workspaces_detection_includes_shelved_changes of bool
      ]
    [@@deriving of_sexp]
  end

  type t =
    { mutable are_enabled : Are_enabled.t
    ; mutable auto_update_clean_workspaces_is_enabled : bool
    ; mutable basedir : Abspath.t
    ; mutable do_not_distclean : Feature_path.Hash_set.t
    ; mutable do_not_auto_update : Feature_path.Hash_set.t
    ; mutable unclean_workspaces_detection_is_enabled : bool
    ; mutable unclean_workspaces_detection_max_concurrent_jobs : int
    ; mutable unclean_workspaces_detection_includes_shelved_changes : bool
    }

  let create () =
    { are_enabled      = `default
    ; auto_update_clean_workspaces_is_enabled = false
    ; basedir          =
        concat [ "/usr/local/home/"
               ; User_name.to_string User_name.unix_login
               ; "/workspaces"
               ]
        |> Abspath.of_string
    ; do_not_distclean = Feature_path.Hash_set.create ()
    ; do_not_auto_update = Feature_path.Hash_set.create ()
    ; unclean_workspaces_detection_is_enabled = false
    ; unclean_workspaces_detection_max_concurrent_jobs = 5
    ; unclean_workspaces_detection_includes_shelved_changes = true
    }
  ;;

  (* If some user has a workspace section in their .ferc we want this to be enough to
     enable workspaces, unless specified otherwise.  In the common files that are loaded,
     we just have to mention an instruction {[ (are_enabled default) ]} to prevent the
     common file from enabling the workspaces for everyone. *)
  let maybe_auto_enable t =
    match t.are_enabled with
    | `default -> t.are_enabled <- `set_to true
    | `set_to _ -> ()
  ;;

  let update t = function
    | `are_enabled are_enabled -> t.are_enabled <- are_enabled
    | `auto_update_clean_workspaces_is_enabled is_enabled ->
      t.auto_update_clean_workspaces_is_enabled <- is_enabled
    | `basedir basedir -> t.basedir <- Abspath.of_string (unix_wordexp_resolve basedir)
    | `do_not_distclean set -> Set.iter set ~f:(Hash_set.add t.do_not_distclean)
    | `do_not_auto_update set -> Set.iter set ~f:(Hash_set.add t.do_not_auto_update)
    | `unclean_workspaces_detection_is_enabled is_enabled ->
      t.unclean_workspaces_detection_is_enabled <- is_enabled
    | `unclean_workspaces_detection_max_concurrent_jobs max_concurrent_jobs ->
      t.unclean_workspaces_detection_max_concurrent_jobs <- max_concurrent_jobs
    | `unclean_workspaces_detection_includes_shelved_changes value ->
      t.unclean_workspaces_detection_includes_shelved_changes <- value
  ;;
end

module Directory_order = struct
  type t = { mutable paths : Path_in_repo.t list list }

  let create () = { paths = [] }

  let update t paths = t.paths <- paths :: t.paths
end

module M = struct
  let always_loaded_if_present =
    if am_functional_testing
    then []
    else [ Abspath.extend Iron_config.prod_etc (File_name.of_string "ferc") ]
  ;;

  let home_basename = ".ferc"

  type t =
    { cmd_create                                           : Cmd_create.t
    ; cmd_list                                             : Cmd_list.t
    ; cmd_obligations_show                                 : Cmd_obligations_show.t
    ; cmd_rebase                                           : Cmd_rebase.t
    ; cmd_review                                           : Cmd_review.t
    ; cmd_show                                             : Cmd_show.t
    ; cmd_star                                             : Cmd_star.t
    ; cmd_todo                                             : Cmd_todo.t
    ; cmd_wait_for_hydra                                   : Cmd_wait_for_hydra.t
    ; directory_order                                      : Directory_order.t
    ; mutable may_infer_feature_path_from_current_bookmark : bool
    ; mutable pager_for_review                             : string option
    ; mutable send_push_events_to_server                   : bool
    ; mutable show_commit_session_warning                  : bool
    ; workspaces                                           : Workspace_config.t
    }
  [@@deriving fields]

  let directory_order t = t.directory_order.paths

  module Statement = struct
    type cmd =
      [ `create
      | `list
      | `obligations_show
      | `rebase
      | `review
      | `show
      | `star
      | `todo
      | `wait_for_hydra
      ]
    [@@deriving of_sexp]

    let cmd_of_sexp = function
      | Sexp.Atom "*" -> `star
      | Sexp.Atom str ->
        (Sexp.Atom (String.substr_replace_all str ~pattern:"-" ~with_:"_"))
        |> [%of_sexp: cmd]
      | Sexp.List [ Atom "obligations" ; Atom "show" ] -> `obligations_show
      | Sexp.List _ as sexp -> sexp |> [%of_sexp: cmd]
    ;;

    type t =
      [ `add_flag_to of cmd * Sexp.t list
      | `directory_order of Path_in_repo.t list
      | `may_infer_feature_path_from_current_bookmark of bool
      | `pager_for_review of string
      | `send_push_events_to_server of bool
      | `show_commit_session_warning of bool
      | `workspaces of Sexp.t list
      ]
    [@@deriving of_sexp]

    let t_of_sexp = function
      | Sexp.List (Sexp.Atom "add_flag_to" :: cmd :: args) ->
        `add_flag_to (cmd_of_sexp cmd, args)
      | sexp -> t_of_sexp sexp
    ;;
  end

  let create () =
    { cmd_create = Cmd_create.create ()
    ; cmd_list   = Cmd_list.create ()
    ; cmd_obligations_show = Cmd_obligations_show.create ()
    ; cmd_rebase = Cmd_rebase.create ()
    ; cmd_review = Cmd_review.create ()
    ; cmd_show   = Cmd_show.create ()
    ; cmd_star   = Cmd_star.create ()
    ; cmd_todo   = Cmd_todo.create ()
    ; cmd_wait_for_hydra = Cmd_wait_for_hydra.create ()
    ; directory_order = Directory_order.create ()
    ; may_infer_feature_path_from_current_bookmark = true
    ; pager_for_review = None
    ; send_push_events_to_server = am_functional_testing
    ; show_commit_session_warning = true
    ; workspaces = Workspace_config.create ()
    }
  ;;

  let update t (statement : Statement.t) =
    match statement with
    | `workspaces statements ->
      Workspace_config.maybe_auto_enable t.workspaces;
      let exns =
        List.filter_map statements ~f:(fun sexp ->
          (* We do that so that we can just ignore the statement that are broken but
             still honor the rest. *)
          try
            Workspace_config.update t.workspaces
              (sexp |> [%of_sexp: Workspace_config.Statement.t]);
            None
          with
          | exn -> Some exn)
      in
      if not (List.is_empty exns)
      then raise_s [%sexp "invalid workspaces statements", (exns : Exn.t list)]
    | `may_infer_feature_path_from_current_bookmark bool ->
      t.may_infer_feature_path_from_current_bookmark <- bool
    | `pager_for_review string -> t.pager_for_review <- Some (unix_wordexp_resolve string)
    | `send_push_events_to_server bool ->
      t.send_push_events_to_server <- bool
    | `show_commit_session_warning bool -> t.show_commit_session_warning <- bool
    | `add_flag_to (cmd, args) ->
      (match cmd with
       | `create -> Cmd_create.update t.cmd_create args
       | `list   -> Cmd_list.update   t.cmd_list   args
       | `obligations_show -> Cmd_obligations_show.update t.cmd_obligations_show args
       | `rebase -> Cmd_rebase.update t.cmd_rebase args
       | `review -> Cmd_review.update t.cmd_review args
       | `show   -> Cmd_show.update   t.cmd_show   args
       | `star   -> Cmd_star.update   t.cmd_star   args
       | `todo   -> Cmd_todo.update   t.cmd_todo   args
       | `wait_for_hydra -> Cmd_wait_for_hydra.update t.cmd_wait_for_hydra args)
    | `directory_order paths -> Directory_order.update t.directory_order paths
  ;;
end

include M
include Make_client_config.Make (M)

module Cmd = struct
  module Create = struct
    let reviewing t = t.cmd_create.reviewing
  end

  module List = struct
    let depth t = t.cmd_list.depth
  end

  module Obligations_show = struct
    let display_in_table t = t.cmd_obligations_show.display_in_table
  end

  module Rebase = struct
    let merge_tool t = t.cmd_rebase.merge_tool
  end

  module Review = struct
    let do_not_modify_local_repo t = t.cmd_review.do_not_modify_local_repo
    let emacs                    t = t.cmd_review.emacs
    let sort_build_order         t = t.cmd_review.sort_build_order
  end

  module Show = struct
    let omit_completed_review         t = t.cmd_show.omit_completed_review
    let omit_unclean_workspaces_table t = t.cmd_show.omit_unclean_workspaces_table
    let show_compilation_status       t = t.cmd_show.show_compilation_status
    let show_feature_id               t = t.cmd_show.show_feature_id
    let show_full_compilation_status  t = t.cmd_show.show_full_compilation_status
    let show_inheritable_attributes   t = t.cmd_show.show_inheritable_attributes
    let show_lock_reasons             t = t.cmd_show.show_lock_reasons
  end

  module Todo = struct
    let do_not_show_cr_soons           t = t.cmd_todo.do_not_show_cr_soons
    let do_not_show_unclean_workspaces t = t.cmd_todo.do_not_show_unclean_workspaces
  end

  module Wait_for_hydra = struct
    let update_local_repo t = t.cmd_wait_for_hydra.update_local_repo
  end

  let context t = t.cmd_star.context
end

module Workspaces = struct

  let readme () = "\
Workspaces automate the workflow of having a single hg share for each feature that one is
working on.  For more information, including how to start using workspaces, see:

  http://docs/app/fe/workspaces.html
"
  ;;

  let are_enabled t =
    match Iron_options.workspaces_are_enabled__forced_value with
    | Some value -> value
    | None ->
      match t.workspaces.are_enabled with
      | `default -> false
      | `set_to bool -> bool
  ;;

  let auto_update_clean_workspaces_is_enabled t =
    are_enabled t && t.workspaces.auto_update_clean_workspaces_is_enabled
  ;;

  let unclean_workspaces_detection_is_enabled t =
    are_enabled t && t.workspaces.unclean_workspaces_detection_is_enabled
  ;;

  let unclean_workspaces_detection_max_concurrent_jobs t =
    t.workspaces.unclean_workspaces_detection_max_concurrent_jobs
  ;;

  let unclean_workspaces_detection_includes_shelved_changes t =
    t.workspaces.unclean_workspaces_detection_includes_shelved_changes
  ;;

  let get_exn t =
    if are_enabled t
    then t.workspaces
    else (
      let message =
        [ "\
You do not have fe workspaces set up.  See the README below for more information.

"
        ; readme ()
        ; (match errors () with
           | [] -> ""
           | errors ->
             concat [ "\

Your .ferc is currently invalid, which might be the cause:

"
                    ; errors |> [%sexp_of: Error.t list] |> Sexp.to_string_hum
                    ; "\n"
                    ; "\

Fix your .ferc and check it with:

  $ fe tools validate-ferc

"
                    ])
        ; "\

Workspace not set up"
        ]
      in
      failwith (concat message))
  ;;

  let are_enabled_exn t = ignore (get_exn t : Workspace_config.t)

  let basedir t = (get_exn t).basedir

  let do_not_distclean t =
    (get_exn t).do_not_distclean
    |> Hash_set.to_list
    |> Feature_path.Set.of_list
  ;;

  let do_not_auto_update t =
    (get_exn t).do_not_auto_update
    |> Hash_set.to_list
    |> Feature_path.Set.of_list
  ;;
end
