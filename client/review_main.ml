open Core
open Async
open Pdiff4.Std
open Import

type result =
  [ `Reviewed
  | `Commit_session
  | `Quit
  ]
[@@deriving sexp_of]

let available_view_ids hunk = List.map (Hunk.views hunk) ~f:Diff_algo.View.id
;;

let available_views hunk = List.map (Hunk.views hunk) ~f:(fun view ->
  Diff_algo.View.id view, view
)
;;

let default_view_configuration =
  lazy (
    let module Patdiff4_config = Pdiff4.Std.User_config in
    Patdiff4_config.get ()
    |> Patdiff4_config.view_configuration)
;;

let with_default_view_configuration = function
  | None -> Lazy.force default_view_configuration
  | Some configuration -> configuration
;;

module type M = sig
  type t
  val reviewed                  : t list -> unit Deferred.t
  val path_in_repo              : t -> Path_in_repo.t
  val num_lines_in_diff         : (t -> int) option
  val always_open_file_in_emacs : bool
  val open_file_in_emacs        : (t -> unit Deferred.t) option
  val may_commit_session        : bool
end

type 'a file =
  { index_in_review   : int
  ; file              : 'a
  ; name              : string
  ; num_lines_in_diff : int option
  ; hunks             : Hunk.t Review_ring.t Or_error.t Deferred.t
  }

type view_configuration = Diff_algo.Id.t list Diff4_class.Map.t

type 'a t =
  { file_ring                    : 'a file Review_ring.t
  ; m                            : (module M with type t = 'a)
  ; number_of_files              : int
  }

let find_view_configuration view_configuration diff4_class =
  Option.value ~default:[]
    (Map.find view_configuration diff4_class)
;;

let hunk_view_configuration view_configuration hunk =
  find_view_configuration view_configuration (Hunk.diff4_class hunk)
;;

let get_and_update_view_configuration view_configuration hunk =
  let diff4_class = Hunk.diff4_class hunk in
  let configuration = find_view_configuration view_configuration diff4_class in
  let update_configuration data =
    Map.add view_configuration ~key:diff4_class ~data
  in
  configuration, update_configuration
;;

let navigate ?view_configuration hunk direction =
  let view_configuration = with_default_view_configuration view_configuration in
  let configuration, update_configuration =
    get_and_update_view_configuration view_configuration hunk
  in
  match Review_select_view.navigate
          ~to_string:Diff_algo.Id.to_string
          ~available:(available_view_ids hunk)
          ~configuration
          direction
  with
  | None -> view_configuration
  | Some configuration -> update_configuration [configuration]
;;

let hunks_to_lines_with_configuration ?view_configuration hunks =
  let is_default = Option.is_none view_configuration in
  let view_configuration = with_default_view_configuration view_configuration in
  let hunks = List.map hunks ~f:(fun hunk ->
    let shown =
      Review_select_view.select
        ~to_string:Diff_algo.Id.to_string
        ~available:(available_views hunk)
        ~configuration:(hunk_view_configuration view_configuration hunk)
    in
    if is_default
    then { hunk with views = List.map shown ~f:snd ; view_ids_shown = All }
    else { hunk with view_ids_shown = Only (List.map shown ~f:fst) })
  in
  Hunk.list_to_lines hunks
;;

let show_hunks ?view_configuration hunks =
  hunks_to_lines_with_configuration ?view_configuration hunks
  |> Review_util.show_lines
;;

let rec hunk_by_hunk : type a.
  ?view_configuration:view_configuration
  -> a t -> a file Review_ring.Elt.t -> result Deferred.t
  = fun ?view_configuration t file_elt ->
    let hunk_by_hunk ?view_configuration () =
      hunk_by_hunk ?view_configuration t file_elt
    in
    let module M = (val t.m : M with type t = a) in
    let file = Review_ring.Elt.value file_elt in
    let%bind hunk_ring = file.hunks in
    let hunk_ring = ok_exn hunk_ring in
    match Review_ring.current hunk_ring with
    | None ->
      let%bind () = reviewed t [ file_elt ] in
      file_by_file t

    | Some hunk_elt ->
      let hunk = Review_ring.Elt.value hunk_elt in
      let view_ids = available_view_ids hunk in
      let%bind () = show_hunks ?view_configuration [ hunk ] in
      let hunk_index = Review_ring.Elt.index hunk_elt in
      let total = Review_ring.length hunk_ring in
      let module Choice = Review_util.Choice in
      let choices =
        [ Choice.quit
        ; Async_interactive.Choice.default (Choice.show_again   "hunk")
        ; Choice.reviewed     "hunk"
        ; Choice.not_reviewed "hunk"
        ; Choice.previous     "hunk"
        ; Choice.Mode.file_by_file
        ; Choice.Mode.global_diff
        ; Choice.Mode.selected_files
        ] in
      let choices =
        if M.may_commit_session
        then choices @ [ Choice.commit_session ]
        else choices
      in
      let choices =
        if List.length view_ids > 1
        then choices
             @ [ Async_interactive.Choice.create 'v' `View "Change the view configuration"
               ; Async_interactive.Choice.create '>' `Succ_view "Show next view available"
               ; Async_interactive.Choice.create '<' `Pred_view "Show previous view available"
               ]
        else choices
      in
      let question =
        sprintf
          "[%d/%d] %s : Hunk [%d/%d] Mark as reviewed?"
          file.index_in_review t.number_of_files file.name
          (succ hunk_index) total
      in
      (match%bind Async_interactive.ask_dispatch_with_help question choices with
       | `Reviewed ->
         Review_ring.delete hunk_ring hunk_elt;
         hunk_by_hunk ()

       | `Not_reviewed ->
         Review_ring.goto_next hunk_ring;
         hunk_by_hunk ()

       | `Previous ->
         Review_ring.goto_previous hunk_ring;
         hunk_by_hunk ()

       | `Show_again -> hunk_by_hunk ?view_configuration ()

       | `Quit -> return `Quit

       | `Commit_session -> return `Commit_session

       | `File_by_file   -> file_by_file t
       | `Global_diff    -> global_diff t
       | `Selected_files -> selected_files t

       | (`Succ_view | `Pred_view) as direction ->
         let view_configuration = navigate ?view_configuration hunk direction in
         hunk_by_hunk ~view_configuration ()

       | `View ->
         let view_configuration = with_default_view_configuration view_configuration in
         let configuration, update_configuration =
           get_and_update_view_configuration view_configuration hunk
         in
         match%bind
           Review_select_view.toggle
             ~menu_name:"Views available"
             ~to_string:Diff_algo.Id.to_string
             ~display_prefix_in_list:None
             ~available:view_ids
             ~configuration
         with
         | `Quit         -> return `Quit
         | `File_by_file -> file_by_file t
         | `Global_diff  -> global_diff t
         | `New_configuration configuration ->
           let view_configuration = update_configuration configuration in
           hunk_by_hunk ~view_configuration ())

and file_by_file : type a. a t -> result Deferred.t = fun t ->
  let module M = (val t.m : M with type t = a) in
  match Review_ring.current t.file_ring with
  | None -> return `Reviewed
  | Some file_elt ->
    let file = Review_ring.Elt.value file_elt in
    let%bind () =
      match M.open_file_in_emacs with
      | Some f when M.always_open_file_in_emacs -> f file.file
      | _ -> return ()
    in
    let%bind hunk_ring = file.hunks in
    let hunk_ring = ok_exn hunk_ring in
    let hunks = Review_ring.values hunk_ring in
    if List.is_empty hunks
    then (
      let%bind () = reviewed t [ file_elt ] in
      file_by_file t)
    else (
      let%bind () = show_hunks hunks in
      let hunk_by_hunk_options =
        let should_offer_hunk_by_hunk_mode =
          match hunks with
          | [] -> false
          | [ hunk ] -> List.length (Hunk.views hunk) > 1
          | _::_::_ -> true
        in
        if not should_offer_hunk_by_hunk_mode
        then []
        else [ Review_util.Choice.Mode.hunk_by_hunk ]
      in
      let navigate_options =
        let has_multiple_views hunk_elt =
          List.length (Hunk.views (Review_ring.Elt.value hunk_elt)) > 1
        in
        match List.find (Review_ring.to_list hunk_ring) ~f:has_multiple_views with
        | None -> []
        | Some first_hunk_with_multiple_views ->
          [ Async_interactive.Choice.create '>'
              (`Navigate (first_hunk_with_multiple_views, `Succ_view))
              "Enter hunk-by-hunk mode and show next view available"
          ; Async_interactive.Choice.create '<'
              (`Navigate (first_hunk_with_multiple_views, `Pred_view))
              "Enter hunk-by-hunk mode and show previous view available"
          ]
      in
      let emacs_options =
        match M.open_file_in_emacs with
        | Some f when not M.always_open_file_in_emacs ->
          [ Async_interactive.Choice.create 'e'
              (`Open_file_in_emacs f) "Open current file in Emacs"
          ]
        | _ -> []
      in
      let module Choice = Review_util.Choice in
      let commit_session_option =
        if M.may_commit_session
        then [ Choice.commit_session ]
        else []
      in
      let question =
        let index = file.index_in_review in
        sprintf "[%d/%d] %s Mark as reviewed?"
          index t.number_of_files file.name
      in
      (match%bind
         Async_interactive.ask_dispatch_with_help question (
           [ Async_interactive.Choice.default (Choice.show_again "file")
           ; Choice.reviewed       "file"
           ; Choice.not_reviewed   "file"
           ; Choice.previous       "file"
           ]
           @ emacs_options
           @ hunk_by_hunk_options
           @ navigate_options
           @ commit_session_option
           @
           [ Choice.Mode.global_diff
           ; Choice.Mode.selected_files
           ; Choice.quit
           ])
       with
       | `Quit -> return `Quit
       | `Commit_session -> return `Commit_session
       | `Reviewed ->
         let%bind () = reviewed t [ file_elt ] in
         file_by_file t
       | `Not_reviewed ->
         Review_ring.goto_next t.file_ring;
         file_by_file t
       | `Previous ->
         Review_ring.goto_previous t.file_ring;
         file_by_file t
       | `Open_file_in_emacs open_file ->
         let file = Review_ring.Elt.value file_elt in
         let%bind () = open_file file.file in
         file_by_file t
       | `Hunk_by_hunk   -> hunk_by_hunk t file_elt
       | `Navigate (hunk, ((`Succ_view | `Pred_view) as direction)) ->
         let view_configuration = navigate (Review_ring.Elt.value hunk) direction in
         ok_exn (Review_ring.goto hunk_ring hunk);
         hunk_by_hunk ~view_configuration t file_elt
       | `Show_again     -> file_by_file t
       | `Global_diff    -> global_diff t
       | `Selected_files -> selected_files t))

and multiple_files
  : type a. a t -> after_review_goto: [ `File_by_file | `Selected_files ]
  -> a file Review_ring.Elt.t list
  -> result Deferred.t
  = fun t ~after_review_goto elements ->
    let module M = (val t.m : M with type t = a) in
    let files = List.map elements ~f:Review_ring.Elt.value in
    let%bind hunks =
      Deferred.List.all (List.map files ~f:(fun file -> file.hunks))
    in
    let hunks = List.concat_map hunks ~f:(fun hunk -> Review_ring.values (ok_exn hunk)) in
    if List.is_empty hunks
    then return `Reviewed
    else (
      let rec loop () =
        let%bind () = show_hunks hunks in
        let%bind () = Async_interactive.print_endline "Selection:" in
        let%bind () =
          Deferred.List.iter files ~f:(fun file ->
            Async_interactive.printf "   [ ] %s\n"
              (Path_in_repo.to_string (M.path_in_repo file.file)))
        in
        let module Choice = Review_util.Choice in
        let%bind action =
          Async_interactive.ask_dispatch_with_help
            (sprintf "Mark the %d files selected as reviewed?" (List.length files))
            [ Async_interactive.Choice.default (Choice.show_again "selection")
            ; Choice.reviewed     "selection"
            ; Choice.not_reviewed "selection"
            ; Choice.Mode.file_by_file
            ; Choice.Mode.global_diff
            ; Choice.Mode.selected_files
            ; Choice.quit
            ]
        in
        let after_review () =
          match after_review_goto with
          | `File_by_file   -> file_by_file t
          | `Selected_files -> selected_files t
        in
        match action with
        | `Quit           -> return `Quit
        | `Reviewed       -> let%bind () = reviewed t elements in after_review ()
        | `Not_reviewed   -> after_review ()
        | `Show_again     -> loop ()
        | `File_by_file   -> file_by_file t
        | `Selected_files -> selected_files t
        | `Global_diff    -> global_diff t
      in
      loop ())

and selected_files : type a. a t -> result Deferred.t
  = fun t ->
    let files = Review_ring.to_list t.file_ring in
    let display_prefix_in_list = Some (fun file ->
      match (Review_ring.Elt.value file).num_lines_in_diff with
      | None -> ""
      | Some int -> Int.to_string_hum int)
    in
    let to_string file = (Review_ring.Elt.value file).name in
    let configuration = [] in
    match%bind
      Review_select_view.toggle
        ~menu_name:"Files to review"
        ~to_string
        ~display_prefix_in_list
        ~available:files ~configuration
    with
    | `Quit         -> return `Quit
    | `File_by_file -> file_by_file t
    | `Global_diff  -> global_diff t
    | `New_configuration files ->
      multiple_files t ~after_review_goto:`Selected_files files

and global_diff : type a. a t ->  result Deferred.t = fun t ->
  multiple_files t ~after_review_goto:`File_by_file (Review_ring.to_list t.file_ring)

and reviewed : type a. a t -> a file Review_ring.Elt.t list -> unit Deferred.t
  = fun t files ->
    let module M = (val t.m : M with type t = a) in
    let selection_to_review =
      List.map files ~f:(fun file -> (Review_ring.Elt.value file).file)
    in
    List.iter files ~f:(fun elt -> Review_ring.delete t.file_ring elt);
    M.reviewed selection_to_review
;;

let files (type t) (module M : M with type t = t) files =
  let number_of_files = List.length files in
  let files = List.mapi files ~f:(fun index (file, hunks) ->
    { index_in_review = index + 1
    ; file
    ; hunks = (hunks >>|? Review_ring.create)
    ; name = Path_in_repo.to_string (M.path_in_repo file)
    ; num_lines_in_diff = Option.map M.num_lines_in_diff ~f:(fun f -> f file)
    }
  ) in
  let t =
    { file_ring = Review_ring.create files
    ; m = (module M)
    ; number_of_files
    }
  in
  let module Choice = Review_util.Choice in
  match%bind
    Async_interactive.ask_dispatch_with_help "How do you want to do this review?"
      ([ Async_interactive.Choice.default Choice.Mode.file_by_file
       ; Choice.Mode.global_diff
       ; Choice.Mode.selected_files
       ]
       @ (if M.may_commit_session
          then [ Choice.commit_session ]
          else []
         )
       @ [ Choice.quit
         ])
  with
  | `Quit           -> return `Quit
  | `Commit_session -> return `Commit_session
  | `File_by_file   -> file_by_file t
  | `Global_diff    -> global_diff t
  | `Selected_files -> selected_files t
;;

let hunks_to_lines hunks =
  hunks_to_lines_with_configuration ?view_configuration:None hunks
;;
