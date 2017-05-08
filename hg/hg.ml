(* So that all Iron hg commands run the same everywhere, we:

   - hardwire a particular version of hg
   - unset [DONT_LOAD_ANY_HGRC]
   - unset [HGMERGE]
   - set [HGUSER] if it wasn't already set
   - set [USE_THIS_HGRC_ONLY]
*)

module Stable = struct
  module Unstable = struct
    open! Import

    module Hash_consing = Hash_consing
  end

  open! Core.Core_stable
  open! Import_stable

  module Node_hash = struct
    module First_12 = struct
      module V1 = struct
        module Unshared = struct
          type t = string [@@deriving bin_io, compare, sexp]
          let module_name = "Node_hash.First_12"
          let hash = Core.String.hash
        end
        include Hash_consing.Make_stable_private (Unshared) ()

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
        ;;
      end
    end

    module V1 = struct
      module Unshared = struct
        type t = string [@@deriving bin_io, compare, sexp]
        let module_name = "Node_hash"
        let hash = Core.String.hash
      end
      include Hash_consing.Make_stable_private (Unshared) ()

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
      ;;
    end
  end

  module Rev = struct

    module Human_readable = struct
      module V1 = struct
        module Unshared = struct
          type t = string option [@@deriving bin_io, compare, sexp]
          let module_name = "Rev.Human_readable"
          let hash_0 = Hashtbl.hash 0
          let hash = function
            | None -> hash_0
            | Some str -> Core.String.hash str
        end
        include Hash_consing.Make_stable_private (Unshared) ()

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| b5b7561677d422cca0151f57e49d1c9b |}]
        ;;
      end
    end

    module V1 = struct
      module Unshared = struct
        type t =
          { human_readable : Human_readable.V1.t
          ; node_hash      : Node_hash.V1.t
          }
        [@@deriving bin_io, compare, fields, sexp]
        let module_name = "Rev"
        let hash { human_readable; node_hash } =
          Unstable.Hash_consing.fold_hash
            (Human_readable.V1.hash human_readable)
            (Node_hash.V1.hash node_hash)
        ;;
      end
      include Hash_consing.Make_stable_private (Unshared) ()
      let human_readable (t : t) = (t :> Unshared.t).human_readable
      let node_hash (t : t) : Node_hash.V1.t = (t :> Unshared.t).node_hash

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f8f4454776dedcc18922e2c2cdc5cf4d |}]
      ;;
    end
  end

  module Tag = struct
    module V1 = struct
      type t = string [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
      ;;
    end

    module Model = V1
  end

end

open Core
open Async
open! Import

module Node_hash = struct

  module Stable = Stable.Node_hash

  module First_12 = struct
    module Stable = Stable.First_12

    module T = Stable.V1

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)

    let to_string (t : t) = (t :> string)

    let length = 12

    let invariant (t : t) =
      Invariant.invariant [%here] t [%sexp_of: t]
        (fun () -> check_hash_exn (t :> string) ~length)
    ;;

    let of_string t =
      let t = shared_t t in
      invariant t;
      t
    ;;
  end

  module T = Stable.V1

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let to_string (t : t) = (t :> string)

  let to_first_12 (t : t) =
    String.sub (t :> string) ~pos:0 ~len:First_12.length
    |> First_12.shared_t
  ;;

  let length = 40

  let to_file_name (t : t) = File_name.of_string (t :> string)

  let invariant (t : t) =
    Invariant.invariant [%here] t [%sexp_of: t]
      (fun () -> check_hash_exn (t :> string) ~length)
  ;;

  let of_string t =
    let t = shared_t t in
    invariant t;
    t
  ;;
end

module Tag = struct
  module Stable = Stable.Tag

  include Stable.Model
  include Comparable.Make_plain(Stable.Model)

  let to_string = Fn.id
  let of_string = Fn.id

  let is_nice_name t ~for_tree_rooted_at =
    let regex =
      ok_exn
        (Regex.create (concat [ "^"
                              ; Regex.escape (Feature_name.to_string for_tree_rooted_at)
                              ; "-[0-9]+\\.[0-9]+"
                              ]))
    in
    Regex.matches regex t
  ;;

  let%test_unit _ =
    List.iter ~f:(fun (t, for_tree_rooted_at, expect) ->
      if not (Bool.equal expect
                (is_nice_name t
                   ~for_tree_rooted_at:(Feature_name.of_string for_tree_rooted_at)))
      then
        raise_s
          [%sexp "mismatch"
               , { t : t
                 ; for_tree_rooted_at : string
                 ; expect : bool
                 }])
      [ "jane-111.13"                                  , "jane"        , true
      ; "foo-jane-111.13"                              , "jane"        , false
      ; "jane-111.13"                                  , "trader-tools", false
      ; "jane-111.22+01-update-locked-down-projections", "jane"        , true
      ]
  ;;
end

let dont_load_any_hgrc = "DONT_LOAD_ANY_HGRC"
(* This hgrc is the [hgrc] file in this directory.  It is deployed along with the Iron
   executable and should be thought of as part of the executable, since it affects the
   semantics of [hg] commands run here.

   We unset [DONT_LOAD_ANY_HGRC] on the off chance that someone runs Iron in a context
   where it is set.  We don't want it set, because it would disable the hgrc that Iron
   wants, set via [USE_THIS_HGRC_ONLY] below. *)
let hgrc =
  lazy (
    Unix.unsetenv dont_load_any_hgrc;
    let%map config = force Iron_config.as_per_IRON_CONFIG in
    Iron_config.hgrc config
  )
;;

let verbose = Verbose.hg

let node_template = "{node}\\n"

type _ how =
  | Capture_stdout : string           how
  | Capture_output : Process.Output.t how
  | Create_process : Process.t        how
  | Share_io       : unit             how
;;

let hg_executable =
  "/j/office/app/hg/versions/3.8.2+hgrc_env_vars+share_cache_fix+eliminate_bookmark_race/bin/hg"
;;

let hg_user_env_var = "HGUSER"

let hg_user_value =
  lazy_deferred (fun () ->
    if is_some (Sys.getenv hg_user_env_var)
    then return `Already_set
    else (
      let user = User_name.(to_string unix_login) in
      let email = sprintf "%s@janestreet.com" user in
      match%map Unix.Passwd.getbyname user with
      | None -> `Inferred email
      | Some { gecos; _ } -> `Inferred (sprintf "%s <%s>" gecos email)))
;;

let env () =
  let%bind hgrc = force hgrc in
  let%map hg_user_value = hg_user_value () in
  let python_path_var = "PYTHONPATH" in
  let python_path =
    let pyre2_dir =
      "/j/office/app/hg/extra-python-libs/pyre2/build/lib.linux-x86_64-2.6"
    in
    match Sys.getenv python_path_var with
    | None -> pyre2_dir
    | Some old_python_path -> pyre2_dir ^ ":" ^ old_python_path
  in
  Unix.unsetenv "HGMERGE";
  let env_alist =
    [ "USE_THIS_HGRC_ONLY", Abspath.to_string hgrc
    ; python_path_var, python_path
    ]
    @ (match hg_user_value with
      | `Already_set -> []
      | `Inferred hg_user_value -> [ hg_user_env_var, hg_user_value ])
  in
  `Extend env_alist
;;

let hg_with_optional_repo_root :
  type a .
  how : a how
  (* [accept_nonzero_exit] is used by [Capture_stdout] and [Share_io], but not by
     [Capture_output] or [Create_process]. *)
  -> ?accept_nonzero_exit : int list
  -> ?repo_root : Repo_root.t
  -> string
  -> args : string list
  -> a Or_error.t Deferred.t =
  fun ~how
    ?(accept_nonzero_exit = [])
    ?repo_root subcommand ~args ->
    if verbose
    then Verbose.message "hg"
           (`subcommand subcommand, `args args)
           [%sexp_of: [ `subcommand of string ] * [ `args of string list ]];
    let%bind env = env () in
    let args =
      let args = subcommand :: args in
      if Iron_options.silence_uninteresting_hg_warnings
      then args @
           [ "--config"
           ; "extensions.silence_lock_message=\
              /j/office/app/hg/prod/jhg/silence_lock_message_v2.py"
           ]
      else args
    in
    let working_dir = Option.map repo_root ~f:Repo_root.to_string in
    match how with
    | Capture_stdout ->
      (Process.run ~accept_nonzero_exit ?working_dir ~prog:hg_executable
         ~args ~env () : a Or_error.t Deferred.t)
    | Capture_output ->
      (match%bind Process.create ?working_dir ~prog:hg_executable ~args ~env () with
       | Error _ as e -> return e
       | Ok process ->
         let%map output = Process.collect_output_and_wait process in
         Ok output)
    | Create_process ->
      Process.create ?working_dir ~prog:hg_executable ~args ~env ()
    | Share_io ->
      let cwd =
        match working_dir with
        | None -> []
        | Some working_dir -> [ "--cwd"; working_dir ]
      in
      let%bind pid =
        Unix.fork_exec ~env ~prog:hg_executable ~argv:(hg_executable :: (args @ cwd)) ()
      in
      let%map wait_result = Unix.waitpid pid in
      match wait_result with
      | Error (`Exit_non_zero n) when List.mem accept_nonzero_exit n ~equal:Int.equal
        -> Ok ()
      | _ ->
        match Unix.Exit_or_signal.or_error wait_result with
        | Ok () -> Ok ()
        | Error e -> error "hg call failed" (e, hg_executable, args)
                       [%sexp_of: Error.t * string * string list]
;;

let hg ~how ?accept_nonzero_exit ~repo_root subcommand ~args =
  hg_with_optional_repo_root ~how ?accept_nonzero_exit ~repo_root subcommand ~args
;;

let is_ok = function
  | Ok _    -> true
  | Error _ -> false
;;

let one_line = function
  | Error e -> `Error e
  | Ok string ->
    match String.split_lines string with
    | [ line ] -> `One_line line
    | ([] | _ :: _ :: _ as lines) -> `Not_one_line lines
;;

module Rev = struct

  include Stable.Rev.V1
  let sexp_of_t_round_trippable = sexp_of_t
  let sexp_of_t = `Redefined_below
  let _ = sexp_of_t
  let compare = `Do_not_use
  let equal   = `Do_not_use

  module Human_readable = struct
    include Stable.Rev.Human_readable.V1

    let unshared (t : t) = (t :> Unshared.t)
    let is_none t = Option.is_none (unshared t)
    let some str = shared_t (Some str)
    let none = shared_t None
  end

  let invariant (t : t) =
    Invariant.invariant [%here] t sexp_of_t_round_trippable (fun () ->
      let check f = Invariant.check_field (t :> Unshared.t) f in
      Unshared.Fields.iter
        ~human_readable:ignore
        ~node_hash:(check Node_hash.invariant))
  ;;

  let to_first_12 t = Node_hash.to_first_12 (node_hash t)

  let to_string_12 t = Node_hash.First_12.to_string (to_first_12 t)
  let to_string_40 t = Node_hash.to_string (node_hash t)

  let of_node_hash node_hash =
    shared_t { Unshared. node_hash; human_readable = Human_readable.none }
  ;;

  let of_string_40 s = of_node_hash (Node_hash.of_string s)

  let with_human_readable t ~human_readable =
    shared_t { Unshared.
               node_hash      = node_hash t
             ; human_readable = Human_readable.some human_readable
             }
  ;;

  let without_human_readable t =
    if Human_readable.is_none (human_readable t)
    then t
    else of_node_hash (node_hash t)
  ;;

  let to_string_hum ?(lossy = false) t =
    match Human_readable.unshared (human_readable t) with
    | None -> to_string_12 t
    | Some human_readable ->
      let f12 = to_string_12 t in
      if String.is_prefix ~prefix:f12 human_readable
      || String.equal human_readable "."
      || String.equal human_readable "(.)"
      then f12
      else
      if lossy
      then human_readable
      else String.concat [ human_readable; " ["; f12; "]" ]
  ;;

  let sexp_of_t t = sexp_of_string (to_string_hum t)
  let has_prefix t first_12 = Node_hash.First_12.equal (to_first_12 t) first_12

  module Compare_by_hash = struct
    module T = struct
      type nonrec t = t [@@deriving sexp]
      let compare t1 t2 = Node_hash.compare (node_hash t1) (node_hash t2)
      let hash t = Node_hash.hash (node_hash t)
    end
    include T
    include Comparable.Make (T)
    include Hashable.  Make (T)
    let invariant = invariant
  end

  let equal_node_hash = Compare_by_hash.equal

  module Stable = Stable.Rev

  let human_readable t = Human_readable.unshared_t (human_readable t)
end

module Bookmark = struct
  type t =
    | Feature of Feature_path.t
    | Release of Feature_path.t
  [@@deriving sexp_of]

  let to_string = function
    | Feature feature_path -> Feature_path.to_string feature_path
    | Release feature_path ->
      concat [ "[release]"; Feature_path.to_string feature_path ]
  ;;
end

module Revset = struct
  type t =
    | Rev of Rev.t
    | Numeric_rev of int
    | String of string
    | Bookmark of Bookmark.t
    | Tag of Tag.t
    | All_tags
    | Range of Rev.t * Rev.t
    | And of t list
    | Or  of t list
    | Not of t
    | Difference of t * t
    | Ancestors of t
    | Descendants of t
    | First_greatest_common_ancestor of Rev.t list
    | Limit of t * int
    | Merge
    | Reverse of t
    | Min of t
    | Max of t
  [@@deriving sexp_of]

  let of_string str = String str

  let dot = of_string "."

  let tag tag = Tag tag

  let all_tags = All_tags

  let of_rev rev = Rev rev

  let merge = Merge

  let reverse t = Reverse t

  let min t = Min t

  let max t = Max t

  let difference t1 t2 = Difference (t1, t2)

  let and_ ts = And ts

  let or_ ts = Or ts

  let not t = Not t

  let ancestors t = Ancestors t

  let descendants t = Descendants t

  let for_feature ~base ~tip =
    difference (ancestors (of_rev tip)) (ancestors (of_rev base))
  ;;

  let feature_tip feature_path = Bookmark (Feature feature_path)

  let bookmark b = Bookmark b

  let range ~from ~to_ = Range (from, to_)

  let first_greatest_common_ancestor revs = First_greatest_common_ancestor revs

  let limit t ~limit = Limit (t, limit)

  let to_string = function
    | String str -> str (* the toplevel of the revset is parsed differently, so let's
                           avoid adding useless parens that change the parsing *)
    | t ->
      let rec nullary ac op = op :: "()" :: ac
      and unary t ac op = op :: "(" :: to_strings t (")" :: ac)
      and binary t1 t2 ac op =
        "(" :: to_strings t1 (" " :: op :: " " :: to_strings t2 (")" :: ac))
      and n_ary ts ac op =
        let op = concat [ " "; op; " " ] in
        "("
        :: (snd
              (List.fold (List.rev ts) ~init:(false, (")":: ac))
                 ~f:(fun (insert_op, ac) t ->
                   let ac =
                     if insert_op
                     then op :: ac
                     else ac
                   in
                   (true, to_strings t ac))))
      and to_strings t ac =
        match t with
        | Rev rev -> "id(" :: Rev.to_string_40 rev :: ")" :: ac
        | Numeric_rev d -> "rev(" :: Int.to_string d :: ")" :: ac
        | String str -> "(" :: str :: ")" :: ac
        | Bookmark b -> "bookmark(\"" :: Bookmark.to_string b :: "\")" :: ac
        | Tag tag -> "tag(\"literal:" :: tag :: "\")" :: ac
        | All_tags -> "tag()" :: ac
        | Range (from, to_) -> Rev.to_string_40 from :: "::" :: Rev.to_string_40 to_ :: ac
        | Merge -> nullary ac "merge"
        | Ancestors   t -> unary t ac "ancestors"
        | Descendants t -> unary t ac "descendants"
        | Min         t -> unary t ac "min"
        | Max         t -> unary t ac "max"
        | Reverse     t -> unary t ac "reverse"
        | Difference (t1, t2) -> binary t1 t2 ac "-"
        | And ts -> n_ary ts ac "and"
        | Or  ts -> n_ary ts ac "or"
        | Not t -> "(not " :: to_strings t (")" :: ac)
        | First_greatest_common_ancestor revs ->
          "ancestor("
          :: (List.intersperse ~sep:", " (List.map revs ~f:Rev.to_string_40)
              @ (")" :: ac))
        | Limit (t, limit) ->
          "limit(" :: to_strings t (", " :: Int.to_string limit :: ")" :: ac)
      in
      concat (to_strings t [])
  ;;
end

let diff repo_root ~from ~to_ ~args =
  let tail =
    match to_ with
    | `Working_copy -> args
    | `Rev rev -> "-r" :: Rev.to_string_40 rev :: args
  in
  hg ~repo_root ~how:Capture_stdout
    "diff" ~args:("-r" :: Rev.to_string_40 from :: tail)
;;

let log ?(args = []) ?template repo_root revset =
  let template =
    match template with
    | None -> []
    | Some template -> [ "--template"; template ]
  in
  hg ~repo_root "log" ~how:Capture_stdout
    ~args:([ "-r"; Revset.to_string revset ]
           @ template
           @ args)
;;

let output_to_revs =
  Or_error.map ~f:(fun output ->
    String.split_lines output
    |> List.map ~f:Rev.of_string_40)
;;

let create_revs ?restrict_to repo_root revset =
  let args =
    Option.value_map restrict_to ~default:[] ~f:(fun path ->
      [ Path_in_repo.to_string path ])
  in
  let%map output = log repo_root revset ~args ~template:node_template in
  output_to_revs output
;;

let create_rev ?human_readable repo_root revset =
  match%map create_revs repo_root revset with
  | Error _ as error -> error
  | Ok [ rev ] ->
    let human_readable =
      Option.value human_readable ~default:(Revset.to_string revset)
    in
    Ok (Rev.with_human_readable rev ~human_readable)
  | Ok revs ->
    error "[Hg.create_rev] got a revset not of size one" (revset, revs)
      [%sexp_of: Revset.t * Rev.t list]
;;

let create_rev_exn ?human_readable repo_root revset =
  match%map create_rev ?human_readable repo_root revset with
  | Ok rev    -> rev
  | Error err -> Error.raise err
;;

let create_rev_zero repo_root =
  create_rev_exn repo_root ~human_readable:"0" (Revset.Numeric_rev 0)
;;

let get_remote_rev remote_repo_path what_rev =
  let rev =
    (* [hg id] doesn't take revsets over ssh, it only accept simple forms: tags,
       bookmarks, revs, ids. *)
    match what_rev with
    | `Feature feature_path -> Feature_path.to_string feature_path
    | `Numeric_rev n -> Int.to_string n
  in
  let%map stdout =
    hg_with_optional_repo_root
      "id" ~how:Capture_stdout
      ~args:[ "-r"; rev
            (* We tried using the "--full-id" switch, but that is a jane extension. *)
            ; "--id"
            ; Remote_repo_path.to_string remote_repo_path
            ]
  in
  match one_line stdout with
  | `One_line first_12 -> Node_hash.First_12.of_string first_12
  | `Error e ->
    raise_s
      [%sexp (sprintf "cannot find rev %s of remote repo" rev : string)
           , (remote_repo_path : Remote_repo_path.t)
           , (e : Error.t)]
  | `Not_one_line lines ->
    raise_s
      [%sexp (sprintf "wrong output while finding rev %s of remote repo" rev : string)
           , (remote_repo_path : Remote_repo_path.t)
           , `Lines (lines : string list)]
;;

let get_remote_rev_zero remote_repo_path =
  get_remote_rev remote_repo_path (`Numeric_rev 0)
;;

let ensure_local_repo_is_in_family ?remote_rev_zero repo_root remote_repo_path =
  let%bind first_12 =
    match remote_rev_zero with
    | Some rev -> return (Rev.to_first_12 rev)
    | None -> get_remote_rev_zero remote_repo_path
  in
  let%map rev_zero = create_rev_zero repo_root in
  if not (Node_hash.First_12.equal (Rev.to_first_12 rev_zero) first_12)
  then repo_mismatch remote_repo_path;
;;

(* [ensure_can_access_remote_repo] only checks that you have read permissions on the repo,
   not write permissions, which is usually what we need.  But usually, we give out
   read and write access together, so this isn't a problem in practice.

   To check for write permissions, we could do it without modifying the repo by seeing if
   we can pull from the repo to itself.  To do this, we would have to add some function
   like [hg] but that can take a [Remote_repo_path.t] and potentially run hg through ssh.

   E.g., to check for write permissions on jane/submissions:

   $ ssh hg hg --cwd /hg/jane/submissions pull .
   pulling from .
   searching for changes
   no changes found

   But, on a repo without write permissions:

   $ hg init test
   $ cd test
   $ touch a; hg add a; hg commit -m commit
   $ chmod -R a-w .
   $ hg pull .
   abort: could not lock repository /usr/local/home/jdoe/test: Permission denied
*)
let ensure_can_access_remote_repo remote_repo_path =
  let%bind (_ : Node_hash.First_12.t) = get_remote_rev_zero remote_repo_path in
  return ()
;;

module Cleanliness_witness : sig
  type t = private Repo_is_clean

  val status_cleanliness : ?repo_is_clean:t -> Repo_root.t -> t Or_error.t Deferred.t
end = struct
  type t = Repo_is_clean

  let status_cleanliness ?repo_is_clean repo_root =
    match repo_is_clean with
    | Some t -> return (Ok t)
    | None ->
      let%map result =
        Async_interactive.Job.run !"Checking cleanliness of %{Repo_root#hum}" repo_root
          ~f:(fun () -> hg ~repo_root "status" ~args:[] ~how:Capture_stdout)
      in
      let report = ok_exn result in
      if String.is_empty report
      then Ok Repo_is_clean
      else Or_error.error "hg repository is not clean"
             (`repo repo_root, `hg_status report)
             [%sexp_of: [ `repo of Repo_root.t ] * [ `hg_status of string ]]
end

let status_cleanliness = Cleanliness_witness.status_cleanliness

let status_cleanliness_exn ?repo_is_clean repo_root =
  let%map result = status_cleanliness ?repo_is_clean repo_root in
  ok_exn result
;;

let active_bookmark repo_root =
  let%map stdout = hg ~repo_root "active-bookmark" ~args:[] ~how:Capture_stdout in
  match one_line stdout with
  | `One_line line -> Ok line
  | `Error e -> Error e
  | `Not_one_line lines ->
    error "unexpected multi-line output from 'hg active-bookmark'" lines
      [%sexp_of: string list]
;;

let current_bookmark repo_root =
  if verbose
  then Verbose.message "current_bookmark" repo_root [%sexp_of: Repo_root.t];
  let path = Repo_root.to_string repo_root ^ "/.hg/bookmarks.current" in
  match%map
    Monitor.try_with ~extract_exn:true (fun () -> Reader.file_contents path)
  with
  | Error exn -> error "could not read bookmark file" exn [%sexp_of: exn]
  | Ok bookmark ->
    if not (String.is_empty bookmark)
    then Ok bookmark
    else error "there is no current bookmark" () [%sexp_of: unit]
;;

module Shelve_description = struct
  type t = string [@@deriving sexp_of]
end

let list_shelves_exn repo_root =
  match%map
    hg ~repo_root ~how:Capture_stdout "shelve"
      ~args:[ "--list"
            ; "--config"; "extensions.shelve="
            ]
  with
  | Ok stdout -> String.split_lines stdout
  | Error err ->
    raise_s
      [%sexp "cannot list the existing shelves"
           , (repo_root : Repo_root.t)
           , (err : Error.t)
      ]
;;

let bookmark_exists repo_root bookmark =
  let%map result = create_revs repo_root (Revset.bookmark bookmark) in
  is_ok result
;;

let revs_exist repo_root revs =
  match revs with
  | [] -> return true
  | _ :: _ ->
    let args = List.concat_map revs ~f:(fun rev -> [ "-r"; Rev.to_string_40 rev ]) in
    let%map result = hg ~repo_root "log" ~args:("-q" :: args) ~how:Capture_stdout in
    is_ok result
;;

let rev_exists repo_root rev = revs_exist repo_root [ rev ]

(* We use this to guard hg operations such as pull or push that work on two repos: the
   local one (a [t]) and a remote one (a [Remote_repo_path.t]). If these are the same
   repo, hg will hang trying to take a lock on "both" repos.

   It's impossible, in general, to tell if the two arguments reference the same repo.
   If they are both file names, we can use stat to see if they refer to the same
   inode. Thus, true means they are definitely the same. False means either they're
   definitely not the same or we couldn't figure out the answer. *)
let target_repo_is_root_repo repo_root remote =
  match remote with
  (* Can't really compare a filename & an ssh url. But probably distinct: *)
  | Remote_repo_path.Ssh  _ -> false
  | Remote_repo_path.File f -> Abspath.(=) f (Repo_root.to_abspath repo_root)
;;

let commit ?(metadata = String.Map.empty) repo_root ~message =
  let metadata_args =
    List.concat_map (String.Map.to_alist metadata) ~f:(fun (key,value) ->
      if String.mem key '='
      then raise_s [%sexp "Cannot have a metadata key with '=' in it"
                        , (metadata : string String.Map.t)];
      [ "--metadata"; key ^ "=" ^ value ])
  in
  let args = "-m" :: message :: metadata_args in
  hg ~repo_root "commit" ~args ~how:Share_io
;;

let distclean repo_root =
  Async_interactive.Job.run !"Distcleaning %{Repo_root#hum}" repo_root
    ~f:(fun () -> hg ~repo_root "distclean" ~args:[] ~how:Share_io)
;;

let first_greatest_common_ancestor repo_root rev1 rev2 =
  create_rev_exn repo_root
    (Revset.first_greatest_common_ancestor [ rev1; rev2 ])
    ~human_readable:
      (sprintf "ancestor(%s, %s)"
         (Rev.to_string_hum rev1 ~lossy:true)
         (Rev.to_string_hum rev2 ~lossy:true))
;;

let greatest_common_ancestors repo_root rev1 rev2 =
  let%map output =
    hg ~repo_root "gcas"
      ~args:[ "--config"; "extensions.gcas=/j/office/app/hg/prod/jhg/\
                           greatest_common_ancestors_v1.py"
            ; Rev.to_string_40 rev1
            ; Rev.to_string_40 rev2
            ]
      ~how:Capture_stdout
  in
  ok_exn (output_to_revs output)
;;

let greatest_common_ancestor repo_root rev1 rev2 =
  match%map greatest_common_ancestors repo_root rev1 rev2 with
  | [ gca ] -> gca
  | [] ->
    raise_s [%sexp "could not determine greatest common ancestors",
                   ([ rev1; rev2 ] : Rev.t list)]
  | _ :: _ :: _ as gcas ->
    raise_s [%sexp "multiple greatest common ancestors",
                   ([ rev1; rev2 ] : Rev.t list),
                   ("greatest common ancestors", (gcas : Rev.t list))]
;;

let is_ancestor repo_root ~ancestor ~descendant =
  if Rev.equal_node_hash ancestor descendant
  then return true
  else (
    let%map first_greatest_common_ancestor =
      create_rev_exn repo_root
        (Revset.first_greatest_common_ancestor [ ancestor; descendant ])
    in
    Rev.equal_node_hash first_greatest_common_ancestor ancestor);
;;

let list_bookmarks repo_root =
  let%map result = hg ~repo_root "bookmark" ~args:["-q"] ~how:Capture_stdout in
  result
  |> ok_exn
  |> String.split_lines
;;

let files ?include_ repo_root =
  let args =
    match include_ with
    | None -> []
    | Some path_in_repo -> [ "--include"; Path_in_repo.to_string path_in_repo ]
  in
  let%map output =
    hg ~repo_root ~accept_nonzero_exit:[1] "files" ~args ~how:Capture_stdout
  in
  output
  |> ok_exn
  |> String.split_lines
  |> List.map ~f:Path_in_repo.of_string
;;

let grep_conflicts_exn ?(below = Path_in_repo.root) repo_root ~grep_display_option =
  let%bind files_to_grep = files repo_root ~include_:below in
  let files_to_grep =
    List.map files_to_grep ~f:(fun file_to_grep ->
      ok_exn (Path_in_repo.chop_prefix ~prefix:below file_to_grep))
  in
  let xargs_args =
    [ "-d"; "\n"
    ; "-r"
    ; "grep"
    ; "--binary-files=without-match"
    ; "-s"; "^<<<<<<"
    ; grep_display_option
    ]
  in
  let working_dir =
    Abspath.to_string (Repo_root.append repo_root below)
  in
  let%bind grep_process =
    Process.create ~working_dir ~prog:"xargs" ~args:xargs_args ()
  in
  let grep_process = ok_exn grep_process in
  let grep_in = Process.stdin grep_process in
  Writer.set_buffer_age_limit grep_in `Unlimited;
  List.iter files_to_grep ~f:(fun file ->
    Writer.write_line grep_in (Relpath.to_string file));
  (* According to its man page, [xargs] returns status 123 if any of the
     [grep] processes returns an error (including "no match found"). *)
  let%map stdout =
    Process.collect_stdout_and_wait grep_process ~accept_nonzero_exit:[ 123 ]
  in
  ok_exn stdout
;;

let is_conflict_free_exn repo_root =
  let%map output = grep_conflicts_exn repo_root ~grep_display_option:"-l" in
  String.is_empty output
;;

let manifest repo_root source =
  let%map output =
    match source with
    | `Revset revset ->
      hg ~repo_root "manifest" ~args:[ "-r"; Revset.to_string revset ] ~how:Capture_stdout
    | `Dirstate ->
      (* We list what the manifest would be after [hg commit], so we include clean (-c),
         modified (-m), and added (-a), but not removed (-r), untracked (-u) or ignored
         (-i).  We do include files deleted (-d) from the working copy without telling hg,
         since those would still be in the manifest after [hg commit].  From [hg help
         status]:

         {v
           -a --added               show only added files
           -c --clean               show only files without changes
           -d --deleted             show only deleted (but tracked) files
           -i --ignored             show only ignored files
           -m --modified            show only modified files
           -n --no-status           hide status prefix
           -r --removed             show only removed files
           -u --unknown             show only unknown (not tracked) files
         v}

         This is actually very slightly wrong because a file that is both added and deleted
         from the working copy would have status 'deleted' but hg commit would be
         perfectly happy to succeed while ignoring this file.  *)
      hg ~repo_root "status" ~args:[ "-acdmn" ] ~how:Capture_stdout
  in
  let lines = String.split_lines (ok_exn output) in
  List.map lines ~f:Path_in_repo.of_string
;;

let outgoing repo_root (`Feature feature_path) ~to_ =
  Async_interactive.Job.run !"Checking for outgoing changesets in %{Repo_root#hum}" repo_root
    ~f:(fun () ->
      match%map
        hg ~repo_root "outgoing" ~how:Capture_output
          ~args:[ "-r"; Feature_path.to_string feature_path
                ; Remote_repo_path.to_string to_
                (* For people who use changeset evolution, not enabling it here will cause
                   spurious output on stderr.

                   On the other hand, it doesn't do any harm to enable it here, since
                   [hg outgoing] does not modify the repo.
                *)
                ; "--config"; "experimental.evolution=all"
                ]
      with
      | Error _ as err -> err
      | Ok output ->
        match output.exit_status with
        | Ok () -> Ok (`Hg_out (String.strip output.stdout))
        | Error (`Exit_non_zero 1) when String.is_empty output.stderr -> Ok `Nothing
        | Error _ ->
          error_s
            [%sexp
              "'hg outgoing' failed",
              { repo_root = (repo_root     : Repo_root.t)
              ; feature   = (feature_path  : Feature_path.t)
              ; process   = (output        : Process.Output.t)
              }
            ])
;;

let approximate_should_pull_revset repo_root (revset : Revset.t) =
  let doesnt_exist_locally () =
    let%map result = create_rev repo_root revset in
    is_error result
  in
  let always_pull = return true in
  match revset with
  | Rev _ -> doesnt_exist_locally ()
  | String string ->
    (* If the revision looks like a rev 40 or rev 12, let's avoid pulling needlessly. In
       theory, it could be a bookmark that happens to be only written with hexadecimal
       characters, but how likely is that? *)
    (match Node_hash.First_12.of_string string with
     | _ -> doesnt_exist_locally ()
     | exception _ ->
       match Rev.of_string_40 string with
       | _ -> doesnt_exist_locally ()
       | exception _ -> always_pull)
  | _ -> always_pull
;;

(* An "hg pull rev" fails if [rev] is already in the local repo.  This is bogus.  But
   there's no hg flag to make this ok.  So we workaround this: we do an initial test to
   see if [rev] is already present.  Note that this test is racy; too bad.

   We typically refuse to pull if the repo is unclean; override with [even_if_unclean]. *)
let pull ?(even_if_unclean = false) ?repo_is_clean repo_root ~from what_to_pull =
  if target_repo_is_root_repo repo_root from
  then return () (* Don't need to pull and pulling would lock up. *)
  else (
    let%bind should_pull =
      match what_to_pull with
      | `Feature _     -> return true
      | `Features l    -> return (not (List.is_empty l))
      | `Bookmarks l   -> return (not (List.is_empty l))
      | `Rev rev       -> rev_exists repo_root rev  |> Deferred.map ~f:not
      | `Revs revs     -> revs_exist repo_root revs |> Deferred.map ~f:not
      | `Revset revset -> approximate_should_pull_revset repo_root revset
      | `All_revs      -> return true
    in
    if not should_pull
    then return ()
    else (
      (* We delay the clean check until we know we're really going to pull. *)
      let%bind () =
        if even_if_unclean
        then return ()
        else (
          let%map Repo_is_clean = status_cleanliness_exn ?repo_is_clean repo_root in
          ())
      in
      let pull revs to_string to_string_hum =
        Async_interactive.Job.run "Pulling %s%s"
          (match revs with
           | [] -> "all revisions "
           | [ r ] -> concat [ to_string_hum r; " " ]
           | _ ->
             concat ("\n"
                     :: List.map revs ~f:(fun r ->
                       concat [ "  "; to_string_hum r; "\n" ])))
          (sprintf !"in %{Repo_root#hum}" repo_root)
          ~f:(fun () ->
            hg ~repo_root "pull" ~how:Share_io
              ~args:(List.concat_map revs ~f:(fun r -> [ "-r"; to_string r ])
                     @ [ "-q"
                       (* makes sure we create at most one divergent bookmark @default
                          and not @1, @2 etc. *)
                       ; "--config"; "paths.default=" ^ Remote_repo_path.to_string from
                       ]))
      in
      let pull_features fs  = pull fs Feature_path.to_string Feature_path.to_string in
      let pull_bookmarks bs = pull bs Bookmark.to_string     Bookmark.to_string     in
      let pull_revs     rs  = pull rs Rev.to_string_40       Rev.to_string_hum      in
      let pull_revset   rs  = pull rs Revset.to_string       Revset.to_string       in
      let%map pull_result =
        match what_to_pull with
        | `Feature feature_path   -> pull_features  [ feature_path ]
        | `Features feature_paths -> pull_features  feature_paths
        | `Bookmarks bookmarks    -> pull_bookmarks bookmarks
        | `Rev rev                -> pull_revs      [ rev ]
        | `Revs revs              -> pull_revs      revs
        | `Revset revset          -> pull_revset    [ revset ]
        | `All_revs               -> pull_revs      []
      in
      ok_exn pull_result))
;;

let parent repo_root =
  let fail (type a) msg (a : a) sexp_of_a =
    raise_s
      [%sexp
        (msg : string),
        { repo_root = (repo_root : Repo_root.t)
        ; error     = (a : a)
        }
      ]
  in
  let%map result =
    hg ~repo_root "parent" ~args:[ "--template"; node_template ] ~how:Capture_stdout
  in
  match one_line result with
  | `Error error -> fail "'hg parent' failed" error [%sexp_of: Error.t]
  | `Not_one_line output ->
    fail "'hg parent' gave unexpected output" output [%sexp_of: string list]
  | `One_line node_hash -> Rev.of_string_40 node_hash
;;

(* [hg push] exits status 1 if we don't push any changesets, even if we do push a
   bookmark.  That is a fairly common case, which we don't want to fail.  So we look for a
   particular string on stdout to detect this case. *)
let push_no_changes_pattern = lazy (String.Search_pattern.create "no changes found")

(* If [hg push -B $BOOK] fails to update bookmark [$BOOK], either hg will exit 255 (if an
   exception was raised), or an "updating bookmark ... failed" message is printed to
   stderr. *)
let updating_bookmark_failed = Regex.create_exn "updating bookmark .* failed!"

let with_temp_share ?in_dir repo_root ~f =
  Path.with_temp_dir ?in_dir (File_name.of_string "fe-share")
    ~f:(fun temp_dir ->
      (* Thanks to the cache sharing extension, this is very quick, as quick as using
         quick-share.  The tags cache is not shared though, so let's copy it along with
         all the other caches. *)
      let%bind result =
        hg ~repo_root "share" ~how:Share_io
          ~args:[ "-U"; "."; Abspath.to_string temp_dir ]
      in
      let () = ok_exn result in
      let hg_cache = Path_in_repo.of_string ".hg/cache" in
      let source_cache = Repo_root.append repo_root hg_cache in
      let%bind () =
        match%bind Abspath.file_exists_exn source_cache with
        | false ->
          (* May not exist for caches newly created with [hg share] (rather than
             [hg quick-share]). There isn't much we can do at this point. *)
          Deferred.unit
        | true ->
          let%map result =
            Process.run
              ~prog:"/bin/cp"
              ~args:[ "-a"; "--"
                    ; Abspath.to_string source_cache
                    ; Abspath.to_string
                        (Abspath.append temp_dir (Path_in_repo.to_relpath hg_cache))
                    ]
              ()
          in
          ignore (ok_exn result : string)
      in
      let human_readable =
        let prefix = "temp share of " in
        let human_readable = Repo_root.to_string_hum repo_root in
        if String.is_prefix ~prefix human_readable
        then human_readable
        else prefix ^ human_readable
      in
      f (Repo_root.of_abspath ~human_readable temp_dir))
;;

(* close to the destination because rename fails if the source and destination aren't on
   the same filesystem *)
let dst_parent_dir ~dst_repo_root_abspath__delete_if_exists =
  Option.value_exn ~here:[%here] (Abspath.parent dst_repo_root_abspath__delete_if_exists)
;;

let share repo_root ~dst_repo_root_abspath__delete_if_exists =
  match%map
    Monitor.try_with ~extract_exn:true (fun () ->
      with_temp_share ~in_dir:(dst_parent_dir ~dst_repo_root_abspath__delete_if_exists)
        repo_root ~f:(fun temp_repo_root ->
          Abspath.rename_exn
            ~src:(Repo_root.to_abspath temp_repo_root)
            ~dst__delete_if_exists:dst_repo_root_abspath__delete_if_exists))
  with
  | Ok () -> Ok (Repo_root.of_abspath dst_repo_root_abspath__delete_if_exists)
  | Error exn ->
    error_s
      [%sexp
        "'hg share' failed",
        { src = (repo_root                               : Repo_root.t)
        ; dst = (dst_repo_root_abspath__delete_if_exists : Abspath.t)
        ; exn = (exn                                     : Exn.t)
        }
      ]
;;

let clone remote_repo_path ~dst_repo_root_abspath__delete_if_exists =
  match%map
    Monitor.try_with ~extract_exn:true (fun () ->
      Path.with_temp_dir
        ~in_dir:(dst_parent_dir ~dst_repo_root_abspath__delete_if_exists)
        (File_name.of_string "fe-clone")
        ~f:(fun temp_dir ->
          let%bind result =
            hg_with_optional_repo_root "clone" ~how:Share_io
              ~args: [ "-U" ; "-q"
                     ; Remote_repo_path.to_string remote_repo_path
                     ; Abspath.to_string temp_dir
                     ]
          in
          ok_exn result;
          Abspath.rename_exn ~src:temp_dir
            ~dst__delete_if_exists:dst_repo_root_abspath__delete_if_exists))
  with
  | Ok () -> Ok (Repo_root.of_abspath dst_repo_root_abspath__delete_if_exists)
  | Error exn ->
    error_s
      [%sexp
        "'hg clone' failed",
        { remote_repo_path = (remote_repo_path                        : Remote_repo_path.t)
        ; dst              = (dst_repo_root_abspath__delete_if_exists : Abspath.t)
        ; exn              = (exn                                     : Exn.t)
        }
      ]
;;

let push repo_root bookmarks ~to_ ~overwrite_bookmark =
  if target_repo_is_root_repo repo_root to_ || List.is_empty bookmarks
  then return (Ok ()) (* Don't need to push and pushing would lock up. *)
  else (
    let msg =
      match bookmarks with
      | [ bookmark ] ->
        sprintf "Pushing %s to %s"
          (Bookmark.to_string bookmark)
          (Remote_repo_path.to_string to_)
      | _ ->
        sprintf "Pushing to %s:\n%s"
          (Remote_repo_path.to_string to_)
          (concat (List.map bookmarks ~f:(fun bookmark ->
             concat [ "  "; Bookmark.to_string bookmark; "\n" ])))
    in
    let%bind output =
      Async_interactive.Job.run "%s" msg ~f:(fun () ->
        let flag = if overwrite_bookmark then "-B" else "-r" in
        hg ~repo_root "push" ~how:Capture_output
          ~args:(List.concat_map bookmarks
                   ~f:(fun bookmark -> [ flag; Bookmark.to_string bookmark ])
                 @ [ "-f"
                   ; Remote_repo_path.to_string to_
                   ]))
    in
    let output = ok_exn output in
    let error () =
      return (error "[hg push] failed" output [%sexp_of: Process.Output.t])
    in
    let check_bookmarks_were_pushed () =
      if overwrite_bookmark
      then return (Ok ())
      else (
        (* In practice, we only push one bookmark when not using -B.  For the jane repo,
           this check takes about 1s.  We think that's acceptable since the main use of
           push is in [rebase] and [release], where the performance is relatively less
           important and the error checking is relatively more important. *)
        let%bind bookmark_and_revs =
          Deferred.List.map bookmarks ~f:(fun b ->
            let%map rev = create_rev_exn repo_root (Revset.bookmark b) in
            b, rev)
        in
        let%map results = with_temp_share repo_root ~f:(fun repo_root ->
          (* share doesn't copy the bookmarks over (but checking in case it changes) *)
          let%bind () =
            let%map bookmarks_after_sharing = list_bookmarks repo_root in
            [%test_result: string list] bookmarks_after_sharing ~expect:[]
          in
          let%bind () =
            pull ~even_if_unclean:true repo_root ~from:to_ (`Bookmarks bookmarks)
          in
          Deferred.List.map bookmark_and_revs ~f:(fun (b, local_rev) ->
            let%bind remote_rev = create_rev_exn repo_root (Revset.bookmark b) in
            match%map
              is_ancestor repo_root ~ancestor:local_rev ~descendant:remote_rev
            with
            | true -> Ok ()
            | false ->
              error_s
                [%sexp
                  "failed to push",
                  { bookmark        = (Bookmark.to_string b        : string)
                  ; local_revision  = (Rev.to_string_12 local_rev  : string)
                  ; remote_revision = (Rev.to_string_12 remote_rev : string)
                  }
                ]))
        in
        Or_error.combine_errors_unit results)
    in
    match output.exit_status with
    | Ok () -> check_bookmarks_were_pushed ()
    | Error (`Exit_non_zero 1) ->
      if is_some (String.Search_pattern.index (force push_no_changes_pattern)
                    ~in_:output.stdout)
      && not (Regex.matches updating_bookmark_failed output.stderr)
      then check_bookmarks_were_pushed ()
      else error ()
    | Error _ -> error ())
;;

let delete_bookmarks repo_root bookmarks maybe_push =
  let%bind existing_bookmarks = list_bookmarks repo_root in
  let exists = Hash_set.mem (String.Hash_set.of_list existing_bookmarks) in
  let to_delete = List.filter bookmarks ~f:(fun b -> exists (Bookmark.to_string b)) in
  let%bind () =
    if List.is_empty to_delete
    then return ()
    else (
      let%map result =
        hg ~repo_root "bookmark" ~how:Share_io
          ~args:("--delete" :: List.map to_delete ~f:Bookmark.to_string)
      in
      ok_exn result)
  in
  match maybe_push with
  | `Do_not_push -> return ()
  | `Push_to to_ ->
    let%map push_result = push repo_root bookmarks ~to_ ~overwrite_bookmark:true in
    ok_exn push_result
;;

let phase repo_root revision =
  let rev =
    match revision with
    | `Feature feature_path -> Feature_path.to_string feature_path
    | `Rev rev -> Rev.to_string_40 rev
  in
  let%map result =
    hg ~repo_root "log" ~how:Capture_stdout
      ~args:[ "-r" ; rev
            ; "--template={phase}"
            ]
  in
  match one_line result with
  | `Error e ->
    raise_s [%sexp (sprintf "cannot find phase of %s" rev : string), (e : Error.t)]
  | `Not_one_line lines ->
    raise_s
      [%sexp
        (sprintf "wrong output while finding phase of %s" rev : string)
      , `Lines (lines : string list)
      ]
  | `One_line line ->
    match line with
    | "public" -> `Public
    | "draft"  -> `Draft
    | "secret" -> `Secret
    | _ ->
      raise_s
        [%sexp (sprintf "unexpected output while finding phase of %s" rev : string)
             , (line : string)
        ]
;;

(* The code for the [`Push_to_*] cases used to be:

   {v
     create local bookmark;
     push it to remote;
   v}

   In the [Push_to_and_overwrite] case, we experienced an issue where a concurrent cron
   job pulls between the two steps, causing the local bookmark to be deleted and the
   subsequent push to fail.  We solved this here by using a temporary share:

   {v
     create a temporary share;
     create local bookmark in it;
     push from share to remote;
     delete share;
     create local bookmark;
   v}

   This is less likely to be needed in the case [`Push_to_and_advance] because the
   bookmark is expected to exist on the remote repo, thus the concurrent pull would not
   delete it.  However, it seemed more self-contained to use the same logic for both
   cases, and accept the additional cost of resolving the rev first and creating the
   temporary share. *)
let set_bookmark repo_root bookmark ~to_ maybe_push =
  let set_bookmark_locally repo_root to_ =
    let rev =
      match to_ with
      | `Rev rev -> Rev.to_string_40 rev
      | `Feature feature_path -> Feature_path.to_string feature_path
    in
    let%map result =
      hg ~repo_root "bookmark" ~how:Share_io
        ~args:[ "-f"
              ; "-r"; rev
              ; Bookmark.to_string bookmark
              ]
    in
    ok_exn result
  in
  match maybe_push with
  | `Do_not_push -> set_bookmark_locally repo_root to_
  | `Push_to_and_overwrite remote_repo_path
  | `Push_to_and_advance remote_repo_path as overwrite_or_advance ->
    let%bind rev =
      match to_ with
      | `Rev rev -> return rev
      | `Feature feature_path ->
        create_rev_exn ~human_readable:(Feature_path.to_string feature_path) repo_root
          (Revset.feature_tip feature_path)
    in
    let%bind () =
      if target_repo_is_root_repo repo_root remote_repo_path
      then Deferred.unit
      else
        with_temp_share repo_root ~f:(fun repo_root ->
          let%bind () = set_bookmark_locally repo_root (`Rev rev) in
          let overwrite_bookmark =
            match overwrite_or_advance with
            | `Push_to_and_overwrite _ -> true
            | `Push_to_and_advance   _ -> false
          in
          let%map push_result =
            push repo_root [ bookmark ] ~to_:remote_repo_path ~overwrite_bookmark
          in
          ok_exn push_result
        )
    in
    set_bookmark_locally repo_root (`Rev rev)
;;

module Clean_after_update = struct
  type t =
    | Yes of Cleanliness_witness.t
    | No
end

let clean repo_root (Repo_is_clean : Cleanliness_witness.t) =
  hg ~repo_root "clean" ~how:Share_io ~args:[]
;;

let update
      ?(discard_uncommitted_changes = false)
      ~(clean_after_update : Clean_after_update.t)
      repo_root where_to_update =
  let rev_for_hg, rev_for_hum =
    match where_to_update with
    | `Feature path -> let str = Feature_path.to_string path in str, str
    | `Rev rev -> Rev.to_string_40 rev, Rev.to_string_hum rev
    | `Revset revset -> let str = Revset.to_string revset in str, str
  in
  (* [hg update] can remove the current working directory, so we [chdir] to [repo_root]
     to ensure that the current working directory remains valid. *)
  let%bind () = Unix.chdir (Repo_root.to_string repo_root) in
  let%map update_result =
    Async_interactive.Job.run !"Updating %{Repo_root#hum} to %s" repo_root rev_for_hum
      ~f:(fun () ->
        hg ~repo_root "update" ~how:Share_io
          ~args:([ "-r"; rev_for_hg
                 ; "-q"
                 ]
                 @ if discard_uncommitted_changes then [ "-C" ] else [])
        >>=? fun () ->
        match clean_after_update with
        | No -> return (Ok ())
        | Yes repo_is_clean ->
          clean repo_root repo_is_clean)
  in
  ok_exn update_result
;;

let create_bookmark_and_update_to_it
      ?repo_is_clean repo_root remote_repo_path feature_path rev =
  let%bind repo_is_clean = status_cleanliness_exn ?repo_is_clean repo_root in
  let%bind () = pull ~repo_is_clean repo_root ~from:remote_repo_path (`Rev rev) in
  (* We do

     {[
       set `Do_not_push;
       update;
       push;
     ]}

     rather than:

     {[
       set `Push_to_and_overwrite;
       update;
     ]}

     so that even if the [push] fails, we've already updated.
  *)
  let%bind () =
    set_bookmark repo_root (Feature feature_path) ~to_:(`Rev rev) `Do_not_push
  in
  let%bind () =
    update repo_root (`Feature feature_path) ~clean_after_update:(Yes repo_is_clean)
  in
  let%map push_result =
    push repo_root [ Feature feature_path ] ~to_:remote_repo_path ~overwrite_bookmark:true
  in
  ok_exn push_result
;;

let whats_new repo_root ~from ~to_ ~args =
  let%map result =
    log repo_root Revset.(reverse (for_feature ~base:from ~tip:to_)) ~args ?template:None
  in
  print_string (ok_exn result)
;;

let has_spec_txt repo_root rev =
  match%map
    hg ~repo_root "locate" ~how:Capture_stdout
      ~args:[ "-r"; Rev.to_string_40 rev
            ; "path:.projections/spec.txt"
            ]
  with
  | Ok (_ : string) -> true
  | Error _ -> false
;;

let am_rebasing_across_switch_to_dot_fe repo_root ~old_tip ~new_base =
  match%bind has_spec_txt repo_root old_tip with
  | false -> return false
  | true ->
    let%map has_spec_txt = has_spec_txt repo_root new_base in
    not has_spec_txt
;;

let remove_dot_projections_if_rebasing_across_switch_to_dot_fe
      repo_root ~old_tip ~new_base =
  match%bind am_rebasing_across_switch_to_dot_fe repo_root ~old_tip ~new_base with
  | false -> return ()
  | true ->
    let%bind () =
      Async_interactive.printf "Removing .projections due to rebase across switch-to-dot-fe.\n"
    in
    let%bind rm_result =
      hg ~repo_root "rm" ~how:Capture_stdout ~args:[ ".projections" ]
    in
    let (_ : string) = ok_exn rm_result in
    let%map commit_result =
      commit repo_root
        ~message:"removed .projections before rebasing across switch-to-dot-fe"
    in
    ok_exn commit_result
;;

let rebase ?merge_tool ?repo_is_clean
      ?(abort_on_merge_conflicts = false) ?post_merge_validation_hook
      repo_root feature_path ~feature_id ~old_tip ~old_base ~new_base =
  let%bind repo_is_clean = status_cleanliness_exn ?repo_is_clean repo_root in
  let%bind () =
    update repo_root (`Feature feature_path) ~clean_after_update:(Yes repo_is_clean)
  in
  let%bind () =
    remove_dot_projections_if_rebasing_across_switch_to_dot_fe repo_root
      ~old_tip ~new_base
  in
  let%bind () =
    Async_interactive.printf "Merging with %s.\n" (Rev.to_string_hum new_base)
  in
  let accept_nonzero_exit =
    (* [hg merge] returns 0 on success, 1 if there are unresolved files (see [hg help
       merge | grep 1]).  For Iron's standard merge, we want to [hg resolve -m] the
       conflicts and continue, so we accept status 1.  For custom merge tools, we don't
       have a good story about how to continue if the merge fails, so we abort. *)
    match merge_tool with
    | Some _ -> []
    | None   -> [ 1 ]
  in
  let abort_rebase ~clean_after_update message error =
    let%bind () = Async_interactive.printf "%s -- aborting rebase.\n" message in
    let%bind () =
      update repo_root (`Feature feature_path)
        ~discard_uncommitted_changes:true
        ~clean_after_update
    in
    return error
  in
  match%bind
    hg ~repo_root "merge"
      ~how:Share_io
      ~accept_nonzero_exit
      ~args:([ "-r"; Rev.to_string_40 new_base
             (* Force hg to be interactive so it will prompt (when a file has been deleted
                on one side and changed on the other side).  Otherwise, when stdin is not
                a tty, it picks one.  But because we force it to prompt even when stdin is
                not a tty, it simply reads (and fails if there is no input). *)
             ; "--config"; "ui.interactive=true"
             ]
             @ (match merge_tool with
               | None ->
                 [ "--config"
                 ; sprintf "ui.merge=/usr/bin/merge -A -L %s -L %s -L %s"
                     (sprintf "'old tip: %s [%s]'"
                        (Feature_path.to_string feature_path)
                        (Rev.to_string_12 old_tip))
                     (sprintf "'old base: %s'" (Rev.to_string_hum old_base))
                     (sprintf "'new base: %s [%s]'"
                        (Feature_path.to_string
                           (ok_exn (Feature_path.parent feature_path)))
                        (Rev.to_string_12 new_base))
                 ]
               | Some { Merge_tool. executable; args } ->
                 let config name value =
                   [ "--config"; sprintf "merge-tools.user-merge.%s=%s" name value ]
                 in
                 config "executable" (Abspath.to_string executable)
                 @ config "args" args))
  with
  | Error e ->
    (* In theory, repo_is_clean doesn't necessarily hold anymore after a failed merge: we
       can have .orig files in the tree, if they're not ignored. But it seems fine to
       delete them anyway. *)
    abort_rebase ~clean_after_update:(Yes repo_is_clean)
      "Merge failed" (error "aborted rebase because merge failed" e [%sexp_of: Error.t])
  | Ok () ->
    (if not abort_on_merge_conflicts
     then return (Ok ())
     else (
       match%bind
         Monitor.try_with_or_error ~extract_exn:true
           (fun () -> is_conflict_free_exn repo_root)
       with
       | Ok true  -> return (Ok ())
       | Ok false ->
         abort_rebase ~clean_after_update:(Yes repo_is_clean)
           (sprintf "Merge has conflicts and %s is provided"
              Switch.abort_on_merge_conflicts)
           (Or_error.error_string "aborted rebase because of merge conflicts")
       | Error err ->
         abort_rebase ~clean_after_update:(Yes repo_is_clean)
           (sprintf "Failed while grepping for conflicts and %s is provided"
              Switch.abort_on_merge_conflicts)
           (error "aborted rebase, error while grepping for conflicts"
              err [%sexp_of: Error.t]))
    ) >>=? fun () ->
    (match post_merge_validation_hook with
     | None -> return (Ok ())
     | Some f ->
       match%bind Monitor.try_with_join_or_error ~extract_exn:true f with
       | Ok () -> return (Ok ())
       | Error err ->
         abort_rebase ~clean_after_update:(Yes repo_is_clean)
           "Post merge validation failed"
           (error "aborted rebase because of post merge validation error"
              err [%sexp_of: Error.t])
    ) >>=? fun () ->
    let%bind resolve_result =
      hg ~repo_root "resolve" ~how:Share_io ~args:["-m"; "-q"]
    in
    let () = ok_exn resolve_result in
    let feature_s = Feature_path.to_string feature_path in
    let message =
      sprintf "%s: rebase to %s with ancestor %s"
        feature_s
        (Rev.to_string_hum new_base)
        (Rev.to_string_hum old_base)
    in
    commit repo_root ~message
      ~metadata:(String.Map.of_alist_exn
                   [ "original_bookmark", feature_s
                   ; "iron_feature_id"  , Feature_id.to_string feature_id
                   ])
    >>=? fun () ->
    clean repo_root repo_is_clean
;;

let rename ?repo_is_clean repo_root remote_repo_path (renames : Rename.t list) =
  let%bind bookmark_at_start_of_rename = current_bookmark repo_root in
  let bookmarks_to_pull_and_delete = List.map renames ~f:(fun { from; _ } -> from) in
  let%bind () =
    pull repo_root ~from:remote_repo_path (`Features bookmarks_to_pull_and_delete)
      ?repo_is_clean
  in
  let%bind bookmarks = list_bookmarks repo_root in
  (let conflicting_bookmarks =
     List.filter_map bookmarks ~f:(fun bookmark ->
       try Some (Feature_path.of_string (fst (String.lsplit2_exn bookmark ~on:'@')))
       with _ -> None)
     |> Feature_path.Set.of_list
   in
   let relevant_conflicting_bookmarks =
     List.filter bookmarks_to_pull_and_delete ~f:(Set.mem conflicting_bookmarks)
   in
   if not (List.is_empty relevant_conflicting_bookmarks)
   then
     raise_s
       [%sexp
         "cannot rename with conflicting bookmarks"
       , (relevant_conflicting_bookmarks : Feature_path.t list)
       ]);
  (* This pull/change/push sequence is racy (the bookmarks could move while we're
     doing the renaming), but hg doesn't provide any way of avoiding the race.  We
     can't even go faster than calling [set_bookmark] sequentially n times.
     Eventually, we should complain about unbookmarked heads, which would at least
     notify people if their change was dropped. *)
  let%bind () =
    Deferred.List.iter renames ~f:(fun { feature_id = _; from; to_ } ->
      set_bookmark repo_root (Feature to_) ~to_:(`Feature from) `Do_not_push)
  in
  let%bind () =
    delete_bookmarks repo_root
      (List.map bookmarks_to_pull_and_delete ~f:(fun feature_path ->
         Bookmark.Feature feature_path))
      `Do_not_push
  in
  let bookmarks_to_push =
    List.concat_map renames ~f:(fun { feature_id = _; from; to_ } ->
      [ Bookmark.Feature from ; Feature to_ ])
  in
  (* Because we push -B, we should hopefully manage to push all bookmarks or fail
     entirely, but not fail for half of the bookmarks. *)
  let%bind () =
    push repo_root bookmarks_to_push ~to_:remote_repo_path
      ~overwrite_bookmark:true
    |> Deferred.map ~f:ok_exn
  in
  match Feature_path.of_string (ok_exn bookmark_at_start_of_rename) with
  | exception _ -> Deferred.unit
  | current_feature_path ->
    match
      List.find renames ~f:(fun rename ->
        Feature_path.equal current_feature_path rename.from)
    with
    | None -> Deferred.unit
    | Some rename ->
      (* Just changing the active bookmark, there is nothing to clean. *)
      update repo_root (`Feature rename.to_) ~clean_after_update:No
;;

module Status = struct

  type t =
    | Added    of Path_in_repo.t
    | Removed  of Path_in_repo.t
    | Modified of Path_in_repo.t
    | Copied   of copied
  and copied = { src : Path_in_repo.t; dst : Path_in_repo.t }
  [@@deriving sexp_of]

  let split_line s =
    assert (String.length s >= 2);
    assert (Char.(=) s.[1] ' ');
    s.[0], String.drop_prefix s 2
  ;;

  let parse lines =
    let rec aux acc = function
      | [] -> List.rev acc
      | ('A', dst) :: (' ', src) :: rest ->
        aux (Copied { src = Path_in_repo.of_string src
                    ; dst = Path_in_repo.of_string dst
                    }
             :: acc)
          rest
      | ('A', file) :: rest -> aux (Added    (Path_in_repo.of_string file) :: acc) rest
      | ('R', file) :: rest -> aux (Removed  (Path_in_repo.of_string file) :: acc) rest
      | ('M', file) :: rest -> aux (Modified (Path_in_repo.of_string file) :: acc) rest
      | pair :: _ -> raise_s [%sexp "bad line", (pair : char * string)]
    in
    try aux [] (List.map lines ~f:split_line)
    with exn ->
      raise_s [%sexp "failed to parse output of hg status"
                   , (lines : string list), (exn : Exn.t)]
  ;;

  let src_path_in_repo diffs =
    (* A file can be appear up to N + 1 times, if it is modified and copied N times. *)
    List.dedup_and_sort ~compare:Path_in_repo.compare
      (List.filter_map diffs ~f:(function
         | Added _ -> None
         | Removed file -> Some file
         | Modified file -> Some file
         | Copied { src; dst = _ } -> Some src))
  ;;

  let dst_path_in_repo diffs =
    List.filter_map diffs ~f:(function
      | Added file -> Some file
      | Removed _ -> None
      | Modified file -> Some file
      | Copied { src = _; dst } -> Some dst)
  ;;

  module Changed = struct
    type t =
      | Between of
          { src : Rev.t
          ; dst : [ `Working_copy | `Rev of Rev.t ]
          }
      | Changed_by of Rev.t
  end
end

let status repo_root (changed : Status.Changed.t) =
  let might_have_changes =
    match changed with
    | Changed_by _
    | Between { dst = `Working_copy; _ } -> true
    | Between { src; dst = `Rev dst } -> not (Rev.equal_node_hash src dst)
  in
  if not might_have_changes
  then return []
  else (
    let args =
      match changed with
      | Changed_by rev -> [ "--change"; Rev.to_string_40 rev ]
      | Between { src; dst } ->
        [ "--rev"; Rev.to_string_40 src
        ] @ match dst with
        | `Working_copy -> []
        | `Rev dst -> [ "--rev"; Rev.to_string_40 dst ]
    in
    let%map lines =
      hg ~repo_root "status" ~how:Capture_stdout
        ~args:("-marC" :: args)
    in
    Status.parse (String.split_lines (ok_exn lines)))
;;

let tags repo_root rev =
  let%map result = log repo_root (Revset.of_rev rev) ~template:"{tags}" in
  match one_line result with
  | `One_line line -> Ok (String.split line ~on:' ')
  | `Error e -> Error e
  | `Not_one_line lines ->
    error "[Hg.tags] got unexpected multi-line output" lines [%sexp_of: string list]
;;

let unix_sort ?(args = []) lines =
  let%bind sorter = Process.create ~prog:"/bin/sort" ~args () in
  let sorter = ok_exn sorter in
  let sort_input = Process.stdin sorter in
  Writer.set_buffer_age_limit sort_input `Unlimited;
  List.iter lines ~f:(fun line -> Writer.write_line sort_input line);
  let%bind () = Writer.close sort_input in
  Process.collect_stdout_lines_and_wait sorter |> Deferred.map ~f:ok_exn
;;

let infer_tag repo_root ~root rev =
  if verbose
  then Debug.ams [%here] "infer_tag" (root, rev) [%sexp_of: Feature_name.t * Rev.t];
  match%bind tags repo_root rev with
  | Error _ -> return None
  | Ok tags ->
    match
      List.filter tags ~f:(fun tag -> Tag.is_nice_name tag ~for_tree_rooted_at:root)
    with
    | [] -> return None
    | tags ->
      (* [sort -g] is what hydra already uses to infer tags.  We use the [-r] to get the
         largest tag numerically that identifies the rev. *)
      match%bind unix_sort ~args:[ "-gr" ] (List.map tags ~f:Tag.to_string) with
      | [] -> failwith "unexpected empty output from sort"
      | tag_string :: _ ->
        if verbose then Debug.ams [%here] "got tag" tag_string [%sexp_of: string];
        let tag = Tag.of_string tag_string in
        match%map
          create_rev repo_root (Revset.tag tag) ~human_readable:tag_string
        with
        | Error _ -> None
        | Ok tagged_rev ->
          if Rev.equal_node_hash rev tagged_rev
          then Some tagged_rev
          else None
;;

(* hg cat will pick the first revision of the revset (and the order is undefined), so
   let's not expose it outside this module. The one place we use it, it would be quite
   unlikely that people would put complicated revsets and hopefully people will start
   using Scaffold_requires_global_tag_or_rev_hash, so the problem will go away. *)
let cat_one_revset repo_root revset file =
  let args =
    [ "-r"; Revset.to_string revset
    ; Path_in_repo.to_string file
    ]
  in
  hg ~repo_root "cat" ~how:Capture_stdout ~args
;;

let cat_one repo_root rev file =
  cat_one_revset repo_root (Revset.of_rev rev) file
;;

let cat repo_root rev files ~dir =
  match files with
  | [] -> return Path_in_repo.Map.empty (* hg cat returns 1 when there are no files *)
  | _ :: _ ->
    let dirs_to_build =
      List.dedup_and_sort ~compare:Relpath.compare
        (List.map files ~f:(fun file ->
           let file = Path_in_repo.to_relpath file in
           match Relpath.parent file with
           | None -> Relpath.empty
           | Some dir -> dir))
    in
    let%bind () =
      Deferred.List.iter dirs_to_build ~f:(fun dir_to_build ->
        Unix.mkdir ~p:() (Abspath.to_string (Abspath.append dir dir_to_build)))
    in
    let filename_format =
      String.substr_replace_all (Abspath.to_string dir) ~pattern:"%" ~with_:"%%" ^ "/%p"
    in
    let args =
      [ "-o"; filename_format
      ; "-r"; Rev.to_string_40 rev
      ; "set: 'listfile:/dev/stdin'"
      ]
    in
    let%bind process = hg ~repo_root "cat" ~how:Create_process ~args in
    let process = ok_exn process in
    let to_hg_cat = Process.stdin process in
    Writer.set_buffer_age_limit to_hg_cat `Unlimited;
    List.iter files ~f:(fun file ->
      Writer.write_line to_hg_cat ("path:" ^ Path_in_repo.to_string file));
    let%bind () = Writer.close to_hg_cat in
    let%bind output = Process.collect_output_and_wait process in
    (* We can get warnings about obsolescence markers on stderr, so we shouldn't complain
       about things on stderr. It is not reliable anyway:
       $ DONT_LOAD_ANY_HGRC= command hg cat -r . foobar
       foobar: no such file in rev edb4fcda4011
       [1]
       $ DONT_LOAD_ANY_HGRC= command hg cat -r . "set: 'foobar'"
       [1]
       hg cat seems to fail with code 1 if a file isn't found, and if not, then the caller
       would blow up trying to read the filename we stick in the map, so we are fine. *)
    (match output with
     | { exit_status = Ok (); stdout = ""; stderr = _ } -> ()
     | _ -> raise_s [%sexp "hg cat failed"
                         , (hg_executable : string)
                         , (("cat" :: args) : string list)
                         , (output : Process.Output.t)]);
    return
      (Path_in_repo.Map.of_alist_exn
         (List.map files ~f:(fun file ->
            file, Abspath.append dir (Path_in_repo.to_relpath file))))
;;

module Scaffold = struct
  type shallow_scaffold =
    { repo : Remote_repo_path.t
    ; id   : string [@default "tip"];
    }
  [@@deriving of_sexp]
  ;;

  let shallow_scaffold_of_sexp =
    Sexp.of_sexp_allow_extra_fields shallow_scaffold_of_sexp
  ;;

  let resolve_revision repo ~revision ~scaffold_requires_global_tag_or_rev_hash =
    let extended_PATH =
      concat [ Filename.dirname hg_executable
             ; (match Sys.getenv "PATH" with
                | None -> ""
                | Some rest -> ":" ^ rest)
             ]
    in
    let%map revision =
      match%map
        Process.run ~env:(`Extend [ (dont_load_any_hgrc, "")
                                  ; ("PATH", extended_PATH)
                                  ])
          ~prog:"/j/office/app/hg/prod/bin/resolve-global-tag"
          ~args:[ Remote_repo_path.to_string repo
                ; revision
                ]
          ()
      with
      | Error _   -> revision
      | Ok output -> String.try_chop_suffix output ~suffix:"\n"
    in
    if not scaffold_requires_global_tag_or_rev_hash
    then Ok revision
    else (
      match Rev.of_string_40 revision with
      | _rev -> Ok revision
      | exception exn ->
        Or_error.error "\
              scaffold file must use either a global tag or a 40-char revision hash"
          exn [%sexp_of: Exn.t])
  ;;
end

(* We don't even consider the possibility of a repo scaffolded in a repo scaffolded in a
   repo -- that is, scaffolded setups more than 2-ply deep.  The scaffold machinery does
   not allow this.  But if that ever changed, this code would break: it only indirects
   through $repo_root/scaffold.sexp once. *)
let cat_from_scaffold repo_root file ~scaffold_requires_global_tag_or_rev_hash =
  let rel_scaffold_sexp = Relpath.of_list [ File_name.scaffold_sexp ] in
  let abs_scaffold_sexp =
    Abspath.append (Repo_root.to_abspath repo_root) rel_scaffold_sexp
  in
  match%bind Unix.access (Abspath.to_string abs_scaffold_sexp) [ `Exists ] with
  | Error _ -> return (`No_scaffold rel_scaffold_sexp)
  | Ok () ->
    let res =
      let%bind scaffold_data =
        Reader.load_sexp (Abspath.to_string abs_scaffold_sexp)
          [%of_sexp: Scaffold.shallow_scaffold]
        >>=? fun { repo; id } ->
        Scaffold.resolve_revision repo ~revision:id
          ~scaffold_requires_global_tag_or_rev_hash
        >>|? fun revision ->
        repo, revision
      in
      match scaffold_data with
      | Error _ as err ->
        return (Or_error.tag_arg err
                  (sprintf !"Error loading %{File_name}" File_name.scaffold_sexp)
                  repo_root [%sexp_of: Repo_root.t])
      | Ok (repo, revision) ->
        let host_opt, repo_root_from_scaffold =
          match repo with
          | Ssh { host; path } -> Some host, Repo_root.of_abspath path
          | File path          -> None,      Repo_root.of_abspath path
        in
        match host_opt with
        | None ->
          cat_one_revset repo_root_from_scaffold
            (Revset.of_string revision) file
        | Some host ->
          (* We can't pick the version of hg on the hg box *)
          Process.run ~prog:"ssh" ~args:
            [ host; "--"
            ; "hg"
            ; "--cwd"; Repo_root.to_string repo_root_from_scaffold
            ; "cat"
            ; "-r" ; revision
            ; "--"; Path_in_repo.to_string file
            ] ()
    in
    let%map v = res in
    `Scaffold_exists v
;;

let hg_path_command =
  Command.async'
    ~summary:"print out the file path for the hg executable used by fe"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       print_endline hg_executable;
       return ()
    )
;;

let hg_command =
  Command.async'
    ~summary:"runs hg the same way as fe does internally"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and args =
       map (flag "--" escape ~doc:" arguments to pass to hg")
         ~f:(Option.value ~default:[])
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map env = env () in
       Unix.exec
         ~env
         ~prog:hg_executable
         ~argv:(hg_executable :: args) ()
       |> never_returns
    )
;;
