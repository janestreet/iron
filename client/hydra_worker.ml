open! Core
open! Async
open! Import

let profile = ref false

module Event : sig
  val nest : string -> (unit -> 'a Deferred.t) -> 'a Deferred.t
  val add : string -> unit
  val print : unit -> unit
end = struct
  type t = { name : string
           ; start : Time.t
           ; end_ : Time.t
           ; children : t list }
  let all = ref (Queue.create ())
  let nest name f =
    let current = !all in
    all := Queue.create ();
    let start = Time.now () in
    let%map v = f () in
    let end_ = Time.now () in
    Queue.enqueue current { name; start; end_; children = Queue.to_list !all };
    all := current;
    v
  ;;

  let add name =
    let start = Time.now () in
    let end_ = Time.now () in
    Queue.enqueue !all { name; start; end_; children = [] }
  ;;

  let () = add "toplevel"

  let print ()  =
    if !profile
    then (
      match Queue.to_list !all with
      | [] -> ()
      | { start; _ } :: next ->
        let rec widest_column acc l =
          List.fold_left l ~init:acc
            ~f:(fun acc { name; children; _ } ->
              widest_column (Int.max acc (String.length name) - 2) children + 2)
        in
        let width = widest_column 0 next in
        let rec print prefix first max l =
          let prev = ref first in
          let total = Time.diff max first in
          List.iter l ~f:(fun { name; start; end_; children } ->
            let diff_from src =
              let span = Time.diff end_ src in
              sprintf "\t%.2fs\t%.1f%%"
                (Time.Span.to_sec span) (Time.Span.(to_sec span /. to_sec total) *. 100.)
            in
            printf "%-*s : %s%s\n"
              width (prefix ^ name) (diff_from !prev) (diff_from first);
            print ("  " ^ prefix) start end_ children;
            prev := end_)
        in
        print "" start (Time.now ()) next)
  ;;

end
let event = Event.add

let dont_fail_if_server_is_down f =
  match%bind Deferred.Or_error.try_with f with
  | Ok () -> Deferred.unit
  | Error e ->
    match%map
      Deferred.Or_error.try_with (fun () -> Ping.rpc_to_server_exn ())
    with
    | Ok () ->
      (* If we can connect to the server, the previous error was genuine, so we fail. *)
      Error.raise e
    | Error _ ->
      (* If we can't connect, we assume the server is down, and fail quietly. The server
         will rerequest anything it's missing on startup, and this way hydra can keep
         sending mails for really unexpected failures. *)
      eprintf "%s\n" (Error.to_string_hum e)
;;

module Fake_attribute = struct
  let default_owner = Unresolved_name.of_string "file-owner"
  let default_scrutiny_level = Scrutiny_level.of_int 10

  type t =
    { owner                              : Unresolved_name.t       [@default default_owner]
    ; file_reviewers                     : Unresolved_name.t list  [@default []]
    ; followers                          : Unresolved_name.t list  [@default []]
    ; scrutiny_level                     : Scrutiny_level.Syntax.t [@default default_scrutiny_level]
    ; is_read_by_whole_feature_reviewers : bool                    [@default true]
    }
  [@@deriving sexp]

  let default = t_of_sexp (Sexp.of_string "()")

  let to_attribute ~aliases
        { owner
        ; file_reviewers
        ; followers
        ; scrutiny_level
        ; is_read_by_whole_feature_reviewers
        } =
    let scrutiny_name =
      Scrutiny_name.of_string
        (sprintf !"level%{sexp:Scrutiny_level.Syntax.t}" scrutiny_level)
    in
    let min_file_reviewers = 1 in
    let max_file_reviewers = 2 in
    let build_projections = Build_projection_name.Set.empty in
    let more_than_max_reviewers  =
      Int.O.(List.length file_reviewers > max_file_reviewers)
    in
    let fewer_than_min_reviewers =
      Int.O.(List.length file_reviewers < min_file_reviewers)
    in
    let owner = User_name_by_alternate_name.to_user_name aliases owner in
    let review_obligation =
      file_reviewers
      |> List.map ~f:(User_name_by_alternate_name.to_user_name aliases)
      |> User_name.Set.of_list
      |> Review_obligation.all_of
    in
    let followers =
      followers
      |> List.map ~f:(User_name_by_alternate_name.to_user_name aliases)
      |> User_name.Set.of_list
    in
    Review_attributes.create
      ~build_projections
      ~tags:Tag.Set.empty
      ~fewer_than_min_reviewers
      ~followers
      ~is_read_by_whole_feature_reviewers
      ~more_than_max_reviewers
      ~owner
      ~review_obligation
      ~scrutiny_level
      ~scrutiny_name
  ;;
end

module Fake_attribute_by_rev = struct
  type t = Fake_attribute.t Node_hash.Table.t [@@deriving sexp]
end

let use_or_compute_and_store worker_cache_session key rev ~fake_attribute compute =
  match fake_attribute with
  | `Fake_default | `Fake_by_rev _ -> compute ~fake_attribute rev
  | `Do_not_fake ->
    Worker_cache.Worker_session.use_or_compute_and_store worker_cache_session
      key rev (compute ~fake_attribute:`Do_not_fake)
;;

(* Keep this function lambda lifted, so it's easier to see at the call site what it uses
   from its environment that is not part of the key of the cache. *)
let compute_obligations_uncached repo_root ~repo_is_clean ~fake_attribute ~aliases rev =
  let fake_for_testing =
    match fake_attribute with
    | `Do_not_fake -> None
    | `Fake_default -> Some Fake_attribute.default
    | `Fake_by_rev fake_by_rev -> Some (Hashtbl.find_exn fake_by_rev (Rev.node_hash rev))
  in
  let fake_for_testing =
    Option.map fake_for_testing ~f:(Fake_attribute.to_attribute ~aliases)
  in
  let%bind () = Hg.update repo_root (`Rev rev) ~clean_after_update:No in
  event ("hg update " ^ String.prefix (Rev.to_string_40 rev) 4);
  let%map (obligations_are_valid, obligations, obligations_version) =
    Rev_facts.Obligations_are_valid.create_exn
      ?fake_for_testing repo_root ~repo_is_clean rev ~aliases
  in
  { Worker_obligations.
    obligations_are_valid
  ; obligations
  ; obligations_version
  }
;;

let compute_obligations repo_root ~repo_is_clean rev
      ~fake_attribute ~aliases ~worker_cache_session =
  let compute = compute_obligations_uncached repo_root ~repo_is_clean ~aliases in
  use_or_compute_and_store worker_cache_session Worker_obligations rev ~fake_attribute
    compute
;;

let can_compute_incrementally fake_attribute =
  match fake_attribute with
  | `Fake_default | `Fake_by_rev _ -> false
  | `Do_not_fake -> true
;;

let compute_worker_rev_facts_uncached repo_root ~repo_is_clean ~fake_attribute
      ~aliases ?try_incremental_computation_based_on ~worker_cache_session rev
  =
  Event.nest "compute_worker_rev_facts" (fun () ->
    let cached_facts_for_incremental_computation =
      let open Option.Let_syntax in
      let%bind base_rev =
        if can_compute_incrementally fake_attribute
        then try_incremental_computation_based_on
        else None
      in
      let%bind { Worker_rev_facts. rev_facts = _; crs; cr_soons } =
        Worker_cache.Worker_session.find worker_cache_session base_rev Worker_rev_facts
      in
      match crs, cr_soons with
      | Error _,  _ | _, Error _ -> None
      | Ok due_now, Ok due_soon ->
        let%bind { Worker_obligations. obligations_version; _ } =
          Worker_cache.Worker_session.find worker_cache_session base_rev
            Worker_obligations
        in
        let%map base_cr_format =
          match obligations_version with
          | Error _ -> None
          | Ok obligations_version ->
            Some (Obligations_version.cr_comment_format obligations_version)
        in
        { Cr_comment.Cached_facts_for_incremental_computation.
          base_rev
        ; base_crs = { due_now; due_soon }
        ; base_cr_format
        }
    in
    let%bind () = Hg.update repo_root (`Rev rev) ~clean_after_update:No in
    event ("hg update " ^ String.prefix (Rev.to_string_40 rev) 4);
    let%bind conflict_free =
      Rev_facts.Is_conflict_free.create repo_root ~repo_is_clean rev
    in
    event "after conflict_free";
    let%bind { Worker_obligations.
               obligations_are_valid
             ; obligations
             ; obligations_version
             } =
      compute_obligations repo_root ~repo_is_clean rev
        ~fake_attribute ~aliases ~worker_cache_session
    in
    event "after obligations";
    let file_owner =
      match obligations with
      | Error _ -> const (error_string "broken obligations")
      | Ok obligations -> Obligations.file_owner obligations
    in
    let%map (is_cr_clean, crs_or_error) =
      Rev_facts.Is_cr_clean.create repo_root ~repo_is_clean
        (Or_error.map obligations_version ~f:Obligations_version.cr_comment_format)
        ~incremental_based_on:cached_facts_for_incremental_computation
        rev ~file_owner
    in
    event "after crs";
    let crs, cr_soons =
      match crs_or_error with
      | Error _ as e -> e, e
      | Ok { due_now; due_soon } -> Ok due_now, Ok due_soon
    in
    let rev_facts =
      ok_exn (Rev_facts.create conflict_free is_cr_clean obligations_are_valid)
    in
    { Worker_rev_facts. rev_facts; crs ; cr_soons })
;;

let compute_worker_rev_facts repo_root ~repo_is_clean ~rev ~fake_attribute
      ~aliases ?try_incremental_computation_based_on ~worker_cache_session ()
  =
  let compute =
    compute_worker_rev_facts_uncached repo_root ~repo_is_clean
      ~aliases ?try_incremental_computation_based_on ~worker_cache_session
  in
  let%bind { Worker_rev_facts. rev_facts; crs; cr_soons } =
    use_or_compute_and_store worker_cache_session
      Worker_rev_facts rev ~fake_attribute compute
  in
  let result =
    (* Always use the latest available inferred human name *)
    { Worker_rev_facts. rev_facts = Rev_facts.with_rev_exn rev_facts rev; crs; cr_soons }
  in
  (* In case we used a cached [Worker_rev_facts.t] value, we may not have computed the
     obligations.  We'll need it later anyway and now is a better time than another
     because we are already at the right revision. *)
  let%map { Worker_obligations. obligations; _ } =
    compute_obligations repo_root ~repo_is_clean rev
      ~fake_attribute ~aliases ~worker_cache_session
  in
  result, obligations
;;

let infer_base_of_repository remote_repo_path repo_root =
  let tag_root =
    match Remote_repo_path.family remote_repo_path with
    | None -> "jane"
    | Some family -> family
  in
  let%bind all_tags =
    Hg.log repo_root Revset.all_tags ~template:"{tags % \"{tag}\\n\"}"
  in
  let%bind tag =
    Process.run ~prog:"bash"
      ~args:[ "-c"
            ; sprintf !"echo %{sh} | grep ^%{sh}- | sort -g | tail -n 1"
                (ok_exn all_tags)
                tag_root
            ]
      ()
  in
  let tag = ok_exn tag in
  let%map rev = Hg.create_rev repo_root (Revset.of_string (String.strip tag)) in
  ok_exn rev
;;

let infer_base_of_bookmark remote_repo_path repo_root rev =
  let%bind repo_base = infer_base_of_repository remote_repo_path repo_root in
  Hg.first_greatest_common_ancestor repo_root repo_base rev
;;

let infer_base_command =
  Command.async'
    ~summary:"guess the base of a revision (assuming it is the tip of a feature)"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and rev = rev
     and remote_repo_path = anon (maybe ("remote_repo_path" %: string))
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind rev = Raw_rev.resolve_exn rev ~in_:Repo_root.program_started_in in
       let remote_repo_path =
         Option.value_map remote_repo_path ~f:Remote_repo_path.of_string
           ~default:Remote_repo_path.jane_submissions
       in
       let%map base =
         infer_base_of_bookmark remote_repo_path (ok_exn Repo_root.program_started_in) rev
       in
       printf "base = %s\n" (Rev.to_string_40 base)
    )
;;

let all_obligations
      ~base_is_ancestor_of_tip
      ~obligations_at_tip ~tip
      ~obligations_at_base ~base
      ~fake_attribute
      ~aliases
      ~worker_cache_session
      repo_root
      ~repo_is_clean
      need_diff4s_starting_from =
  let base_is_ancestor_of_tip =
    Rev_facts.Is_ancestor.check base_is_ancestor_of_tip ~ancestor:base ~descendant:tip
    |> ok_exn
  in
  match obligations_at_tip, obligations_at_base, base_is_ancestor_of_tip with
  | (Error _ as e), _, _
  | _, (Error _ as e), _ -> return e
  | _, _, false ->
    return (error_string
              "avoiding a costly computation, since tip doesn't descend from base")
  | Ok obligations_at_tip, Ok obligations_at_base, true ->
    let obligations_by_rev = Rev.Compare_by_hash.Map.singleton tip obligations_at_tip in
    let obligations_by_rev = Map.add obligations_by_rev ~key:base ~data:obligations_at_base in
    let all_revs =
      List.concat_map need_diff4s_starting_from
        ~f:(fun { Review_edge.base; tip } -> [ base; tip ])
    in
    let rec loop obligations_by_rev = function
      | [] -> return (Ok obligations_by_rev)
      | rev :: revs ->
        if Map.mem obligations_by_rev rev
        then loop obligations_by_rev revs
        else (
          let%bind { Worker_obligations. obligations; _ } =
            compute_obligations repo_root ~repo_is_clean rev
              ~fake_attribute ~aliases ~worker_cache_session
          in
          event "after obligations";
          match obligations with
          | Error _ as e -> return e
          | Ok obligations ->
            let obligations_by_rev =
              Map.add obligations_by_rev ~key:rev ~data:obligations
            in
            loop obligations_by_rev revs)
    in
    loop obligations_by_rev all_revs
;;

let testing_incremental_rev_facts =
  Option.is_some (Sys.getenv "IRON_FUNCTIONAL_TESTING_INCREMENTAL_REV_FACTS")
;;

let run_without_server
      ~repo_root
      ~feature_path
      ~base
      ~aliases
      ~tip
      ~fake_attribute
      ~need_diff4s_starting_from
      ~lines_required_to_separate_ddiff_hunks
      ~worker_cache_session
  =
  let review_edge_from_base_to_base = { Review_edge. base; tip = base } in
  let need_diff4s_starting_from =
    List.dedup_and_sort ~compare:Review_edge.compare
      (review_edge_from_base_to_base :: need_diff4s_starting_from)
  in
  event "start";
  let base_is_ancestor_of_tip_def =
    Rev_facts.Is_ancestor.create repo_root ~ancestor:base ~descendant:tip
  in
  let%bind repo_is_clean = Hg.status_cleanliness repo_root in
  let repo_is_clean = ok_exn repo_is_clean in
  let%bind () =
    if not testing_incremental_rev_facts
    then return ()
    else (
      assert (can_compute_incrementally fake_attribute);
      printf "Check equality of tip facts computed from scratch and incrementally...\n";
      let%bind (_ : Worker_rev_facts.t * Obligations.t Or_error.t) =
        compute_worker_rev_facts repo_root ~repo_is_clean ~rev:base
          ~fake_attribute ~aliases ~worker_cache_session ()
      in
      Worker_cache.Worker_session.remove worker_cache_session tip Worker_rev_facts;
      let%bind (tip_facts_from_scratch, (_ : Obligations.t Or_error.t)) =
        compute_worker_rev_facts repo_root ~repo_is_clean ~rev:tip ~fake_attribute ~aliases
          ?try_incremental_computation_based_on:None
          ~worker_cache_session ()
      in
      Worker_cache.Worker_session.remove worker_cache_session tip Worker_rev_facts;
      let%bind (tip_facts_incremental, (_ : Obligations.t Or_error.t)) =
        compute_worker_rev_facts repo_root ~repo_is_clean ~rev:tip ~fake_attribute ~aliases
          ~try_incremental_computation_based_on:base
          ~worker_cache_session ()
      in
      let tip_facts_from_scratch =
        Worker_rev_facts.for_sorted_output tip_facts_from_scratch
      in
      let tip_facts_incremental =
        Worker_rev_facts.for_sorted_output tip_facts_incremental
      in
      [%test_result: Worker_rev_facts.t]
        ~message:"inconsistent tip worker_rev_facts results"
        ~expect:tip_facts_from_scratch tip_facts_incremental;
      return ())
  in
  let%bind ( { Worker_rev_facts.
               rev_facts   = tip_facts
             ; crs         = crs_at_tip
             ; cr_soons    = cr_soons_at_tip
             }, obligations_at_tip ) =
    compute_worker_rev_facts repo_root ~repo_is_clean ~rev:tip ~fake_attribute ~aliases
      ~try_incremental_computation_based_on:base
      ~worker_cache_session ()
  in
  (* BEWARE: At this point, the working copy may or may not have the initial revision *)
  let base_facts_def =
    (* This still shortcuts the computation wheen [~fake_attribute] is [`Fake]. *)
    if Rev.Compare_by_hash.(=) base tip then
      return ( { Worker_rev_facts.
                 rev_facts   = Rev_facts.with_rev_exn tip_facts base
               ; crs         = crs_at_tip
               ; cr_soons    = cr_soons_at_tip
               }, obligations_at_tip )
    else
      compute_worker_rev_facts repo_root ~repo_is_clean ~rev:base
        ~fake_attribute ~aliases ~worker_cache_session ()
  in
  let%bind (({ Worker_rev_facts.
               rev_facts   = base_facts
             ; crs         = _
             ; cr_soons    = cr_soons_at_base
             }, obligations_at_base), base_is_ancestor_of_tip) =
    Deferred.both base_facts_def base_is_ancestor_of_tip_def
  in
  event "after base facts";
  let%bind obligations_by_rev =
    Event.nest "all obligations" (fun () ->
      all_obligations
        repo_root ~repo_is_clean
        need_diff4s_starting_from
        ~base_is_ancestor_of_tip
        ~tip ~obligations_at_tip
        ~base ~obligations_at_base
        ~fake_attribute ~aliases
        ~worker_cache_session)
  in
  let%bind diffs_by_review_edge =
    match obligations_by_rev with
    | Error _ as e -> return e
    | Ok obligations_by_rev ->
      let b2 = base and f2 = tip in
      let diamonds =
        (List.map need_diff4s_starting_from ~f:(fun { base = b1; tip = f1 } ->
           { Diamond. b1; b2; f1; f2 }))
      in
      Event.nest "all file_diffs" (fun () ->
        Diff4s_for_diamond.Cache.with_ ~time:event repo_root obligations_by_rev diamonds
          ~f:(fun cache ->
            event "file_diffs cache";
            Deferred.List.map diamonds ~f:(fun diamond ->
              Event.nest "file_diffs" (fun () ->
                let%map diffs =
                  Diff4s_for_diamond.create cache diamond
                    ~lines_required_to_separate_ddiff_hunks
                in
                let { Diamond.b1 ; f1 ; _ } = diamond in
                { Review_edge.base = b1; tip = f1 }, diffs
              ))))
      >>| Or_error.return
  in
  let cr_soons =
    match cr_soons_at_base, cr_soons_at_tip with
    | Error _, _ | _, Error _ ->
      error_s
        [%sexp
          "cannot find CR-soons",
          { base = (cr_soons_at_base : _ Or_error.t)
          ; tip  = (cr_soons_at_tip  : _ Or_error.t)
          }
        ]
    | Ok base_cr_soons, Ok tip_cr_soons ->
      Cr_soons.In_feature.create
        ~feature_path
        ~base_facts
        ~base_cr_soons
        ~tip_facts
        ~tip_cr_soons
        ~base_is_ancestor_of_tip
  in
  let diff4s =
    Or_error.map diffs_by_review_edge ~f:(fun diffs_by_review_edge ->
      (List.concat_map diffs_by_review_edge ~f:snd))
  in
  let diff_from_base_to_tip =
    match diffs_by_review_edge with
    | Error _ as e -> e
    | Ok l ->
      let diffs =
        List.Assoc.find_exn l review_edge_from_base_to_base
          ~equal:(fun x y -> [%compare: Review_edge.t] x y = 0)
      in
      Ok (List.map diffs ~f:(fun diff4 ->
        match Diff4.as_from_scratch_to_diff2 diff4 with
        | Some diff2 -> diff2
        | None -> raise_s [%sexp "diff4 should have been a diff2", (diff4 : Diff4.t)]))
  in
  let base_allow_review_for =
    Result.map obligations_at_base
      ~f:(fun { Iron_obligations.Obligations. obligations_repo; _ } ->
        match obligations_repo with
        | `Fake ->
          (* This case happens when we fake valid obligations in functional tests, and in
             that case we want to allow all [fe review -for]. *)
          Allow_review_for.all
        | `Actual { Iron_obligations.Obligations_repo. allow_review_for; _ } ->
          allow_review_for)
  in
  (* We want to make sure that invalid [base_allow_review_for] are reported by Iron, so
     we check that invalid [base_allow_review_for] implies invalid [base_facts]. *)
  if is_error base_allow_review_for
  then assert (not (ok_exn
                      (Rev_facts.Obligations_are_valid.check
                         base_facts.obligations_are_valid
                         (Rev_facts.rev base_facts))));
  Event.print ();
  return { Update_bookmark.Info.
           crs_at_tip
         ; base_is_ancestor_of_tip
         ; base_facts
         ; tip_facts
         ; base_allow_review_for
         ; diff_from_base_to_tip
         ; diff4s
         ; cr_soons
         }
;;

let infer_tag repo_root feature_path rev =
  let rev = Rev.without_human_readable rev in
  let%map tag = Hg.infer_tag repo_root rev ~root:(Feature_path.root feature_path) in
  Option.value tag ~default:rev
;;

let run repo_root fake_attribute ~run_between_rpcs =
  event "before connecting";
  dont_fail_if_server_is_down (fun () ->
    let%bind tip = Hg.create_rev repo_root Revset.dot in
    let tip = ok_exn tip in
    match Sys.getenv "BOOKMARK" with
    | None ->
      (* Can happen when using 'hydra compile', but this case simply shouldn't happen
         (because there is no reason to use it). *)
      raise_s [%sexp "BOOKMARK isn't set, fe worker can't continue", Rev (tip : Rev.t)]
    | Some bookmark ->
      event "before feature";
      let stop e =
        printf "stopping:\n%s\n" (Error.to_string_hum e);
        Deferred.unit
      in
      let%bind rev_zero = Hg.create_rev_zero repo_root in
      match Feature_path.of_string_or_error bookmark with
      | Error e -> stop e
      | Ok feature_path ->
        let%bind tip = infer_tag repo_root feature_path tip in
        match%bind
          Hydra_worker.rpc_to_server { feature_path; rev_zero; tip = Some tip }
        with
        | Error e -> stop e
        | Ok { base
             ; feature_id
             ; need_diff4s_starting_from
             ; aliases
             ; lines_required_to_separate_ddiff_hunks
             ; worker_cache
             } ->
          let%bind () = run_between_rpcs () in
          let%bind base = infer_tag repo_root feature_path base in
          let worker_cache_session = Worker_cache.Worker_session.create worker_cache in
          let%bind info =
            Deferred.Or_error.try_with (fun () ->
              let need_diff4s_starting_from = List.map need_diff4s_starting_from ~f:fst in
              run_without_server
                ~repo_root
                ~feature_path
                ~base
                ~aliases
                ~tip
                ~fake_attribute
                ~need_diff4s_starting_from
                ~lines_required_to_separate_ddiff_hunks
                ~worker_cache_session)
          in
          let info =
            if testing_incremental_rev_facts
            then Ok (ok_exn info)
            else info
          in
          Update_bookmark.rpc_to_server_exn
            { feature_path
            ; feature_id
            ; info
            ; augment_worker_cache
              = Worker_cache.Worker_session.back_to_server worker_cache_session
            })
;;

let profile_flag =
  let open Command.Param in
  no_arg_flag "-profile" ~doc:"print information about where the time is spent"
  |> map ~f:(fun p -> profile := p)
;;

let command =
  Command.async'
    ~summary:"the part of Iron that works each time something changes in a repository"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and fake_attribute =
       no_arg_flag "-fake-valid-obligations"
         ~doc:"pretend to call the obligations lib, for testing"
     and attributes_by_rev =
       flag "-fake-attribute-by-rev" (optional string)
         ~doc:"map eg. ((rev1 attribute) (rev2 attributes)), \
               for use with -fake-valid-obligations"
     and run_between_rpcs =
       flag "-run-between-rpcs" (optional string)
         ~doc:"script a string to be interpreted by bash to run between the two rpcs, \
               to test race conditions"
     and () = profile_flag
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let fake_attribute =
         match fake_attribute, attributes_by_rev with
         | false, Some _ ->
           failwith "can't use -fake-attributes-by-rev without -fake-valid-obligations"
         | false, None -> `Do_not_fake
         | true, None -> `Fake_default
         | true, Some fake_attribute ->
           `Fake_by_rev (Fake_attribute_by_rev.t_of_sexp
                           (Sexp.of_string (String.concat ["("; fake_attribute; ")"])))
       in
       let repo_root = ok_exn Repo_root.program_started_in in
       let run_between_rpcs =
         match run_between_rpcs with
         | None -> return
         | Some script ->
           assert am_functional_testing;
           fun () ->
             let%map code = Sys.command (sprintf !"bash -e -c %{sh}" script) in
             [%test_result: int] code ~expect:0
       in
       run repo_root fake_attribute ~run_between_rpcs
    )
;;

let serverless_command =
  Command.async'
    ~summary:""
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and () = profile_flag
     and base_opt =
       flag "base" (optional rev_arg_type) ~doc:"REV the assumed base of the feature"
     and unparsed_review_managers =
       flag "review-manager" (listed string) ~doc:"base,tip"
     and lines_required_to_separate_ddiff_hunks =
       lines_required_to_separate_ddiff_hunks_with_default
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let repo_root = ok_exn Repo_root.program_started_in in
       let%bind bookmark = Hg.current_bookmark repo_root in
       let bookmark = ok_exn bookmark in
       let%bind tip = Hg.create_rev repo_root Revset.dot in
       let tip = ok_exn tip in
       let%bind base =
         match base_opt with
         | None -> infer_base_of_bookmark Remote_repo_path.jane_submissions repo_root tip
         | Some base -> Raw_rev.resolve_exn base ~in_:(Ok repo_root)
       in
       printf !"Assuming base = %s, tip = %s, bookmark = %s\n"
         (Rev.to_string_40 base)
         (Rev.to_string_40 tip)
         bookmark;
       let%bind review_managers =
         Deferred.List.map unparsed_review_managers ~f:(fun unparsed ->
           let base, tip = String.lsplit2_exn unparsed ~on:',' in
           let%map (base, tip) =
             Deferred.both
               (Hg.create_rev repo_root (Revset.of_string base))
               (Hg.create_rev repo_root (Revset.of_string tip))
           in
           { Review_edge.
             base = ok_exn base
           ; tip  = ok_exn tip
           }
         )
       in
       let feature_path = Feature_path.of_string bookmark in
       let worker_cache_session =
         Worker_cache.Worker_session.create Worker_cache.From_server_to_worker.empty
       in
       let%bind result =
         run_without_server
           ~repo_root
           ~feature_path
           ~base
           ~aliases:User_name_by_alternate_name.not_available
           ~tip
           ~fake_attribute:`Do_not_fake
           ~need_diff4s_starting_from:review_managers
           ~lines_required_to_separate_ddiff_hunks
           ~worker_cache_session
       in
       let%bind (_ : unit Or_error.t) =
         Deferred.Or_error.try_with
           (fun () -> Hg.update repo_root (`Feature feature_path) ~clean_after_update:No)
       in
       print_endline (result
                      |> [%sexp_of: Update_bookmark.Info.t]
                      |> Sexp.to_string_hum);
       Deferred.unit
    )
;;
