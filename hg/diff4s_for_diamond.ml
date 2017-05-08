open Core
open Async
open Import

module Cache = struct
  module Rev_pair = struct
    module T = struct
      type t = Rev.Compare_by_hash.t * Rev.Compare_by_hash.t
      [@@deriving compare, sexp_of]
      let hash (rev1, rev2) =
        Rev.Compare_by_hash.hash rev1 lxor Rev.Compare_by_hash.hash rev2
    end
    include T
    include Hashable.Make_plain   (T)
    include Comparable.Make_plain (T)
  end

  module Rev_and_path_in_repo = struct
    module T = struct
      type t = Rev.Compare_by_hash.t * Path_in_repo.t
      [@@deriving compare, sexp_of]
      let hash (rev, path_in_repo) =
        Rev.Compare_by_hash.hash rev lxor Path_in_repo.hash path_in_repo
    end
    include T
    include Hashable.Make_plain   (T)
    include Comparable.Make_plain (T)
  end

  type t =
    { repo_root             : Repo_root.t
    ; all_revs              : Rev.Compare_by_hash.Set.t
    ; rev_diamonds          : Rev.t Diamond.t list
    ; status_cache          : Hg.Status.t list Rev_pair.Table.t
    ; manifest_cache        : Path_in_repo.Set.t Rev.Compare_by_hash.Table.t
    ; cat_cache             : Abspath.t Path_in_repo.Map.t Rev.Compare_by_hash.Table.t
    ; attributed_file_cache : Attributed_file.t Deferred.t Rev_and_path_in_repo.Table.t
    ; diff4_cache           : Diff4.Cache.t
    ; obligations_by_rev    : Obligations.t Rev.Compare_by_hash.Map.t
    } [@@deriving sexp_of]

  let status t rev1 rev2 =
    Hashtbl.find_exn t.status_cache (rev1, rev2)
  ;;

  let manifest t rev =
    Hashtbl.find_exn t.manifest_cache rev
  ;;

  let cat t rev =
    Hashtbl.find_exn t.cat_cache rev
  ;;

  (* When a file changes (including appears or disappears) in the tree either in the old
     feature or in the new feature, we claim it is a file of interest (see
     files_modified_in_some_way_from_status below).  Here we do the same thing for the
     files whose scrutiny changed.  If the scrutiny changed either in the old feature or
     in the new feature for a file, we claim it is a file of interest.  This function
     causes us to not miss diffs when the scrutiny of a file is bumped but this file is
     kept unchanged, since such files have no reason to show up in hg status.  Because
     file changes already includes files that are deleted or added in either feature, we
     only have to consider files that exist at both the base and tip of a feature, not
     files that exist only at the tip or the base. *)
  let all_existing_files_whose_attribute_changed t (revs : _ Diamond.t) =
    let { Diamond. b1; b2; f1; f2 } =
      Diamond.map revs ~f:(fun rev -> Map.find_exn t.obligations_by_rev rev)
    in
    List.fold ~init:Path_in_repo.Set.empty [(b1, f1); (b2, f2)] ~f:(fun set (ob1, ob2) ->
      Hashtbl.fold ob1.by_path ~init:set ~f:(fun ~key:path ~data:attr1 set ->
        match Hashtbl.find ob2.by_path path with
        | None -> set
        | Some attr2 ->
          if Review_attributes.equal attr1 attr2
          then set
          else Set.add set path))
  ;;

  (* The list of files we want to see in the diffs.  If a file appears in one the two
     feature diffs, we want to know about that file in all the revisions of the
     diamond. *)
  let files_modified_in_some_way_from_status t (revs : Rev.t Diamond.t) =
    Path_in_repo.Set.of_list
      (Hg.Status.src_path_in_repo   (status t revs.b1 revs.f1)
       @ Hg.Status.dst_path_in_repo (status t revs.b1 revs.f1)
       @ Hg.Status.src_path_in_repo (status t revs.b2 revs.f2)
       @ Hg.Status.dst_path_in_repo (status t revs.b2 revs.f2))
  ;;

  let paths_of_interest t revs =
    Set.union
      (files_modified_in_some_way_from_status t revs)
      (all_existing_files_whose_attribute_changed t revs)
  ;;

  let fill_in_status_cache t =
    let all_pairs = List.concat_map t.rev_diamonds ~f:Diamond.edges in
    let all_pairs = List.dedup_and_sort ~compare:Rev_pair.compare all_pairs in
    Deferred.List.iter ~how:(`Max_concurrent_jobs 4) all_pairs
      ~f:(fun ((src, dst) as key) ->
        let%map status = Hg.status t.repo_root (Between { src; dst = `Rev dst }) in
        Hashtbl.add_exn t.status_cache ~key ~data:status)
  ;;

  let fill_in_manifest_cache t =
    Deferred.List.iter ~how:(`Max_concurrent_jobs 4) (Set.to_list t.all_revs)
      ~f:(fun rev ->
        let%map paths = Hg.manifest t.repo_root (`Revset (Hg.Revset.of_rev rev)) in
        Hashtbl.add_exn t.manifest_cache ~key:rev
          ~data:(Path_in_repo.Set.of_list paths))
  ;;

  let fill_in_cat_cache t dir =
    let superset_by_rev =
      Map.map ~f:Path_in_repo.Set.union_list
        (Rev.Compare_by_hash.Map.of_alist_multi
           (List.concat_map t.rev_diamonds ~f:(fun revs ->
              let data = paths_of_interest t revs in
              List.map (Diamond.to_list revs) ~f:(fun f -> f, data))))
    in
    let paths_to_cat_by_rev =
      Map.mapi superset_by_rev ~f:(fun ~key:rev ~data:superset ->
        Set.to_list (Set.inter (manifest t rev) superset))
    in
    Deferred.List.iter ~how:(`Max_concurrent_jobs 4) (Map.to_alist paths_to_cat_by_rev)
      ~f:(fun (rev, paths_to_cat) ->
        let%map map =
          Hg.cat t.repo_root rev paths_to_cat
            ~dir:(Abspath.extend dir (Node_hash.to_file_name (Rev.node_hash rev)))
        in
        Hashtbl.add_exn t.cat_cache ~key:rev ~data:map)
  ;;

  let attributed_file t rev path_in_repo (file : Abspath.t) =
    let key = (rev, path_in_repo) in
    match Hashtbl.find t.attributed_file_cache key with
    | Some af -> af
    | None ->
      let def =
        Attributed_file.create ~path_in_repo ~rev ~file
          (Hashtbl.find_exn (Map.find_exn t.obligations_by_rev rev).by_path path_in_repo)
      in
      Hashtbl.add_exn t.attributed_file_cache ~key ~data:def;
      def
  ;;

  let with_ ?(time = ignore) repo_root obligations_by_rev rev_diamonds ~f =
    let t =
      { repo_root
      ; all_revs              = Rev.Compare_by_hash.Set.of_list
                                  (List.concat_map rev_diamonds ~f:Diamond.to_list)
      ; rev_diamonds
      ; status_cache          = Rev_pair.Table.create ()
      ; manifest_cache        = Rev.Compare_by_hash.Table.create ()
      ; cat_cache             = Rev.Compare_by_hash.Table.create ()
      ; attributed_file_cache = Rev_and_path_in_repo.Table.create ()
      ; diff4_cache           = Diff4.Cache.create ()
      ; obligations_by_rev
      }
    in
    let%bind () = fill_in_status_cache t in
    time "statuses";
    let%bind () = fill_in_manifest_cache t in
    time "manifests";
    Path.with_temp_dir (File_name.of_string "fe_cat_") ~f:(fun dir ->
      let%bind () = fill_in_cat_cache t dir in
      time "cats";
      f t
    )
  ;;

end



module Renaming = struct
  type t = {
    src : Path_in_repo.t;
    dst : Path_in_repo.t;
    src_rev : Rev.t;
    dst_rev : Rev.t;
  } [@@deriving sexp_of]
end

type debug =
  [ `Revs of Rev.t Diamond.t
  | `Paths_of_interest of Path_in_repo.Set.t
  | `Attributed_file_diamonds_by_path of Attributed_file.t Diamond.t Path_in_repo.Map.t
  | `Renaming of (Path_in_repo.t * Renaming.t) list Path_in_repo.Map.t
  | `Cache of Cache.t
  | `Result of (Attributed_file.t Diamond.t * Error.t list) list
  ] list
[@@deriving sexp_of]

let verbose = Verbose.worker

let attributed_files_diamonds cache revs attributed_files_by_paths
  : (Attributed_file.t Diamond.t * Error.t list) list =
  let paths_of_interest = Cache.paths_of_interest cache revs in
  let attributed_file_diamonds_by_path =
    Set.to_map paths_of_interest ~f:(fun path ->
      Diamond.map2 revs attributed_files_by_paths
        ~f:(fun rev attributed_file_by_path ->
          match Map.find attributed_file_by_path path with
          | Some v -> v
          | None -> Attributed_file.absent ~path_in_repo:path ~rev))
  in
  let renamings =
    (* hg doesn't have a notion of renaming, so if a file is the source of exactly
       one copy and is deleted, then we say it was renamed to the target of the copy. *)
    List.concat_map (Diamond.edges revs) ~f:(fun (rev1, rev2) ->
      let status = Cache.status cache rev1 rev2 in
      List.filter_map status ~f:(function
        | Added _ -> None
        | Removed file -> Some (file, None)
        | Modified _ -> None
        | Copied { src; dst } -> Some (src, Some dst))
      |> Path_in_repo.Map.of_alist_multi
      |> Map.to_alist
      |> List.filter_map ~f:(function
        | (src, ([None; Some dst] | [Some dst; None])) ->
          let r : Renaming.t = { src; dst; src_rev = rev1; dst_rev = rev2 } in
          Some (src, (dst, r))
        | _ -> None))
    |> Path_in_repo.Map.of_alist_multi
  in
  let debug =
    [ `Revs revs
    ; `Paths_of_interest paths_of_interest
    ; `Attributed_file_diamonds_by_path attributed_file_diamonds_by_path
    ; `Renaming renamings
    ; `Cache cache ]
  in
  let result =
    attributed_file_diamonds_by_path
    |> Map.mapi ~f:(fun ~key:path ~data:v -> Union_find.create (path, (v, [])))
    |> (fun map ->
      let try_merge d1 d2 renaming =
        Or_error.try_with (fun () ->
          Diamond.map2 d1 d2 ~f:(fun x y ->
            match Attributed_file.is_present x, Attributed_file.is_present y with
            | true, true ->
              raise_s [%sexp "Tried to take into account a renaming but couldn't"
                           , Renaming (renaming : Renaming.t)
                           , Clash_on ((x : Attributed_file.t), (y : Attributed_file.t))
                           , In_diamonds
                               ( (d1 : Attributed_file.t Diamond.t)
                               , (d2 : Attributed_file.t Diamond.t))]
            | false, true -> y
            | true, false -> x
            | false, false ->
              (* Because absent attributed files have a filename, here we have to choose
                 if we want to pick the one from x or the one from y. We take [x] because
                 we have to pick one, and [Diff4.create] will normalize after us. *)
              x))
      in
      Map.iteri renamings ~f:(fun ~key:src ~data:dsts ->
        List.iter dsts ~f:(fun (dst, renaming) ->
          match Map.find map src, Map.find map dst with
          | None, None -> ()
          | Some uf, None
          | None, Some uf ->
            let e = Error.create "Dropping rename" renaming [%sexp_of: Renaming.t] in
            let (a, (b, errors)) = Union_find.get uf in
            Union_find.set uf (a, (b, e :: errors))
          | Some uf_left, Some uf_right ->
            if not (Union_find.same_class uf_left uf_right) then (
              let path_left, (files_left, errors_left) = Union_find.get uf_left in
              let path_right, (files_right, errors_right) = Union_find.get uf_right in
              match try_merge files_left files_right renaming with
              | Error e ->
                Union_find.set uf_left (path_left, (files_left, e :: errors_left));
                Union_find.set uf_right (path_right, (files_right, e :: errors_right));
              | Ok files_merged ->
                Union_find.union uf_left uf_right;
                Union_find.set uf_left (path_left, (files_merged, errors_left @ errors_right))
            )));
      map)
    |> Map.data
    |> List.map ~f:Union_find.get
    |> Path_in_repo.Map.of_alist_multi
    |> Map.map ~f:List.hd_exn
    |> Map.data
  in
  (if verbose
   then
     Verbose.message "attributed_files_diamonds"
       (`Result result :: debug) sexp_of_debug);
  result
;;

let create cache revs ~lines_required_to_separate_ddiff_hunks =
  let files_by_paths = Diamond.map revs ~f:(Cache.cat cache) in
  let attributed_files rev (map : Abspath.t Path_in_repo.Map.t) =
    let%map files =
      Deferred.List.map ~how:(`Max_concurrent_jobs 4) (Map.to_alist map)
        ~f:(fun (path_in_repo, file) ->
          let%map v = Cache.attributed_file cache rev path_in_repo file in
          path_in_repo, v)
    in
    Path_in_repo.Map.of_alist_exn files
  in
  let%bind attributed_files_by_paths =
    Diamond.Deferred.map2 revs files_by_paths ~f:attributed_files
  in
  let file_by_path_by_rev =
    Rev.Compare_by_hash.Map.of_alist_fold
      (Diamond.to_list (Diamond.both revs files_by_paths))
      ~init:Path_in_repo.Map.empty
      ~f:(fun acc map ->
        Map.merge acc map
          ~f:(fun ~key:_ -> function
            | `Left x -> Some x
            | `Right x -> Some x
            | `Both (_, x) -> Some x))
  in
  let%map diff4s =
    Deferred.List.map ~how:(`Max_concurrent_jobs 3)
      (attributed_files_diamonds cache revs attributed_files_by_paths)
      ~f:(fun (attr_files, errors) ->
        Diff4.create ~cache:cache.diff4_cache ~file_by_path_by_rev
          ~errors ~lines_required_to_separate_ddiff_hunks attr_files)
  in
  List.filter_map diff4s ~f:(function
    | `Equal -> None
    | `Unequal diff4 -> Some diff4)
;;

let create_using_fake_obligations repo_root rev_diamond
      ~lines_required_to_separate_ddiff_hunks =
  let%bind rev_diamond =
    Diamond.Deferred.map rev_diamond ~f:(fun raw_rev ->
      Raw_rev.resolve_exn raw_rev ~in_:(Ok repo_root))
  in
  let revs =
    Set.to_list (Rev.Compare_by_hash.Set.of_list (Diamond.to_list rev_diamond))
  in
  let%bind obligations_by_rev =
    Deferred.List.map revs ~f:(fun rev ->
      let%map (_witness, e, _format) =
        Rev_facts.Obligations_are_valid.create_exn repo_root
          ~fake_for_testing:Review_attributes.for_testing
          rev
          ~aliases:User_name_by_alternate_name.not_available
      in
      (rev, ok_exn e))
  in
  let obligations_by_rev = Rev.Compare_by_hash.Map.of_alist_exn obligations_by_rev in
  Monitor.try_with_or_error ~extract_exn:true (fun () ->
    Cache.with_ repo_root obligations_by_rev [rev_diamond] ~f:(fun cache ->
      create cache rev_diamond ~lines_required_to_separate_ddiff_hunks))
;;

let command =
  Command.async' ~summary:"compute the diffs between four revs"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and diamond = Diamond.anon (fun node -> Diamond.Node.to_string_short node %: string)
     and lines_required_to_separate_ddiff_hunks =
       let default = Constants.lines_required_to_separate_ddiff_hunks_default in
       flag Switch.lines_required_to_separate_ddiff_hunks
         (optional_with_default default int)
         ~doc:(sprintf "INT required spacing to separate two ddiff hunks (default %d)"
                 default)
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let repo_root = ok_exn Repo_root.program_started_in in
       let%map result =
         create_using_fake_obligations ~lines_required_to_separate_ddiff_hunks
           repo_root (Diamond.map diamond ~f:Raw_rev.string)
       in
       print_endline
         (result |> ok_exn |> [%sexp_of: Diff4.t list] |> Sexp.to_string_hum)
    )
;;
