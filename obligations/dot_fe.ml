open! Core
open! Import

let one_or_more name count = sprintf "%s%s" name (if count = 1 then "" else "s")
let num name count = sprintf "%d %s" count (one_or_more name count)

module Followers = struct

  type t =
    | Users of Unresolved_name.t sexp_list
    | Group of Group_name.t
  [@@deriving sexp]

  let t_of_sexp = function
    | Sexp.Atom _ as sexp -> Users [ Unresolved_name.t_of_sexp sexp ]
    | sexp -> t_of_sexp sexp
  ;;

  let sexp_of_t = function
    | Users [ user ] -> Unresolved_name.sexp_of_t user
    | ( Users ([] | _::_::_) | Group _ ) as t -> sexp_of_t t
  ;;

  let eval t e ~aliases ~allowed_users ~known_groups =
    match t with
    | Group group -> Users.eval_group group e ~aliases ~allowed_users ~known_groups
    | Users users ->
      users
      |> List.map ~f:(fun user -> Users.eval_user user e ~aliases ~allowed_users)
      |> User_name.Set.of_list
  ;;
end

module Declaration = struct
  type u = (syntax, Sexp.t) And_sexp.t
  and syntax =
    | Build_projections        of Build_projection_name.t sexp_list
    | Owner                    of Unresolved_name.t
    | Followers                of Followers.t sexp_list
    | Reviewed_by              of Reviewed_by.t
    | Scrutiny                 of Scrutiny_name.t
    | Tags                     of Tag.t sexp_list
    | Fewer_than_min_reviewers of bool
    | More_than_max_reviewers  of bool
    | Apply_to                 of File_set.t
    | Local                    of u sexp_list
    | Used_in_subdirectory
  [@@deriving sexp]

  (* The outer annotated sexp is from the [.fe.sexp] file.  The inner sexps occur inside
     the outer annotated sexp. *)
  type t = (u, Sexp.Annotated.t) And_sexp.t

  let sexp_of_t (t : t) = t.syntax.syntax |> [%sexp_of: syntax]

  let of_annotated_sexp annotated_sexp : t =
    { syntax = annotated_sexp |> Sexp.Annotated.get_sexp |> [%of_sexp: u]
    ; sexp   = Some annotated_sexp
    }
  ;;
end

type t = Declaration.t list

let has_used_in_subdirectory (t : t) =
  with_return (fun r ->
    let rec loop (u : Declaration.u) =
      match u.syntax with
      | Used_in_subdirectory -> r.return true
      | Local us -> List.iter us ~f:loop
      | Build_projections _
      | Owner _
      | Followers _
      | Reviewed_by _
      | Scrutiny _
      | Tags _
      | Fewer_than_min_reviewers _
      | More_than_max_reviewers _
      | Apply_to _
        -> ()
    in
    List.iter t ~f:(fun (d : Declaration.t) -> loop d.syntax);
    false)
;;

let file_name_can_have_tags file_name =
  let basename, _extension = Filename.split_extension (File_name.to_string file_name) in
  String.equal basename "README"
;;

module Partial_attributes = struct
  type t =
    { build_projections        : Build_projection_name.Set.t option
    ; tags                     : Tag.Set.t                   option
    ; fewer_than_min_reviewers : bool                        option
    ; more_than_max_reviewers  : bool                        option
    ; owner                    : User_name.t                 option
    ; followers                : User_name.Set.t             option
    ; review_obligation        : Review_obligation.t         option
    ; scrutiny_name            : Scrutiny_name.t             option
    ; unapplied_attributes     : Error_context.t sexp_opaque list
    }
  [@@deriving fields, sexp_of]

  let empty =
    { build_projections        = None
    ; tags                     = None
    ; fewer_than_min_reviewers = None
    ; more_than_max_reviewers  = None
    ; owner                    = None
    ; followers                = None
    ; review_obligation        = None
    ; scrutiny_name            = None
    ; unapplied_attributes     = []
    }
  ;;

  let apply_tags t ~to_:file_name =
    if file_name_can_have_tags file_name
    then Option.value t.tags ~default:Tag.Set.empty
    else Tag.Set.empty
  ;;

  let assert_has_been_applied t version =
    if Obligations_version.is_at_least_version version ~version:V4
    then (
      match t.unapplied_attributes with
      | []     -> ()
      | e :: _ ->
        Error_context.raise_s e
          [%sexp "some attribute changes have not been applied to any files"])
  ;;

  let merge t1 t2 ~file ~error_context =
    let module M = struct
      module type S = sig
        type t [@@deriving sexp_of]
        val equal : t -> t -> bool
      end
    end in
    let open M in
    let merge_ok f field =
      let o1 = Field.get field t1 in
      let o2 = Field.get field t2 in
      match o1, o2 with
      | None   , None    -> None
      | Some _ , None    -> o1
      | None   , Some _  -> o2
      | Some a1, Some a2 -> Some (f a1 a2)
    in
    let merge_eq (type a) error_message (m : (module S with type t = a)) =
      let module M = (val m) in
      merge_ok (fun m1 m2 ->
        if not (M.equal m1 m2)
        then
          Error_context.raise_s error_context
            [%sexp
              (sprintf "inconsistent %s for %s" error_message (File_name.to_string file) :
                 string)
            , ((m1, m2) : M.t * M.t)
            ];
        m1)
    in
    Fields.map
      ~build_projections:        (merge_ok Set.union)
      ~tags:                     (merge_ok Set.union)
      ~fewer_than_min_reviewers: (merge_eq "Fewer_than_min_reviewers" (module Bool))
      ~more_than_max_reviewers:  (merge_eq "More_than_max_reviewers" (module Bool))
      ~owner:                    (merge_eq "owners" (module User_name))
      ~followers:                (merge_ok Set.union)
      ~review_obligation:        (merge_ok (fun r1 r2 -> Review_obligation.and_ [ r1; r2 ]))
      ~scrutiny_name:            (merge_eq "scrutinies" (module Scrutiny_name))
      ~unapplied_attributes:     (fun f -> Field.get f t1 @ Field.get f t2)
  ;;

  let to_attributes t ~(obligations_repo : Obligations_repo.t) file_name =
    let build_projections =
      Option.value t.build_projections ~default:Build_projection_name.Set.empty
    in
    let tags = apply_tags t ~to_:file_name in
    let followers = Option.value t.followers ~default:User_name.Set.empty in
    match t.owner with
    | None -> error_string "missing owner"
    | Some owner ->
      let review_obligation =
        Option.value t.review_obligation ~default:Review_obligation.none
      in
      let scrutiny_name =
        match t.scrutiny_name with
        | Some n -> Ok n
        | None ->
          let default_scrutinies =
            List.map (Set.to_list build_projections) ~f:(fun build_projection ->
              (Map.find_exn obligations_repo.build_projections build_projection)
              .default_scrutiny.name)
            |> Scrutiny_name.Set.of_list
            |> Set.to_list
          in
          match default_scrutinies with
          | [ scrutiny_name ] -> Ok scrutiny_name
          | [] -> error_string "missing scrutiny"
          | _ -> error "multiple default scrutinies" default_scrutinies
                   [%sexp_of: Scrutiny_name.t list]
      in
      match scrutiny_name with
      | Error _ as x -> x
      | Ok scrutiny_name ->
        match Map.find obligations_repo.scrutinies scrutiny_name with
        | None ->
          error_s [%sexp "undefined scrutiny", (scrutiny_name : Scrutiny_name.t)]
        | Some { Scrutiny.
                 min_file_reviewers
               ; max_file_reviewers
               ; level
               ; read_by_whole_feature_reviewers
               ; _
               } ->
          let num_reviewers_lower_bound =
            Review_obligation.num_reviewers_lower_bound review_obligation
          in
          let fewer_than_min_reviewers = num_reviewers_lower_bound < min_file_reviewers in
          let more_than_max_reviewers  = num_reviewers_lower_bound > max_file_reviewers in
          let fewer_than_min_reviewers_expected =
            Option.value t.fewer_than_min_reviewers ~default:false
          in
          let more_than_max_reviewers_expected =
            Option.value t.more_than_max_reviewers ~default:false
          in
          if more_than_max_reviewers && not more_than_max_reviewers_expected
          then error (sprintf "allowed up to %s, but there are %d"
                        (num "reviewer" max_file_reviewers)
                        num_reviewers_lower_bound)
                 review_obligation [%sexp_of: Review_obligation.t]
          else if not more_than_max_reviewers && more_than_max_reviewers_expected
          then error (sprintf "\
there are %s, with %d allowed, so must have (More_than_max_reviewers false)"
                        (num "reviewer" num_reviewers_lower_bound)
                        max_file_reviewers)
                 review_obligation [%sexp_of: Review_obligation.t]
          else if fewer_than_min_reviewers && not fewer_than_min_reviewers_expected
          then error (sprintf "need %s but there are only %d"
                        (num "reviewer" min_file_reviewers)
                        num_reviewers_lower_bound)
                 review_obligation [%sexp_of: Review_obligation.t]
          else if not fewer_than_min_reviewers && fewer_than_min_reviewers_expected
          then error (sprintf "\
there are %s and only %d required, so must have (Fewer_than_min_reviewers false)"
                        (num "reviewer" num_reviewers_lower_bound)
                        min_file_reviewers)
                 review_obligation [%sexp_of: Review_obligation.t]
          else
            Ok (Review_attributes.create
                  ~build_projections
                  ~tags
                  ~fewer_than_min_reviewers
                  ~followers
                  ~more_than_max_reviewers
                  ~is_read_by_whole_feature_reviewers:read_by_whole_feature_reviewers
                  ~owner
                  ~review_obligation
                  ~scrutiny_level:level
                  ~scrutiny_name)
  ;;
end

let eval ts ~dot_fe ~used_in_subdirectory ~used_in_subdirectory_declaration_is_allowed
      ~files_in_directory ~obligations_repo ~aliases =
  let { Obligations_repo.
        users                   = allowed_users
      ; groups                  = known_groups
      ; tags                    = known_tags
      ; scrutinies
      ; build_projections
      ; disallow_useless_dot_fe = _
      ; allow_review_for        = _
      ; obligations_version
      } = obligations_repo
  in
  let partial_attributes_by_file_name =
    File_name.Table.of_alist_exn
      (List.map (Set.to_list files_in_directory) ~f:(fun file_name ->
         (file_name, Partial_attributes.empty)))
  in
  let rec eval_one (u : Declaration.u) e ~partial_attributes ~depth =
    let e = Error_context.augment e ?sexp:u.sexp in
    match u.syntax with
    | Apply_to file_set ->
      let files = File_set.eval file_set ~universe:files_in_directory e in
      (if Set.is_empty files
       && Obligations_version.is_at_least_version obligations_version ~version:V4
       then Error_context.raise_s e
              [%sexp "attributes are applied to an empty list of files"]);
      Set.iter files ~f:(fun file ->
        Hashtbl.update partial_attributes_by_file_name file ~f:(function
          | None -> assert false
          | Some prev ->
            Partial_attributes.merge prev partial_attributes ~file ~error_context:e));
      { partial_attributes with unapplied_attributes = [] }
    | Build_projections build_projection_names ->
      (match
         List.map build_projection_names ~f:(fun b -> b, ())
         |> Build_projection_name.Table.of_alist_report_all_dups
       with
       | `Ok _ -> ()
       | `Duplicate_keys bs ->
         Error_context.raise_s e
           [%sexp "duplicate build projections", (bs : Build_projection_name.t list)]);
      let undefined =
        List.filter build_projection_names ~f:(fun build_projection_name ->
          not (Map.mem build_projections build_projection_name))
      in
      if not (List.is_empty undefined)
      then
        Error_context.raise_s e
          [%sexp
            (sprintf "undefined build %s"
               (one_or_more "projection" (List.length undefined)) : string)
          , (undefined : Build_projection_name.t list)
          ];
      { partial_attributes with
        build_projections =
          Some (Build_projection_name.Set.of_list build_projection_names)
      ; unapplied_attributes = e :: partial_attributes.unapplied_attributes
      }
    | Local ts ->
      let local_attributes =
        List.fold ts ~init:partial_attributes ~f:(fun partial_attributes t ->
          eval_one t e ~partial_attributes ~depth:(succ depth))
      in
      Partial_attributes.assert_has_been_applied local_attributes obligations_version;
      { partial_attributes with unapplied_attributes = [] }
    | Owner owner ->
      { partial_attributes with
        owner = Some (Users.eval_user owner e ~aliases ~allowed_users)
      ; unapplied_attributes = e :: partial_attributes.unapplied_attributes
      }
    | Followers followers ->
      let followers =
        followers
        |> List.map ~f:(fun followers ->
          Followers.eval followers e ~aliases ~allowed_users ~known_groups)
        |> User_name.Set.union_list
      in
      { partial_attributes with
        followers = Option.some_if (not (Set.is_empty followers)) followers
      ; unapplied_attributes = e :: partial_attributes.unapplied_attributes
      }
    | Reviewed_by reviewed_by ->
      { partial_attributes with
        review_obligation
        = Some (Reviewed_by.eval reviewed_by e ~aliases ~allowed_users ~known_groups)
      ; unapplied_attributes = e :: partial_attributes.unapplied_attributes
      }
    | Scrutiny scrutiny_name ->
      if not (Map.mem scrutinies scrutiny_name)
      then
        Error_context.raise_s e
          [%sexp "undefined scrutiny", (scrutiny_name : Scrutiny_name.t)];
      { partial_attributes with
        scrutiny_name = Some scrutiny_name
      ; unapplied_attributes = e :: partial_attributes.unapplied_attributes
      }
    | Tags tags ->
      (match
         List.map tags ~f:(fun t -> t, ())
         |> Tag.Table.of_alist_report_all_dups
       with
       | `Ok _ -> ()
       | `Duplicate_keys ts ->
         Error_context.raise_s e [%sexp "duplicate tags", (ts : Tag.t list)]);
      (if not used_in_subdirectory_declaration_is_allowed
       && not (List.is_empty tags)
       && not (Set.exists files_in_directory ~f:file_name_can_have_tags)
       then
         Error_context.raise_s e
           [%sexp "no file in directory supporting tags", (tags : Tag.t list)]);
      let undefined = List.filter tags ~f:(fun tag -> not (Set.mem known_tags tag)) in
      (if not (List.is_empty undefined)
       then
         Error_context.raise_s e
           [%sexp
             (sprintf "undefined %s"
                (one_or_more "tag" (List.length undefined)) : string)
           , (undefined : Tag.t list)
           ]);
      { partial_attributes with
        tags = Some (Tag.Set.of_list tags)
      ; unapplied_attributes = e :: partial_attributes.unapplied_attributes
      }
    | Fewer_than_min_reviewers fewer_than_min_reviewers ->
      { partial_attributes with
        fewer_than_min_reviewers = Some fewer_than_min_reviewers
      ; unapplied_attributes = e :: partial_attributes.unapplied_attributes
      }
    | More_than_max_reviewers more_than_max_reviewers ->
      { partial_attributes with
        more_than_max_reviewers = Some more_than_max_reviewers
      ; unapplied_attributes = e :: partial_attributes.unapplied_attributes
      }
    | Used_in_subdirectory ->
      (if not used_in_subdirectory_declaration_is_allowed
       then Error_context.raise_f e "unnecessary Used_in_subdirectory declaration" ());
      (if depth > 0
       && Obligations_version.is_at_least_version obligations_version ~version:V3
       then Error_context.raise_f e
              "Used_in_subdirectory is not allowed inside a Local declaration" ());
      partial_attributes
  in
  Error_context.within ~file:dot_fe (fun e ->
    let e =
      if Relpath.equal used_in_subdirectory Relpath.empty
      then e
      else Error_context.augment e
             ~info:(Info.create "used in subdirectory" used_in_subdirectory
                      [%sexp_of: Relpath.t])
    in
    let partial_attributes =
      List.fold ts ~init:Partial_attributes.empty
        ~f:(fun partial_attributes (t : Declaration.t) ->
          eval_one t.syntax ~partial_attributes ~depth:0
            (Error_context.augment e ?annotated_sexp:t.sexp))
    in
    Partial_attributes.assert_has_been_applied partial_attributes obligations_version;
    let problems = ref [] in
    let alist =
      List.filter_map (Hashtbl.to_alist partial_attributes_by_file_name)
        ~f:(fun (file_name, partial_attributes) ->
          match
            Or_error.try_with_join (fun () ->
              Partial_attributes.to_attributes partial_attributes
                ~obligations_repo file_name)
          with
          | Ok attributes -> Some (file_name, attributes)
          | Error error -> problems := (file_name, error) :: !problems; None)
    in
    if not (List.is_empty !problems)
    then
      Error_context.raise_s e
        [%sexp
          "invalid file attributes"
        , (List.map !problems ~f:(fun (file, error) ->
          [%sexp
            { file  : File_name.t
            ; error : Error.t
            }
          ]) : Sexp.t list)
        ];
    File_name.Map.of_alist_exn alist)
;;
