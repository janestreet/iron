
module Scope = struct

  module Key = Validated_string.Make (struct
      let module_name = "Iron_common.Fact.Scope.Key"
      let check_valid _ = Core.Result.Ok ()
    end) ()

  module Stable = struct
    open Core.Core_stable
    module V1 = struct
      type t = string Key.Stable.V1.Map.t [@@deriving sexp, bin_io, compare]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| aa41ee0fc9442ac7cd1b3ce2e5196e91 |}]
      ;;
    end
  end

  open Core

  module T = struct
    include Stable.V1
    let hash t = Key.Stable.V1.Map.hash String.hash t
  end
  include T
  include Hashable.Make (T)
  include Comparable.Make (T)
  include Sexpable.To_stringable(T)
end

module Spec = struct
  module Id = Validated_string.Make (struct
      let module_name = "Iron_common.Fact.Spec.Id"
      let check_valid _ = Core.Result.Ok ()
    end) ()

  module Stable = struct

    open Core.Core_stable

    module Authorization_rule = struct
      module V1 = struct
        type t =
          { asserters : User_name.Stable.V1.Set.t
          ; scope_restrictions : Iron_string.Stable.V1.Set.t Scope.Key.Stable.V1.Map.t sexp_option
          } [@@deriving sexp, bin_io, compare]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 00119ddb042afa066378835067beb165 |}]
        ;;
      end
    end

    module V1 = struct

      type t =
        { scope_keys : Scope.Key.Stable.V1.Set.t
        ; authorization_rules : Authorization_rule.V1.t list
        ; description : string
        } [@@deriving sexp, bin_io, compare]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| a33f47db1e91310e63c3171aab6d5a2a |}]
      ;;

      open Core
      let t_of_sexp sexp =
        let t = t_of_sexp sexp in
        (* Ensure that authorization rules only refer to legitimate scope keys. *)
        List.iter t.authorization_rules ~f:(fun rule ->
          Option.iter rule.Authorization_rule.V1.scope_restrictions
            ~f:(fun scope_restrictions ->
              let invalid_scope_keys =
                Set.diff (Scope.Key.Set.of_list (Map.keys scope_restrictions))
                  t.scope_keys
              in
              if not (Set.is_empty invalid_scope_keys) then
                raise (Of_sexp_error ((
                  Error.to_exn (Error.create "invalid scope keys"
                                  invalid_scope_keys Scope.Key.Set.sexp_of_t)), sexp))));
        t
      ;;
    end
  end

  open Core

  module Authorization_rule = struct
    type t = Stable.Authorization_rule.V1.t =
      { asserters : User_name.Set.t
      ; scope_restrictions : String.Set.t Scope.Key.Map.t sexp_option
      } [@@deriving sexp, fields]
  end

  type t = Stable.V1.t =
    { scope_keys : Scope.Key.Set.t
    ; authorization_rules : Authorization_rule.t list
    ; description : string
    } [@@deriving sexp, fields]

  let is_asserter t scope user =
    List.exists t.authorization_rules
      ~f:(fun { Authorization_rule.asserters; scope_restrictions} ->
        Set.mem asserters user
        &&
        (match scope_restrictions with
         | None -> true
         | Some scope_restrictions ->
           List.for_all (Map.to_alist scope_restrictions) ~f:(fun (key, legal_values) ->
             Set.mem legal_values (Map.find_exn scope key))))
  ;;
end

module Evidence = struct
  module Stable = struct
    open Core.Core_stable
    module V1 = struct
      module Time = Iron_time.Stable

      type t =
        { asserter : User_name.Stable.V1.t
        ; assertion_time : Time.V1_round_trippable.t
        ; comment : string
        } [@@deriving sexp, bin_io, compare]

    end
  end

  open Core

  type t = Stable.V1.t =
    { asserter : User_name.t
    ; assertion_time : Iron_time.t
    ; comment : string
    } [@@deriving sexp_of, fields]
end

module Action = struct
  module Stable = struct
    open Core.Core_stable
    module V2 = struct
      type t =
        Spec.Id.Stable.V1.t
        * [ `add_spec of Spec.Stable.V1.t
          | `remove_spec
          | `add_fact of Scope.Stable.V1.t
                         * User_name.Stable.V1.t
                         * [`comment of string]
                         * Iron_time.Stable.V1_round_trippable.t
          | `remove_fact of Scope.Stable.V1.t
          ]
      [@@deriving sexp, bin_io, compare]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 26463e805cec97a9c8a6ba8337f9dcaf |}]
      ;;
    end
    module V1 = struct
      type t =
        Spec.Id.Stable.V1.t
        * [ `add_spec of Spec.Stable.V1.t
          | `remove_spec
          | `add_fact of Scope.Stable.V1.t
                         * User_name.Stable.V1.t
                         * [`comment of string]
          | `remove_fact of Scope.Stable.V1.t
          ]
      [@@deriving sexp, bin_io, compare]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 9132cc0a8b5e2e0024141d1a48840f1d |}]
      ;;

      let to_model (spec_id, action) =
        match action with
        | (`add_spec _  | `remove_spec | `remove_fact _) as a -> (spec_id, a)
        | `add_fact (scope, asserter, comment) ->
          (spec_id, `add_fact (scope, asserter, comment, Iron_time.epoch))
    end
  end

  include Stable.V2
  module Persist = struct
    include Persistent.Make
        (struct let version = 2 end)
        (Stable.V2)
    include Register_read_old_version
        (struct let version = 1 end)
        (Stable.V1)
  end
end

open Core

module Db = struct
  type t =
    { specs : Spec.t Spec.Id.Table.t
    ; evidence : Evidence.t Scope.Table.t Spec.Id.Table.t
    } [@@deriving sexp_of]

  let invariant (_ : t) = ()

  let create () =
    { specs = Spec.Id.Table.create ()
    ; evidence = Spec.Id.Table.create ()
    }
  ;;

  let spec t spec_id =
    Result.of_option (Hashtbl.find t.specs spec_id)
      ~error:(Error.create "no spec defined for id" spec_id Spec.Id.sexp_of_t)
  ;;

  let list_specs t = Hashtbl.to_alist t.specs

  let evidence t spec_id scope =
    let open Or_error.Monad_infix in
    Result.of_option (Hashtbl.find t.evidence spec_id)
      ~error:(Error.create "no evidence for spec id" spec_id Spec.Id.sexp_of_t)
    >>= fun evidence_by_scope ->
    Result.of_option (Hashtbl.find evidence_by_scope scope)
      ~error:(Error.create_s [%sexp "no evidence for scope"
                                  , { scope   : Scope.t
                                    ; spec_id : Spec.Id.t
                                    }])
  ;;

  let add_fact t spec_id scope ~asserter ~comment ~assertion_time =
    let open Or_error.Monad_infix in
    spec t spec_id
    >>= fun spec ->
    let expected_scope_keys = Spec.scope_keys spec in
    let provided_scope_keys = Scope.Key.Set.of_list (Map.keys scope) in
    if not (Set.equal expected_scope_keys provided_scope_keys)
    then Or_error.error "unexpected scope keys"
           (spec_id, `Expected expected_scope_keys, `Got provided_scope_keys)
           ([%sexp_of: Spec.Id.t
                       * [ `Expected of Scope.Key.Set.t ]
                       * [ `Got of Scope.Key.Set.t ]])
    else if not (Spec.is_asserter spec scope asserter)
    then Or_error.error "user not authorized for assertion"
           (spec_id, asserter)
           ([%sexp_of: Spec.Id.t * User_name.t])
    else (
      let evidence_by_scope =
        Hashtbl.find_or_add t.evidence spec_id ~default:Scope.Table.create
      in
      let evidence = { Evidence.asserter; assertion_time; comment } in
      Hashtbl.set evidence_by_scope ~key:scope ~data:evidence;
      Ok ())
  ;;

  let remove_fact t spec_id scope =
    let open Or_error.Monad_infix in
    Result.of_option (Hashtbl.find t.evidence spec_id)
      ~error:(Error.create "no evidence for spec id" spec_id Spec.Id.sexp_of_t)
    >>= fun evidence_by_scope ->
    Result.of_option (Hashtbl.find evidence_by_scope scope)
      ~error:(Error.create_s [%sexp "no evidence for scope"
                                  , { scope   : Scope.t
                                    ; spec_id : Spec.Id.t
                                    }])
    >>= fun _ ->
    Ok (Hashtbl.remove evidence_by_scope scope)
  ;;

  let add_spec t spec_id spec =
    Hashtbl.set t.specs ~key:spec_id ~data:spec;
    Ok ()
  ;;

  let remove_spec t spec_id =
    Hashtbl.remove t.specs spec_id;
    Ok ()
  ;;

  let handle_action t (spec_id, which) =
    match which with
    | `add_spec spec
      -> add_spec    t spec_id spec
    | `remove_spec
      -> remove_spec t spec_id
    | `add_fact (scope, asserter, `comment comment, assertion_time)
      -> add_fact    t spec_id scope ~asserter ~comment ~assertion_time
    | `remove_fact scope
      -> remove_fact t spec_id scope
  ;;

  let list_evidences t spec_id =
    match Hashtbl.find t.evidence spec_id with
    | None -> Result.map (spec t spec_id) ~f:(fun (_ : Spec.t) -> [])
    | Some table -> Ok (Hashtbl.to_alist table)
  ;;
end

module Stable = struct
  module Scope = struct
    module Key = Scope.Key.Stable
    include Scope.Stable
  end

  module Spec = struct
    module Id = Spec.Id.Stable
    include Spec.Stable
  end

  module Evidence = Evidence.Stable
  module Action = Action.Stable
end
