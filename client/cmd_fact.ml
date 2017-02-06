open Core
open Async
open Import

module Arg_types = struct
  let spec_id = Command.Arg_type.create Fact.Spec.Id.of_string
  let scope = Command.Arg_type.create Fact.Scope.of_string
end

module Flags = struct
  open Command.Param

  let spec_id () =
    flag "spec-id" (required Arg_types.spec_id)
      ~doc:"SPEC-ID name of fact spec"
  ;;

  let scope () =
    flag "scope" (required Arg_types.scope)
      ~doc:"SCOPE fact scope, e.g. ((rev 1234567890ab) (repo ink))"
  ;;

  let machine ?(extra_doc = "") () =
    flag "machine" no_arg
      ~doc:(" print results in machine-style sexp format (one sexp per line)" ^ extra_doc)
  ;;
end

let cmd_add_fact =
  Command.async'
    ~summary:"add a fact"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and spec_id = Flags.spec_id ()
     and scope = Flags.scope ()
     and comment = flag "comment" (required string)
                     ~doc:"STRING comment about fact assertion"
     and for_ = for_
     and assertion_time = flag "-time" (optional time)
                            ~doc:"TIME override assertion time (defaults to now)"
     in
     fun () ->
       let assertion_time = Option.value assertion_time ~default:(Time.now ()) in
       Fact_action.rpc_to_server_exn
         (spec_id, `add_fact (scope, for_, `comment comment, assertion_time))
    )
;;

let cmd_remove_fact =
  Command.async'
    ~summary:"remove a fact"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and spec_id = Flags.spec_id ()
     and scope = Flags.scope ()
     in
     fun () ->
       Fact_action.rpc_to_server_exn (spec_id, `remove_fact scope)
    )
;;

let cmd_spec_add =
  Command.async'
    ~summary:"add or change a spec"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and spec_id = Flags.spec_id ()
     and file =
       flag "spec-file" (required file) ~doc:"FILE to be parsed by Fact.Spec.t_of_sexp"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind spec = Reader.load_sexp_exn file Fact.Spec.t_of_sexp in
       Fact_action.rpc_to_server_exn (spec_id, `add_spec spec)
    )
;;

let cmd_spec_remove =
  Command.async'
    ~summary:"remove a spec"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and spec_id = Flags.spec_id ()
     in
     fun () ->
       Fact_action.rpc_to_server_exn (spec_id, `remove_spec)
    )
;;

let print_evidence evidence ~machine =
  if machine
  then printf !"%s\n" (Sexp.to_string_mach (Fact.Evidence.sexp_of_t evidence))
  else (
    let { Fact.Evidence.asserter; assertion_time; comment } = evidence in
    printf !"asserter: %{User_name}\n" asserter;
    printf !"assertion_time: %{Time}\n" assertion_time;
    printf !"comment: %s\n" comment)
;;

let cmd_show =
  Command.async'
    ~summary:"show evidence of fact for scope"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and spec_id = Flags.spec_id ()
     and scope = Flags.scope ()
     and machine = Flags.machine ()
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map evidence = Fact_evidence.rpc_to_server_exn (spec_id, scope) in
       print_evidence ~machine evidence
    )
;;

let cmd_list =
  Command.async'
    ~summary:"list all fact evidence for spec"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and spec_id = Flags.spec_id ()
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map l = Fact_list.rpc_to_server_exn spec_id in
       let l =
         List.sort l
           ~cmp:(fun (sc1, _) (sc2, _) -> Fact.Scope.compare sc1 sc2)
       in
       List.iter l ~f:(printf !"%{sexp:Fact.Scope.t * Fact.Evidence.t}\n")
    )
;;

let print_specs specs ~machine ~display_ascii ~max_output_columns =
  if machine
  then List.iter specs ~f:(fun spec ->
    printf "%s\n" (Sexp.to_string_mach ([%sexp_of: Fact.Spec.Id.t * Fact.Spec.t] spec)))
  else (
    let col header get = Ascii_table.Column.(string ~header (cell get)) in
    let spec_id = col "spec id" (fun (id, _) -> Fact.Spec.Id.to_string id) in
    let description = col "description" (fun (_, spec) -> Fact.Spec.description spec) in
    let scope_keys = col "scope keys" (fun (_, spec) ->
      Fact.Spec.scope_keys spec
      |> Set.to_list
      |> List.map ~f:Fact.Scope.Key.to_string
      |> String.concat ~sep:"\n")
    in
    let asserters = col "asserters" (fun (_, spec) ->
      Fact.Spec.authorization_rules spec
      |> List.map ~f:Fact.Spec.Authorization_rule.asserters
      |> User_name.Set.union_list
      |> Set.to_list
      |> List.map ~f:User_name.to_string
      |> String.concat ~sep:"\n")
    in
    let columns = [spec_id; description; scope_keys; asserters] in
    let table = Ascii_table.create ~columns ~rows:specs in
    printf "%s" (Ascii_table.to_string table ~display_ascii ~max_output_columns))
;;

let cmd_spec_list =
  Command.async'
    ~summary:"list available fact specs"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and machine =
       Flags.machine ()
         ~extra_doc:" with all details, including full authorization rules"
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map specs = List_fact_specs.rpc_to_server_exn () in
       print_specs ~machine ~display_ascii ~max_output_columns specs
    )
;;

let cmd_spec =
  Command.group ~summary:"fact spec commands"
    [ "add", cmd_spec_add
    ; "remove", cmd_spec_remove
    ; "list", cmd_spec_list
    ]
;;

let readme () =
  let link = "http://docs/app/fe/fact.html" in
  concat [ "\
The fact-db is a small repository of asserted propositions used primarily to
ensure that certain conditions are met as a prerequisite for rolling out
production systems.  The basic operations of the fact-db are to assert,
retract, and provide evidence for facts that apply to a particular spec for
specific scopes.

Examples of such conditions include evidence that regression tests have passed
and that compliance has approved a change.

For more information, see "; link ; "
"]
;;

let command =
  Command.group ~summary:"fact db commands" ~readme
    [ "add"            , cmd_add_fact
    ; "list"           , cmd_list
    ; "remove"         , cmd_remove_fact
    ; "show"           , cmd_show
    ; "spec"           , cmd_spec
    ]
;;
