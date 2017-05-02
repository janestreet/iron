open Core

let env_var = "IRON_OPTIONS"

let concat = String.concat

module Verbose_tag = struct
  module T = struct
    type t =
      | All
      | Build_order
      | Command
      | Command_rpc
      | Cr_comment
      | Deserializer
      | Hg
      | Knowledge
      | Patdiff4
      | Rpc
      | Serializer
      | Worker
      | Workspaces
    [@@deriving compare, enumerate, sexp]

    let equal = [%compare.equal: t]
  end

  include T

  include Sexpable.To_stringable (T)

end

type t =
  { display_ascii_always              : bool               sexp_option
  ; find_features_by_partial_name     : bool               sexp_option
  ; load_user_configs                 : bool               sexp_option
  ; silence_uninteresting_hg_warnings : bool               sexp_option
  ; verbose                           : Verbose_tag.t list sexp_option
  ; workspaces                        : bool               sexp_option
  }
[@@deriving fields, sexp]

let empty =
  { display_ascii_always              = None
  ; find_features_by_partial_name     = None
  ; load_user_configs                 = None
  ; silence_uninteresting_hg_warnings = None
  ; verbose                           = None
  ; workspaces                        = None
  }
;;

let default =
  { display_ascii_always              = Some false
  ; find_features_by_partial_name     = Some true
  ; load_user_configs                 = Some true
  ; silence_uninteresting_hg_warnings = Some false
  ; verbose                           = Some []
  ; workspaces                        = None
  }
;;

let example =
  { display_ascii_always              = Some false
  ; find_features_by_partial_name     = Some true
  ; load_user_configs                 = Some true
  ; silence_uninteresting_hg_warnings = Some false
  ; verbose                           = Some [ Hg; Rpc ]
  ; workspaces                        = Some false
  }
;;

let merge t1 t2 =
  let override field = Option.first_some (Field.get field t1) (Field.get field t2) in
  let verbose _ =
    match t1.verbose, t2.verbose with
    | None, None          -> None
    | None, (Some _ as s) -> s
    | (Some _ as s), None -> s
    | Some t1, Some t2    -> Some (List.dedup_and_sort ~compare:Verbose_tag.compare (t1 @ t2))
  in
  Fields.map
    ~display_ascii_always:override
    ~find_features_by_partial_name:override
    ~load_user_configs:override
    ~silence_uninteresting_hg_warnings:override
    ~verbose:verbose
    ~workspaces:override
;;

let reduce ts = List.fold_right ~init:empty ~f:merge ts

let field_descriptions () : string =
  let field to_sexp description ac field =
    (Field.name field,
     to_sexp (Option.value_exn (Field.get field default)),
     description
    ) :: ac
  in
  let opt_field to_sexp description ac field =
    (Field.name field,
     to_sexp (Field.get field default),
     description
    ) :: ac
  in
  let fields =
    Fields.fold ~init:[]
      ~display_ascii_always:
        (field [%sexp_of: bool]
           ["
  If [true] then only use ASCII characters in output, eliding ANSI formatting escapes and
  avoiding unicode.  Also, Iron will act as if the switch [-display-ascii] was passed to
  all commands.
"])
      ~find_features_by_partial_name:
        (field [%sexp_of: bool]
           ["
  If [false] then fail on feature paths given partially instead of completing them.
"])
      ~load_user_configs:
        (field [%sexp_of: bool]
           ["
  If [false] then do not try to load $HOME/.${config} such as .ferc or .patdiff4 files
"])
      ~silence_uninteresting_hg_warnings:
        (field [%sexp_of: bool]
           ["
  If true then filter out from stderr some warnings produced by hg, such as:
  waiting for lock on repository ... held by ...
"])
      ~verbose:
        (field [%sexp_of: Verbose_tag.t list]
           ["
  A list of tags specifying which Iron functions should print debug
  messages to stderr.  Each tag identifies a group of related
  functions.  The tag 'all' means to print debug messages for all
  functions.  Allowed values are:

";
            concat (List.map Verbose_tag.all
                      ~f:(fun d ->
                        concat ["    "; Verbose_tag.to_string d; "\n"]))
           ])
      ~workspaces:
        (opt_field [%sexp_of: bool option]
           ["
  Overwrite whether workspaces are enabled, taking priority over what .ferc says.
"])
  in
  concat
    (List.map
       (List.sort fields
          ~cmp:(fun (name1, _, _) (name2, _, _) -> String.compare name1 name2))
       ~f:(fun (name, default, description) ->
         concat ("\n"
                 :: name :: " (default " :: Sexp.to_string default :: ")"
                 :: description)))
;;

let help_message () =
  concat [
    "\
The "; env_var;" environment variable affects Iron in various ways.
Its value should be a sexp of the following form, where all fields are optional:

";
    Sexp.to_string_hum (sexp_of_t example)
    ;"

It is possible to extend a previously defined variable by appending the new fields
to the left of the variable, for example as in:

    IRON_OPTIONS=\"((find_features_by_partial_name false))$IRON_OPTIONS\"

In case the same property appears several times, the values are merged if possible,
otherwise the left most value is preferred.

Here is an explanation of each field.
";
    field_descriptions ();
  ]
;;

let usage () = eprintf "%s%!" (help_message ()); exit 1

let t =
  match Sys.getenv env_var with
  | None -> empty
  | Some "" -> usage ()
  | Some string ->
    match Result.try_with (fun () ->
      Sexp.of_string_conv_exn (concat [ "(" ; String.strip string ; ")" ])
        [%of_sexp: t list])
    with
    | Ok ts -> reduce ts
    | Error exn ->
      eprintf "%s\n\n"
        (Sexp.to_string_hum
           (Error.sexp_of_t
              (Error.create (sprintf "invalid value for %s environment variable"
                               env_var)
                 exn [%sexp_of: exn])));
      usage ();
;;

module Verbose = struct

  let verbose_for tag =
    match t.verbose with
    | None -> false
    | Some l -> List.mem l tag ~equal:Verbose_tag.equal
  ;;

  let all = verbose_for Verbose_tag.All

  let verbose tag = all || verbose_for tag

  include struct
    open Verbose_tag
    let build_order  = verbose Build_order
    let command      = verbose Command
    let command_rpc  = verbose Command_rpc
    let cr_comment   = verbose Cr_comment
    let deserializer = verbose Deserializer
    let hg           = verbose Hg
    let knowledge    = verbose Knowledge
    let patdiff4     = verbose Patdiff4
    let rpc          = verbose Rpc
    let serializer   = verbose Serializer
    let worker       = verbose Worker
    let workspaces   = verbose Workspaces
  end
end

let default field =
  Option.value (Field.get field t)
    ~default:(Option.value_exn (Field.get field default))
;;

let display_ascii_always = default Fields.display_ascii_always

let find_features_by_partial_name = default Fields.find_features_by_partial_name

let load_user_configs = default Fields.load_user_configs

let silence_uninteresting_hg_warnings = default Fields.silence_uninteresting_hg_warnings

let workspaces_are_enabled__forced_value = t.workspaces

let t =
  { display_ascii_always              = Some display_ascii_always
  ; find_features_by_partial_name     = Some find_features_by_partial_name
  ; load_user_configs                 = Some load_user_configs
  ; silence_uninteresting_hg_warnings = Some silence_uninteresting_hg_warnings
  ; verbose                           = t.verbose
  ; workspaces                        = t.workspaces
  }
;;
