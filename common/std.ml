open! Core

include Import

let (>>>) = Async.(>>>)
let (>>=) = Async.(>>=)
let (>>|) = Async.(>>|)

module Abspath                     = Abspath
module Allow_review_for            = Allow_review_for
module Alternate_name              = Alternate_name
module App_harness                 = App_harness
module Ascii_table                 = Iron_ascii_table
module Async_rpc_port              = Async_rpc_port
module Bool                        = Iron_bool
module Build_projection_name       = Build_projection_name
module Cached                      = Cached
module Catch_up_kind               = Catch_up_kind
module Cleanup                     = Cleanup
module Command                     = Iron_command
module Constants                   = Constants
module Continuous_release_status   = Continuous_release_status
module Cr_comment_format           = Cr_comment_format
module Deserializer                = Deserializer
module Diamond                     = Diamond
module Diff4_class                 = Diff4_class
module Digest                      = Iron_digest
module Dynamic_upgrade             = Dynamic_upgrade
module Email_address               = Email_address
module Enum                        = Enum
module Error_context               = Error_context
module Fact                        = Fact
module Feature_id                  = Feature_id
module Feature_name                = Feature_name
module Feature_path                = Feature_path
module File_contents_hash          = File_contents_hash
module File_name                   = File_name
module File_scrutiny               = File_scrutiny
module Group_name                  = Group_name
module Hash_consing                = Hash_consing
module Incr                        = Incr
module Inheritable_attributes      = Inheritable_attributes
module Iron_config                 = Iron_config
module Iron_options                = Iron_options
module Is_archived                 = Is_archived
module Line_count                  = Line_count
module Lock_name                   = Lock_name
module Lru_cache                   = Lru_cache
module Machine_name                = Machine_name
module Make_client_config          = Make_client_config
module Merge_tool                  = Merge_tool
module Metric                      = Metric
module Metric_name                 = Metric_name
module Next_bookmark_update        = Next_bookmark_update
module Next_step                   = Next_step
module Obligations_version         = Obligations_version
module Observer                    = Incr.Observer
module Ok_or_bug                   = Ok_or_bug
module Or_pending                  = Or_pending
module Path                        = Path
module Path_in_repo                = Path_in_repo
module Persistent                  = Persistent
module Properties                  = Properties
module Property                    = Property
module Query                       = Query
module Release_process             = Release_process
module Relpath                     = Relpath
module Remote_repo_path            = Remote_repo_path
module Repo_controller_name        = Repo_controller_name
module Repo_root                   = Repo_root
module Response                    = Response
module Review_kind                 = Review_kind
module Review_obligation           = Review_obligation
module Review_or_commit            = Review_or_commit
module Reviewer                    = Reviewer
module Reviewing                   = Reviewing
module Rpc_description             = Rpc_description
module Rpc_summary                 = Rpc_summary
module Rpc_to_server_prevention    = Rpc_to_server_prevention
module Scrutiny_level              = Scrutiny_level
module Scrutiny_name               = Scrutiny_name
module Send_email_upon             = Send_email_upon
module Serializer                  = Serializer
module Server_down_message         = Server_down_message
module Session_id                  = Session_id
module Switch                      = Switch
module Symbolic_user_set           = Symbolic_user_set
module Tag                         = Tag
module To_goal_via_session         = To_goal_via_session
module Unclean_workspace           = Unclean_workspace
module Unclean_workspace_reason    = Unclean_workspace_reason
module Unix                        = Iron_unix
module Unresolved_name             = Unresolved_name
module User_name                   = User_name
module User_name_by_alternate_name = User_name_by_alternate_name
module User_name_occurrence        = User_name_occurrence
module Uuid                        = Iron_uuid
module Var                         = Incr.Var
module Validated_string            = Validated_string
module Verbose                     = Verbose
module What_diff                   = What_diff
module When_to_first_notify        = When_to_first_notify
module Which_ancestor              = Which_ancestor
module Which_features              = Which_features
module Which_session               = Which_session
module Who_can_release_into_me     = Who_can_release_into_me

module type Persistent = Persistent.S

type 'a _or_pending = 'a Or_pending.t =
  | Pending_since of Time.t
  | Known of 'a

let is_known    = Or_pending.is_known
let is_pending  = Or_pending.is_pending
let known_exn   = Or_pending.known_exn
let pending_now = Or_pending.pending_now

let is_known_error = function
  | Known (Error _) -> true
  | Known (Ok _) | Pending_since _ -> false
;;

let ok_known_exn x = ok_exn (known_exn x)

let check_hash_exn s ~length =
  [%test_result: int] (String.length s) ~expect:length;
  String.iter s ~f:(function
    | '0'..'9' | 'a'..'f' -> ()
    | c -> raise_s [%sexp "unexpected non hexadecimal digit", (c : char), (s : string)])
;;

let repo_mismatch remote_repo_path =
  raise_s
    [%sexp
      "your working directory must be inside a clone of"
    , (remote_repo_path : Remote_repo_path.t)
    ]
;;

let () = print_elapsed [%here]
