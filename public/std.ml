(** A public interface to Iron to be used in OCaml programs.

    The intended use is to open this std module:

    {[
      open Iron.Std
    ]}

    And then using the functions provided in the [Fe] module, and the submodules of the
    [Iron] module.

    Example:

    {[
      open Core
      open Async
      open Iron.Std

      let is_seconded (feature_path : Iron.Feature_path.t) =
        Fe.get_feature { feature_path } >>| ok_exn
        >>| fun feature ->
        Option.is_some feature.seconder
      ;;

    ]}
*)

include (Export : module type of (struct include Export end)
         with module Iron := Export.Iron)

module Iron = struct
  include Export.Iron
  module Client_config = Client_config
  module Connection    = Connection
  module Param         = Iron_param

  (** When [Sys.executable_name] has prefix "/j/office/app", it will connect to production
      iron by default.  Other executables will require the environment variable
      IRON_CONFIG to be set.  Applications may add an initial call to this function in
      their bin/main.ml file.  It will have the same effect as running the binary with
      IRON_CONFIG=prod *)
  let use_prod_IRON_CONFIG = Iron_config.use_prod_IRON_CONFIG
end

module Fe = struct
  (* The RPCs are exported under [Fe] to qualify the fields of action and
     reaction records, if needed *)
  module Archive                      = Archive
  module Change_feature               = Iron_protocol.Change_feature
  module Compress                     = Compress
  module Create                       = Create
  module Enable_review                = Iron_protocol.Enable_review
  module Fact_action                  = Iron_protocol.Fact_action
  module Fact_evidence                = Iron_protocol.Fact_evidence
  module Feature_exists               = Iron_protocol.Feature_exists
  module Feature_table_of_csv         = Feature_table_of_csv
  module Get_brain                    = Iron_protocol.Get_brain
  module Get_feature                  = Iron_protocol.Get_feature
  module Get_feature_by_id            = Iron_protocol.Get_feature.By_id
  module Get_feature_maybe_archived   = Iron_protocol.Get_feature.Maybe_archived
  module List                         = Fe_list
  module List_feature_names           = Iron_protocol.List_feature_names
  module List_features                = Iron_protocol.List_features
  module Lock                         = Lock
  module Notify_on_descendant_updates = Iron_protocol.Notify_on_descendant_updates
  module Notify_on_feature_updates    = Iron_protocol.Notify_on_feature_updates
  module Obligations                  = Obligations
  module Rebase                       = Rebase
  module Release                      = Release
  module Rename                       = Rename
  module Supported_command_rpcs       = Supported_command_rpcs
  module Unarchive                    = Unarchive
  module Unlock                       = Unlock
  module Update                       = Update
  module Wait_for_hydra               = Wait_for_hydra
  include Fe
end
