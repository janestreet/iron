open! Core
module Patdiff_core = Patdiff_lib.Patdiff_core
module Rule  = Patdiff_core.Format.Rule
module Rules = Patdiff_core.Format.Rules
module Style = Patdiff_core.Format.Style
module Color = Patdiff_core.Format.Color

type t = Patdiff_core.Format.Rules.t

let outer_line_change ~style ~name text color =
  let pre =
    Rule.Annex.create ~styles:Style.([ Bold ; Bg color ; Fg Color.White ]) text
  in
  Rule.create ~pre style ~name
;;

let word_change ~name color =
  Rule.create Style.([ Fg color ]) ~name
;;

let inner_default = Patdiff_core.Format.Rules.default

let outer_default =
  { inner_default with
    Rules.
    line_old = outer_line_change  ~name:"line_old" ~style:[] "--" Color.Magenta
  ; line_new = outer_line_change  ~name:"line_new" ~style:[] "++" Color.Cyan
  ; word_old = word_change        ~name:"word_old" Color.Magenta
  ; word_new = word_change        ~name:"word_new" Color.Cyan
  }
;;

(* This is rather hacky, mainly for historical reason. We want to support loading
   existing config files from patdiff *)
let t_of_sexp sexp =
  let config =
    Result.try_with (fun () -> Patdiff_lib.Configuration.Config.t_of_sexp sexp)
    |> function
    | Ok config -> config
    | Error as_new_config_exn ->
      Result.try_with (fun () -> Patdiff_lib.Configuration.Old_config.t_of_sexp sexp)
      |> function
      | Ok old_config -> Patdiff_lib.Configuration.Old_config.to_new_config old_config
      | Error as_old_config_exn ->
        raise_s
          [%sexp "invalid config"
               , { as_new_config_exn : Exn.t
                 ; as_old_config_exn : Exn.t
                 }]
  in
  Patdiff_lib.Configuration.parse config
  |> fun t -> t.Patdiff_lib.Configuration.rules
;;
