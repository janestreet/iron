open! Core
open! Async
open! Import

module Host   = String
module Office = String

let env_var = "IRON_CONFIG"
let env_var_has_been_read = ref false

let prod_basedir = Abspath.of_string "/j/office/app/fe/prod"

let prod_etc = Abspath.extend prod_basedir (File_name.of_string "etc")

let prod_var = Abspath.extend prod_basedir (File_name.of_string "var")

let deploy_offices : Office.t list =
  [ "etot"
  ; "fen"
  ; "hk1"
  ; "hkg"
  ; "igm"
  ; "ldn"
  ; "tot"
  ; "ves"
  ]
;;

module Rpc_proxy_config : sig

  type t [@@deriving sexp]

  val no_proxy : t
  val find_proxy_host_for_local_office : t -> Host.t option

end = struct
  type t = Host.t Office.Map.t

  module Syntax = struct
    type t = (Host.t * Office.t list) list [@@deriving sexp]
  end

  let t_of_syntax syntax =
    Office.Map.of_alist_exn
      (List.concat_map syntax ~f:(fun (host, offices) ->
         List.map offices ~f:(fun office -> office, host)))
  ;;

  let syntax_of_t t =
    let hosts = Host.Table.create () in
    Map.iteri t ~f:(fun ~key:office ~data:host ->
      Hashtbl.add_multi hosts ~key:host ~data:office);
    Hashtbl.to_alist hosts
  ;;

  include Sexpable.Of_sexpable (Syntax) (struct
      type nonrec t = t
      let to_sexpable = syntax_of_t
      let of_sexpable = t_of_syntax
    end)

  let no_proxy = Office.Map.empty

  let office_of_host_heuristic host =
    Option.map ~f:fst (String.lsplit2 host ~on:'-')
  ;;

  let find_proxy_host_for_local_office t =
    let my_host = Unix.gethostname () in
    match office_of_host_heuristic my_host with
    | None -> None
    | Some office -> Map.find t office
  ;;
end

let serializer_pause_timeout_default = Time.Span.of_min 1.

(* The hydra_user is configurable so that we allow temporary changes via config pending
   a roll, but it changes so rarely that the hard-coding this default value seems
   perfectly fine *)
let default_hydra_user = User_name.of_string "as-hydra"

(* Serialization must have adequate backward and forward compatibility properties, given
   the clients that need consume that type. *)
type t =
  { host                     : string
  ; async_rpc_port           : Async_rpc_port.t
  ; rpc_proxy_config         : Rpc_proxy_config.t [@default Rpc_proxy_config.no_proxy]
  ; hgrc                     : Abspath.t
  ; hydra_user               : User_name.t [@default default_hydra_user]
  ; serializer_pause_timeout : Time.Span.t [@default serializer_pause_timeout_default]
  }
[@@deriving fields, sexp]

let t_of_sexp = Sexp.of_sexp_allow_extra_fields t_of_sexp

let config_file_relative_to_basedir = Relpath.of_string "etc/iron-config.sexp"

let load_exn ~basedir =
  Reader.load_sexp_exn
    (Abspath.to_string (Abspath.append basedir config_file_relative_to_basedir))
    t_of_sexp
;;

let prod () =
  assert (not am_functional_testing);
  load_exn ~basedir:prod_basedir
;;

let load_as_per_IRON_CONFIG () =
  env_var_has_been_read := true;
  match Sys.getenv env_var with
  | Some ("prod" | "PROD") -> prod ()
  | Some s -> return (Sexp.of_string_conv_exn s t_of_sexp)
  | None ->
    if String.is_prefix Core.Sys.executable_name ~prefix:"/j/office/app"
    then prod ()
    else
      failwithf "\
must define %s environment variable or call [Iron_config.use_prod_IRON_CONFIG] prior
to forcing [as_per_IRON_CONFIG]"
        env_var ()
;;

let as_per_IRON_CONFIG = lazy (load_as_per_IRON_CONFIG ())

module Restricted_for_rpcs = struct
  let load_as_per_IRON_CONFIG ~may_connect_to_proxy () =
    let%bind t = load_as_per_IRON_CONFIG () in
    let%map port = Async_rpc_port.port t.async_rpc_port in
    let host =
      if may_connect_to_proxy
      then
        t.rpc_proxy_config
        |> Rpc_proxy_config.find_proxy_host_for_local_office
        |> Option.value ~default:t.host
      else t.host
    in
    Host_and_port.create ~host ~port
  ;;
end

let use_prod_IRON_CONFIG () =
  if !env_var_has_been_read
  then failwithf "use_prod_IRON_CONFIG must be called before %s has been read" env_var ();
  Unix.putenv ~key:env_var ~data:"prod";
;;

let for_checking_invariants =
  { host                     = "localhost"
  ; async_rpc_port           = Static 1
  ; rpc_proxy_config         = Rpc_proxy_config.no_proxy
  ; hgrc                     = Abspath.of_string "/"
  ; hydra_user               = default_hydra_user
  ; serializer_pause_timeout = serializer_pause_timeout_default
  }
;;

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    Fields.iter
      ~host:ignore
      ~async_rpc_port:ignore
      ~rpc_proxy_config:ignore
      ~hgrc:ignore
      ~hydra_user:ignore
      ~serializer_pause_timeout:ignore
  )
;;
