module Stable = struct

  open Core.Core_stable

  module V1 = struct

    module T = struct

      type t =
        | Ssh of ssh
        | File of Abspath.Stable.V1.t
      and ssh = { host : string; path : Abspath.Stable.V1.t }

      open Import

      let of_string s =
        match String.chop_prefix s ~prefix:"ssh://" with
        | None -> File (Abspath.of_string s)
        | Some rest ->
          (* This doesn't parse rare things like user names and ports. *)
          let host, path = String.lsplit2_exn rest ~on:'/' in
          Ssh { host; path = (Abspath.of_string path) }
      ;;

      let to_string = function
        | Ssh { host; path } -> Printf.sprintf !"ssh://%s/%{Abspath}" host path
        | File f -> Abspath.to_string f
      ;;

      let compare t1 t2 = String.compare (to_string t1) (to_string t2)

      let equal t1 t2 = compare t1 t2 = 0
    end

    include T
    include Sexpable .Of_stringable.V1 (T)
    include Binable  .Of_stringable.V1 (T)

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
    ;;

    let hash t =
      let open Core in
      let open Import in
      match t with
      | Ssh { host; path } -> Hashtbl.hash (String.hash host, Abspath.hash path)
      | File f -> Abspath.hash f
    ;;
  end
end

open! Core
open! Async
open! Import

include Stable.V1
include Hashable.Make (Stable.V1)

let invariant _ = ()

let ssh host path = Ssh { host; path = Abspath.of_string path }

let jane = ssh "hg" "/hg/jane"
let jane_submissions = ssh "hg" "/hg/jane/submissions"

let null = File (Abspath.of_string "/dev/null")

let%test_unit _ =
  let string = "ssh://hg//hg/jane/submissions" in
  let t = jane_submissions in
  [%test_result: t] (of_string string) ~expect:t;
  [%test_result: string] (to_string t) ~expect:string
;;

let family t =
  match t with
  | Ssh { host = "hg"; path } ->
    (match List.map (Abspath.to_list path) ~f:File_name.to_string with
     | "hg" :: family :: _ -> Some family
     | _ -> None)
  | File _ | Ssh _ -> None
;;

let%test_unit _ =
  List.iter [ Some "jane"  , jane_submissions
            ; Some "friend", ssh "hg" "/hg/friend/branches/friend-assistant-109.60.01"
            ; None         , File (Abspath.of_string "/tmp/foo")
            ]
    ~f:(fun (expect, remote_repo_path) ->
      [%test_result: string option] ~expect (family remote_repo_path))
;;

let is_prefix ~prefix t =
  match (prefix,t) with
  | (File pf, File tf) -> Abspath.is_prefix ~prefix:pf tf
  | Ssh pssh, Ssh tssh ->
    String.equal pssh.host tssh.host
    && Abspath.is_prefix ~prefix:pssh.path tssh.path
  | (Ssh  _,  File _)
  | (File _,  Ssh  _)  -> false
;;

let is_in_jane = is_prefix ~prefix:jane

let%test _ = is_in_jane jane_submissions ;;
