open Core
open! Import

type t =
  { path           : Abspath.t
  ; human_readable : string option
  }
[@@deriving compare, fields]

let sexp_of_t t = t.path |> [%sexp_of: Abspath.t]

module Hash_by_path = struct
  module T = struct
    type nonrec t = t [@@deriving sexp_of]

    let compare = Comparable.lift [%compare: Abspath.t] ~f:path

    let hash t = Abspath.hash t.path
  end
  include T
  include Hashable.Make_plain(T)
end

let with_human_readable t name =
  { t with human_readable = Some name }
;;

let to_string t = Abspath.to_string t.path

let to_string_hum t =
  match t.human_readable with
  | Some name -> name
  | None -> to_string t
;;

let not_in_repo sexp =
  error_s [%sexp "not inside an hg repo", (sexp : Sexp.t)]
;;

(* Don't skip over a .hg that isn't a directory or can't be read.  Note that it's not
   worth checking now to see if the root is readable & a directory -- the check would
   essentially be repeated when we use the root (by the system call) -- a file system is a
   mutable thing. *)
let containing_root_blocking =
  let dot_hg = File_name.of_string ".hg" in
  fun ?human_readable start ->
    let rec loop path =
      match Unix.access (Abspath.to_string (Abspath.extend path dot_hg)) [`Exists] with
      | Ok ()   -> Ok { path; human_readable }
      | Error _ ->
        match Abspath.parent path with
        | Some path -> loop path
        | None      -> not_in_repo [%sexp (start : Abspath.t)]
    in
    loop start
;;

let program_started_in =
  match Abspath.program_started_in with
  | Error err -> not_in_repo [%sexp (err : Error.t)]
  | Ok started_in ->
    containing_root_blocking ~human_readable:"local repo" started_in
;;

let of_abspath ?human_readable path = { path; human_readable }
let to_abspath t = t.path

let have_same_abspath t1 t2 = Abspath.equal t1.path t2.path

let append t path_in_repo =
  Abspath.append t.path (Path_in_repo.to_relpath path_in_repo)
;;

let relativize_exn t abspath =
  ok_exn (Abspath.chop_prefix ~prefix:(to_abspath t) abspath)
  |> Path_in_repo.of_relpath
;;
