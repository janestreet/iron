module Stable = struct
  open! Core.Stable
  module V1 = struct
    (* We choose [type t = string] so that we can add new values without changing the
       serialization format. *)
    module T = struct
      type t = string
      [@@deriving bin_io, compare, sexp]
    end
    include T
    module Unstable : Core.Std.Comparable.S with type t := t
      = Core.Std.Comparable.Make (T)
    include Core.Stable.Comparable.V1.Make (struct
        include T
        include Unstable
      end)
  end
end

open! Core.Std
open! Import

module V = Stable.V1
include V.T
include V.Unstable
include Sexpable.To_stringable (V)

let archive = "archive"
let release = "release"

let all = [ archive; release ]

let default = Set.of_list all

