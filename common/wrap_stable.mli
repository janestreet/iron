open! Core.Std
open! Import

module F (Stable : Stable_without_comparator)
    (M : sig
       type t
       val of_stable : Stable.t -> t
       val to_stable : t -> Stable.t
     end)
  : Stable_without_comparator with type t = M.t
