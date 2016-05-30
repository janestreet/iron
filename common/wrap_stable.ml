open Core.Stable

module F
    (Stable : Stable_without_comparator)
    (M : sig
       type t
       val of_stable : Stable.t -> t
       val to_stable : t -> Stable.t
     end) = struct

  include M

  let compare t1 t2 = Stable.compare (to_stable t1) (to_stable t2)

  include Binable.Of_binable.V1 (Stable) (struct
      include M
      let of_binable = of_stable
      let to_binable = to_stable
    end)

  include Sexpable.Of_sexpable.V1 (Stable) (struct
      include M
      let of_sexpable = of_stable
      let to_sexpable = to_stable
    end)
end
