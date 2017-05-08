open! Core
open! Import

include Persistent_intf

module Reader = struct
  type 'a t = (module S_reader with type t = 'a)

  let map (type a) (type b) (module Sa : S_reader with type t = a) ~(f : a -> b) =
    let module Sb = struct
      type t = b
      module Persist = struct
        type nonrec t = t
        let t_of_sexp sexp = f (Sa.Persist.t_of_sexp sexp)
      end
    end in
    (module Sb : S_reader with type t = b)
  ;;
end

module Writer = struct
  type 'a t = (module S_writer with type t = 'a)

  let prepend (type a) (type b) (module Sa : S_writer with type t = a) ~(f : b -> a) =
    let module Sb = struct
      type t = b
      module Persist = struct
        type nonrec t = t
        let sexp_of_t b = Sa.Persist.sexp_of_t (f b)
      end
    end in
    (module Sb : S_writer with type t = b)
  ;;
end

module Versioned = struct
  type t =
    { version : int
    ; value   : Sexp.t
    }
  [@@deriving sexp]
end

module Make_with_context
    (Context : Context)
    (Version : Version)
    (Model   : Model) = struct

  let of_sexp_default_fct sexp (_ : Context.t) = Model.t_of_sexp sexp
  let of_sexps = ref [ Version.version, of_sexp_default_fct ]

  let find_version version =
    List.Assoc.find !of_sexps ~equal:Int.equal version
  ;;

  let add_version_exn ((version, _) as of_sexp) =
    match find_version version with
    | Some _ -> raise_s [%sexp "Persistent version already present", (version : int)]
    | None   -> of_sexps := of_sexp :: !of_sexps
  ;;

  let to_sexp_default_fct (_ : Context.t) model = Model.sexp_of_t model
  let to_sexp_default = Version.version, to_sexp_default_fct
  let to_sexp_override = ref None

  module Writer = struct

    type t = Context.t * Model.t

    module Persist = struct

      type t = Context.t * Model.t

      let sexp_of_t (context, model) =
        let version, sexp_of_model =
          match !to_sexp_override with
          | None -> to_sexp_default
          | Some (write_old_version, override) ->
            if write_old_version context
            then override
            else to_sexp_default
        in
        { version
        ; value = sexp_of_model context model
        } |> [%sexp_of: Versioned.t]
      ;;
    end
  end

  module Reader = struct

    type t = Context.t -> Model.t

    module Persist = struct
      type t = Context.t -> Model.t

      let t_of_sexp sexp context =
        let { Versioned. version; value } = sexp |> [%of_sexp: Versioned.t] in
        match find_version version with
        | Some model_of_sexp -> model_of_sexp value context
        | None ->
          of_sexp_error_exn
            (Error.to_exn (Error.create "could not parse versioned sexp" sexp
                             [%sexp_of: Sexp.t]))
            sexp
      ;;
    end
  end

  module Register_read_old_version
      (Version : Version)
      (Conv : sig
         type t [@@deriving of_sexp]
         val to_model : Context.t -> t -> Model.t
       end) = struct

    let t_of_sexp sexp context = sexp |> Conv.t_of_sexp |> Conv.to_model context

    let () = add_version_exn (Version.version, t_of_sexp)
  end

  module Register_read_write_old_version
      (Version : Version)
      (Conv : sig
         type t [@@deriving sexp]
         val of_model : Context.t -> Model.t -> t
         val to_model : Context.t -> t -> Model.t
         val should_write_this_version : Context.t -> bool
       end) = struct

    include Register_read_old_version (Version) (Conv)

    let () =
      if Option.is_some !to_sexp_override
      then raise_s [%sexp "Register_read_write_old_version called more than once"
                        , (Version.version : int)];
      let sexp_of_model context model =
        Conv.of_model context model |> Conv.sexp_of_t
      in
      to_sexp_override :=
        Some (Conv.should_write_this_version, (Version.version, sexp_of_model))
    ;;
  end
end

module Make (Version : Version) (Model : Model) = struct

  module With_context =
    Make_with_context (struct type t = unit [@@deriving sexp_of] end)
      (Version) (Model)

  type t = Model.t

  module Persist = struct
    type nonrec t = t

    let t_of_sexp sexp = With_context.Reader.Persist.t_of_sexp sexp ()
    let sexp_of_t t    = With_context.Writer.Persist.sexp_of_t ((), t)
  end

  module Register_read_old_version
      (Version : Version)
      (Conv : sig
         type t [@@deriving of_sexp]
         val to_model : t -> Model.t
       end) = struct

    include With_context.Register_read_old_version (Version)
        (struct
          type t = Conv.t [@@deriving of_sexp]
          let to_model () t = Conv.to_model t
        end)
  end
end
