open Core.Std
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

module Make
    (Version : Version)
    (Model : Model) = struct

  type t = Model.t

  let of_sexps = ref [ Version.version, Model.t_of_sexp ]

  let find_version version =
    List.Assoc.find !of_sexps ~equal:Int.equal version
  ;;

  let add_version_exn ((version, _) as of_sexp) =
    match find_version version with
    | Some _ -> failwiths "Persistent version already present" version [%sexp_of: int]
    | None   -> of_sexps := of_sexp :: !of_sexps
  ;;

  let to_sexp_default = (Version.version, Model.sexp_of_t)
  let to_sexp = ref None

  module Persist = struct
    type t = Model.t

    let sexp_of_t t =
      let version, sexp_of_model =
        Option.value !to_sexp ~default:to_sexp_default
      in
      { version
      ; value   = sexp_of_model t
      } |> [%sexp_of: Versioned.t]
    ;;

    let t_of_sexp sexp =
      let { Versioned. version; value } = sexp |> [%of_sexp: Versioned.t] in
      match find_version version with
      | Some model_of_sexp -> model_of_sexp value
      | None ->
        of_sexp_error_exn
          (Error.to_exn (Error.create "could not parse versioned sexp" sexp
                           [%sexp_of: Sexp.t]))
          sexp
    ;;
  end

  module Register_read_old_persist
      (Version : Version)
      (Of_sexp : Of_sexp with type model := Model.t) = struct

    let t_of_sexp sexp = sexp |> Of_sexp.t_of_sexp |> Of_sexp.to_model

    let () = add_version_exn (Version.version, t_of_sexp)
  end

  module Register_write_old_persist
      (Version : Version)
      (To_sexp : To_sexp with type model := Model.t) = struct
    let () =
      if not (Option.is_some (find_version Version.version))
      then failwiths "Register_write_old_persist on version not available for reading"
             Version.version [%sexp_of: int];
      if Option.is_some !to_sexp
      then failwiths "Register_write_old_persist called more than once"
             Version.version [%sexp_of: int];
      let sexp_of_model model = model |> To_sexp.of_model |> To_sexp.sexp_of_t in
      to_sexp := Some (Version.version, sexp_of_model)
  end
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
    | Some _ -> failwiths "Persistent version already present" version [%sexp_of: int]
    | None   -> of_sexps := of_sexp :: !of_sexps
  ;;

  let to_sexp_default_fct (_ : Context.t) model = Model.sexp_of_t model
  let to_sexp_default = Version.version, to_sexp_default_fct
  let to_sexp = ref None

  module Writer = struct

    type t = Context.t * Model.t

    module Persist = struct

      type t = Context.t * Model.t

      let sexp_of_t (context, model) =
        let version, sexp_of_model =
          Option.value !to_sexp ~default:to_sexp_default
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

  module Register_read_old_persist
      (Version : Version)
      (Of_sexp : Of_sexp_with_context
       with type model := Model.t
        and type context := Context.t) = struct

    let t_of_sexp sexp context = sexp |> Of_sexp.t_of_sexp |> Of_sexp.to_model context

    let () = add_version_exn (Version.version, t_of_sexp)
  end

  module Register_write_old_persist
      (Version : Version)
      (To_sexp : To_sexp_with_context
       with type model := Model.t
        and type context := Context.t) = struct
    let () =
      if not (Option.is_some (find_version Version.version))
      then failwiths "Register_write_old_persist on version not available for reading"
             Version.version [%sexp_of: int];
      if Option.is_some !to_sexp
      then failwiths "Register_write_old_persist called more than once"
             Version.version [%sexp_of: int];
      let sexp_of_model context model =
        To_sexp.of_model context model |> To_sexp.sexp_of_t
      in
      to_sexp := Some (Version.version, sexp_of_model)
  end
end

