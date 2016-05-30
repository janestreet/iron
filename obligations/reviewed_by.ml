open Core.Std
open Import

module Of : sig
  type t [@@deriving sexp]
  val t : t
end = struct
  type t = unit

  let t = ()

  let sexp_of_t () = Sexp.Atom "of"

  let t_of_sexp sexp =
    match sexp |> [%of_sexp: string] with
    | "of" | "of_" -> ()  (* [of_] is for backward compatibility *)
    | _ -> raise (Of_sexp_error (Failure "expected [of]" , sexp))
  ;;
end

type t = (u, Sexp.t) And_sexp.t
and u =
  | All_of   of Users.t
  | At_least of int * Of.t * Users.t
  | And      of t sexp_list
  | None
  | Or       of t sexp_list
[@@deriving sexp]

let eval t e ~aliases ~allowed_users ~known_groups =
  let rec loops ts e = List.map ts ~f:(fun t -> loop t e)
  and loop (t : t) (e : Error_context.t) =
    let e = Error_context.augment e ?sexp:t.sexp in
    let eval_users users = Users.eval users e ~aliases ~allowed_users ~known_groups in
    match t.syntax with
    | All_of users           -> Review_obligation.all_of (eval_users users)
    | At_least (k, _, users) -> Review_obligation.at_least_wide e k (eval_users users)
    | And ts                 -> Review_obligation.and_ (loops ts e)
    | None                   -> Review_obligation.none
    | Or  ts                 -> Review_obligation.or_wide e (loops ts e)
  in
  let review_obligation = loop t e in
  if not (Review_obligation.is_satisfied review_obligation
            ~by:(Review_obligation.may_reviewers review_obligation))
  then Error_context.error_s e [%sexp "unsatisfiable review obligation", (t : t)];
  review_obligation
;;

let rec synthesize (review_obligation : Review_obligation.t) : t =
  let u =
    match review_obligation with
    | All_of users ->
      if Set.is_empty users
      then None
      else All_of (Users.synthesize users)
    | At_least_wide (k, users) -> At_least (k, Of.t, Users.synthesize users)
    | And ts     -> And (List.map ts ~f:synthesize)
    | Or_wide ts -> Or  (List.map ts ~f:synthesize)
  in
  And_sexp.create u
;;
