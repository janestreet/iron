open Core
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
  then Error_context.raise_s e [%sexp "unsatisfiable review obligation", (t : t)];
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

let%test_module _ =
  (module struct

    let user1 = User_name.of_string "user1"
    let user2 = User_name.of_string "user2"
    let user3 = User_name.of_string "user3"

    let allowed_users =
      [ user1; user2; user3 ]
      |> List.map ~f:User_name.to_unresolved_name
      |> Unresolved_name.Set.of_list
    ;;

    let known_groups : Groups.t =
      Group_name.Map.singleton (Group_name.of_string "users123") allowed_users
    ;;

    let test_subsets =
      [ User_name.Set.empty
      ; User_name.Set.singleton user1
      ; User_name.Set.singleton user2
      ; User_name.Set.singleton user3
      ; User_name.Set.of_list [ user1; user2 ]
      ; User_name.Set.of_list [ user1; user2; user3 ]
      ]
      |> List.map ~f:(fun set ->
        set,
        set |> Set.to_list |> List.map ~f:User_name.to_string |> String.concat ~sep:" ")
    ;;

    let check str =
      Error_context.within ~file:(Path.of_string ".fe.sexp") (fun e ->
        let t = Sexp.of_string_conv_exn str [%of_sexp: t] in
        let review_obligations =
          eval t e ~aliases:User_name_by_alternate_name.not_available
            ~allowed_users ~known_groups
        in
        List.iter test_subsets ~f:(fun (by, by_str_hum) ->
          let is_satisfied = Review_obligation.is_satisfied review_obligations ~by in
          Printf.printf "%3s: %s\n" (if is_satisfied then "yes" else "no") by_str_hum))
      |> function
      | Ok () -> ()
      | Error err -> print_string (Sexp.to_string_hum [%sexp (err : Error.t)])
    ;;

    let%expect_test _ =
      check "None";
      [%expect {|
  yes:
  yes: user1
  yes: user2
  yes: user3
  yes: user1 user2
  yes: user1 user2 user3
    |}];

      check "(All_of (Users))";
      [%expect {|
  yes:
  yes: user1
  yes: user2
  yes: user3
  yes: user1 user2
  yes: user1 user2 user3
    |}];

      check "(All_of (Users user1))";
      [%expect {|
   no:
  yes: user1
   no: user2
   no: user3
  yes: user1 user2
  yes: user1 user2 user3
    |}];

      check "(Or)";
      [%expect {|
      (.fe.sexp:0:0 "[Or] must have at least one clause")
    |}];

      check "(Or (All_of (Users user1)))";
      [%expect {|
   no:
  yes: user1
   no: user2
   no: user3
  yes: user1 user2
  yes: user1 user2 user3
    |}];

      check "(Or (All_of (Users user1)) (All_of (Users user2)))";
      [%expect {|
   no:
  yes: user1
  yes: user2
   no: user3
  yes: user1 user2
  yes: user1 user2 user3
      |}];

      check "(And)";
      [%expect {|
  yes:
  yes: user1
  yes: user2
  yes: user3
  yes: user1 user2
  yes: user1 user2 user3
    |}];

      check "(And (All_of (Users user1)))";
      [%expect {|
   no:
  yes: user1
   no: user2
   no: user3
  yes: user1 user2
  yes: user1 user2 user3
    |}];

      check "(And (All_of (Users user1)) (All_of (Users user2)))";
      [%expect {|
   no:
   no: user1
   no: user2
   no: user3
  yes: user1 user2
  yes: user1 user2 user3
    |}];

      check "(At_least 0 of_ (Users))";
      [%expect {|
      (.fe.sexp:0:0 "[At_least] must get positive int")
    |}];

      check "(At_least 1 of_ (Users))";
      [%expect {|
      (.fe.sexp:0:0 "unsatisfiable [At_least]")
    |}];

      check "(At_least 1 of_ (Users user1))";
      [%expect {|
   no:
  yes: user1
   no: user2
   no: user3
  yes: user1 user2
  yes: user1 user2 user3
    |}];

      check "(At_least 2 of_ (Users user1))";
      [%expect {|
      (.fe.sexp:0:0 "unsatisfiable [At_least]")
    |}];

      check "(At_least 1 of_ (Users user1 user2))";
      [%expect {|
   no:
  yes: user1
  yes: user2
   no: user3
  yes: user1 user2
  yes: user1 user2 user3
    |}];

      check "(At_least 2 of_ (Users user1 user2))";
      [%expect {|
   no:
   no: user1
   no: user2
   no: user3
  yes: user1 user2
  yes: user1 user2 user3
    |}];

      check "(At_least 2 of_ (Users user1 user2 user3))";
      [%expect {|
   no:
   no: user1
   no: user2
   no: user3
  yes: user1 user2
  yes: user1 user2 user3
    |}];

      check "(At_least 3 of_ (Users user1 user2 user3))";
      [%expect {|
   no:
   no: user1
   no: user2
   no: user3
   no: user1 user2
  yes: user1 user2 user3
    |}];

      check "(At_least 2 of_ (Group users123))";
      [%expect {|
   no:
   no: user1
   no: user2
   no: user3
  yes: user1 user2
  yes: user1 user2 user3
    |}];

      check "(At_least 3 of_ (Group users123))";
      [%expect {|
   no:
   no: user1
   no: user2
   no: user3
   no: user1 user2
  yes: user1 user2 user3
    |}];
  end)
;;
