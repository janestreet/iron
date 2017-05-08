open Core

let char_value_for_alphabetic_compare =
  let num_chars = Char.(to_int max_value - to_int min_value) + 1 in
  let result = Array.create ~len:num_chars (-1) in
  let r = ref (-1) in
  let assign c =
    incr r;
    let i = Char.to_int c in
    assert (Int.equal result.(i) (-1));
    result.(i) <- !r;
  in
  String.iter ~f:assign
    "/.aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ0123456789";
  for i = Char.to_int Char.min_value to Char.to_int Char.max_value do
    if Int.equal result.(i) (-1) then assign (Char.of_int_exn i)
  done;
  [%test_result: int] !r ~expect:(num_chars - 1);
  result
;;

let alphabetic_compare t1 t2 =
  let n1 = String.length t1 in
  let n2 = String.length t2 in
  let min_length = min n1 n2 in
  let result = ref 0 in
  let r = ref 0 in
  while !r < min_length && !result = 0; do
    let c1 = t1.[!r] in
    let c2 = t2.[!r] in
    incr r;
    result :=
      Int.compare
        (char_value_for_alphabetic_compare.(Char.to_int c1))
        (char_value_for_alphabetic_compare.(Char.to_int c2))
  done;
  if !result <> 0
  then !result
  else Int.compare n1 n2
;;

let%test_unit _ =
  List.iter
    [ ""     , `equal, ""
    ; ""     , `less , "a"
    ; "a"    , `equal, "a"
    ; "a"    , `less , "aa"
    ; "a"    , `less , "b"
    ; "a"    , `less , "A"
    ; "A"    , `less , "b"
    ; "b"    , `less , "B"
    ; "b"    , `less , "ba"
    ; "B"    , `less , "z"
    ; "z"    , `less , "Z"
    ; "ab"   , `equal, "ab"
    ; "ab"   , `less , "aba"
    ; "foo/b", `less , "fooa"
    ]
    ~f:(fun (s1, relation, s2) ->
      let c = alphabetic_compare s1 s2 in
      let is_correct =
        match relation with
        | `less  -> c < 0
        | `equal -> c = 0
      in
      if not is_correct
      then raise_s
             [%sexp "bug"
                  , { s1       : string
                    ; relation : [ `less | `equal ]
                    ; s2       : string
                    ; c        : int
                    }])
;;

include String

let try_chop_suffix str ~suffix =
  Option.value (chop_suffix str ~suffix) ~default:str
;;
