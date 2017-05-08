module Stable = struct
  open! Core.Core_stable
  module V1 = struct
    type 'a t =
      { b1 : 'a
      ; f1 : 'a
      ; b2 : 'a
      ; f2 : 'a
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: Bin_digest_type_variable.tick_a t];
      [%expect {| 27104f0a1b68f74d209c433e0a826112 |}]
    ;;

    let map t ~f =
      let f field = f (Core.Field.get field t) in
      Fields.map
        ~b1:f
        ~b2:f
        ~f1:f
        ~f2:f
    ;;
  end
end

open! Core
open! Async

include Stable.V1

let invariant invariant t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~b1:(check invariant)
      ~b2:(check invariant)
      ~f1:(check invariant)
      ~f2:(check invariant)
  )
;;

let singleton a = { b1 = a ; b2 = a ; f1 = a ; f2 = a }
;;

include Applicative.Make (struct
    type nonrec 'a t = 'a t
    let return = singleton
    let apply f y =
      { b1 = f.b1 y.b1
      ; b2 = f.b2 y.b2
      ; f1 = f.f1 y.f1
      ; f2 = f.f2 y.f2
      }
    let map = `Custom map
  end)

include Container.Make (struct
    type nonrec 'a t = 'a t

    let fold t ~init ~f =
      let f acc field = f acc (Field.get field t) in
      Fields.fold ~init
        ~b1:f
        ~b2:f
        ~f1:f
        ~f2:f
    ;;

    let iter = `Custom (fun t ~f ->
      let f field = f (Field.get field t) in
      Fields.iter
        ~b1:f
        ~b2:f
        ~f1:f
        ~f2:f
    )
    ;;
  end)

(* custom implementation of some functions with less allocation *)

let map2 a b ~f =
  { b1 = f a.b1 b.b1
  ; b2 = f a.b2 b.b2
  ; f1 = f a.f1 b.f1
  ; f2 = f a.f2 b.f2
  }
;;

let map3 a b c ~f =
  { b1 = f a.b1 b.b1 c.b1
  ; b2 = f a.b2 b.b2 c.b2
  ; f1 = f a.f1 b.f1 c.f1
  ; f2 = f a.f2 b.f2 c.f2
  }
;;

let for_all t ~f =
  f t.b1
  && f t.b2
  && f t.f1
  && f t.f2
;;

let for_all2 a b ~f =
  f a.b1 b.b1
  && f a.b2 b.b2
  && f a.f1 b.f1
  && f a.f2 b.f2
;;

let old_base = b1
let new_base = b2
let old_tip  = f1
let new_tip  = f2

let to_list { b1; b2; f1; f2 } = [ b1; b2; f1; f2 ]

let edges { b1; b2; f1; f2 } =
  [ (b1, b2); (b1, f1); (b2, f2); (f1, f2) ]
;;

let of_one_edge a b =
  { b1 = a
  ; b2 = a
  ; f1 = a
  ; f2 = b
  }
;;

module Node = struct
  type t =
    [ `old_base
    | `new_base
    | `old_tip
    | `new_tip
    ]
  [@@deriving compare, sexp_of, enumerate]

  let to_string_short = function
    | `old_base -> "B1"
    | `new_base -> "B2"
    | `old_tip  -> "F1"
    | `new_tip  -> "F2"
  ;;
end

let get t = function
  | `old_base -> t.b1
  | `new_base -> t.b2
  | `old_tip  -> t.f1
  | `new_tip  -> t.f2
;;

let get' t = function
  | `b1 -> t.b1
  | `b2 -> t.b2
  | `f1 -> t.f1
  | `f2 -> t.f2
;;

let self = { b1 = `old_base ; b2 = `new_base ; f1 = `old_tip ; f2 = `new_tip }
;;

let%test _ =
  List.for_all Node.all
    ~f:(fun dispatch -> get self dispatch = dispatch)
;;

let init ~f = map self ~f
;;

module Deferred = struct
  open Async

  let map ?(how = `Sequential) { b1; b2; f1; f2 } ~f =
    match how with
    | `Sequential ->
      f b1 >>= fun b1 ->
      f b2 >>= fun b2 ->
      f f1 >>= fun f1 ->
      f f2 >>= fun f2 ->
      return { b1; b2; f1; f2 }
    | `Parallel ->
      Deferred.both
        (Deferred.both (f b1) (f b2))
        (Deferred.both (f f1) (f f2))
      >>| fun ((b1, b2), (f1, f2)) ->
      { b1; b2; f1; f2 }
  ;;

  let map2 ?how t1 t2 ~f =
    map ?how (both t1 t2) ~f:(fun (x, y) -> f x y)
  ;;

  let all { b1 ; b2 ; f1 ; f2 } =
    b1 >>= fun b1 ->
    b2 >>= fun b2 ->
    f1 >>= fun f1 ->
    f2 >>| fun f2 ->
    { b1 ; b2 ; f1 ; f2 }
  ;;
end

let param xflag ~doc =
  let open Command.Let_syntax in
  let%map_open () = return ()
  and b1 = flag "-old-base" xflag ~doc:(doc "old-base") ~aliases:["-b1"]
  and f1 = flag "-old-tip"  xflag ~doc:(doc "old-tip" ) ~aliases:["-f1" ; "old-feature"]
  and b2 = flag "-new-base" xflag ~doc:(doc "new-base") ~aliases:["-b2"]
  and f2 = flag "-new-tip"  xflag ~doc:(doc "new-tip" ) ~aliases:["-f2" ; "new-feature"]
  in
  { b1 ; f1 ; b2 ; f2 }
;;

let anon xflag =
  let open Command.Param in
  anon (t4 (xflag `old_base) (xflag `old_tip) (xflag `new_base) (xflag `new_tip))
  |> map ~f:(fun (b1, f1, b2, f2) -> { b1 ; f1 ; b2 ; f2 })
;;

let group diamond ~by:diff4_class =
  let groups = Diff4_class.to_groups diff4_class in
  let result = init ~f:(fun _ -> ref []) in
  List.iter groups ~f:(fun group ->
    let contents = List.map group ~f:(get' diamond) in
    List.iter group ~f:(fun node ->
      (get' result node) := contents;
    )
  );
  map result ~f:(fun t -> !t)
;;

let%test_unit _ =
  List.iter Diff4_class.all ~f:(fun diff4_class ->
    let groups = Diff4_class.to_groups diff4_class in
    let diamond = init ~f:(fun _ -> ref false) in
    List.iter groups ~f:(fun group ->
      List.iter group ~f:(fun node ->
        let r = get' diamond node in
        if !r then raise_s [%sexp "node already visited", (diff4_class : Diff4_class.t)];
        r := true
      ));
    if exists diamond ~f:(fun r -> not !r)
    then
      raise_s [%sexp "unvisited node", (diff4_class : Diff4_class.t)])
;;

let classify ~equal { b1 ; b2 ; f1 ; f2 } =
  Diff4_class.classify ~equal ~b1 ~b2 ~f1 ~f2
;;

let is_forget ~equal diamond = Diff4_class.is_forget (classify ~equal diamond)

let pretty_short_description ~label { b1 ; b2 ; f1 ; f2 } =
  let label s =
    if String.is_empty s
    then label
    else if String.is_empty label
    then s
    else sprintf "%s %s" s label
  in
  let same_bs = String.equal b1 b2 in
  let same_fs = String.equal f1 f2 in
  if same_fs && String.equal b2 f1
  then
    if same_bs
    then (* All are equal *)
      [ label "", b1 ]
    else (* This is a forget *)
      [ label "old base" , b1
      ; label "old tip"  , f1
      ]
  else (
    match same_bs, same_fs with
    | true, true ->
      if String.equal b1 f1
      then
        [ label "", b1 ]
      else
        [ label "base" , b1
        ; label "tip"  , f1
        ]
    | true, false ->
      if String.equal b1 f1
      then
        [ label "base"   , b1
        ; label "tip"    , f2
        ]
      else
        [ label "base"    , b1
        ; label "old tip" , f1
        ; label "new tip" , f2
        ]
    | false, true ->
      [ label "old base"       , b1
      ; label "new base"       , b2
      ; label "old & new tip"  , f2
      ]
    | false, false ->
      [ label "old base"  , b1
      ; label "old tip"   , f1
      ; label "new base"  , b2
      ; label "new tip"   , f2
      ])
;;

let pretty_short_rev_names ~equal { b1 ; b2 ; f1 ; f2 } =
  let same_bs = equal b1 b2 in
  let same_fs = equal f1 f2 in
  if same_fs && equal b2 f1
  then
    if same_bs
    then (* All are equal *)
      init ~f:(const "base")
    else (* This is a forget *)
      { b1 = "old base"
      ; b2 = "old tip"
      ; f1 = "old tip"
      ; f2 = "old tip"
      }
  else
    { b1 = if same_bs then "base" else "old base"
    ; b2 = if same_bs then "base" else "new base"
    ; f1 =
        if same_bs && equal b1 f1 then "base"
        else if same_fs then "tip" else "old tip"
    ; f2 =
        if same_bs && equal b1 f1 then "tip"
        else if same_fs then "tip" else "new tip"
    }
;;

let pretty_short_rev_names_const =
  { b1 = "old base"
  ; b2 = "new base"
  ; f1 = "old tip"
  ; f2 = "new tip"
  }
;;
