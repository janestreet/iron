open! Core
open! Async
open! Import

let print_item ~max_size index (name, _) =
  let index = Int.to_string_hum (succ index) in
  let left_alignment_margin =
    let index_len = String.length index in
    String.make (max 0 (max_size - index_len)) ' '
  in
  printf " %s[%s] %s\n" left_alignment_margin index name
;;

let select ~to_string ~available ~configuration =
  let available = Array.of_list available in
  let menu = Array.map available ~f:(fun ((a, _) as pair) -> to_string a, pair) in
  let length = Array.length menu in
  let index a =
    let str = to_string a in
    Option.map ~f:fst (Array.findi menu ~f:(fun _ (str', _) -> String.equal str str'))
  in
  let default =
    match List.filter_map configuration ~f:index with
    | [] -> List.init length ~f:Fn.id
    | (_::_) as indexes -> indexes
  in
  List.map default ~f:(fun i -> snd menu.(i))
;;

let read_index s =
  Option.try_with (fun () -> pred (Int.of_string s))
;;

let read_index_range s =
  let open Option.Let_syntax in
  let%bind (left, right) = String.lsplit2 s ~on:'-' in
  let%bind left = read_index left in
  let%bind right = read_index right in
  Option.some_if (left <= right) (List.init (right - left + 1) ~f:(fun i -> i + left))
;;

let read_indexes s =
  match read_index_range s with
  | Some _ as indexes -> indexes
  | None ->
    match read_index s with
    | Some index -> Some [index]
    | None -> None
;;

let parse_input input =
  let errorf fmt = ksprintf (fun s -> Error s) fmt in
  let indexes =
    let potential_indexes =
      String.split ~on:' ' input
      |> List.filter ~f:(Fn.non String.is_empty)
      |> List.map ~f:read_indexes
    in
    if List.for_all potential_indexes ~f:Option.is_some
    then
      List.map potential_indexes ~f:(fun o -> Option.value_exn o)
      |> List.concat
      |> function
      | [] -> None
      | _::_ as indexes ->
        Some (Int.Hash_set.of_list indexes)
    else None
  in
  match indexes with
  | Some indexes -> Ok (`Toggle_indexes indexes)
  | None ->
    let regex input =
      match Regex.create input with
      | Ok regex -> Ok (`Toggle_regex regex)
      | Error _ -> errorf "Invalid regex: %S" input
    in
    String.chop_prefix ~prefix:"re:" input
    |> Option.value ~default:input
    |> regex
;;

let%test_unit _ =
  let check is_match (index, file) input =
    let result =
      match parse_input input with
      | Error str ->
        raise_s [%sexp "parse error", { input : string; str : string}]
      | Ok res ->
        match res with
        | `Toggle_indexes hset -> Hash_set.mem hset (pred index)
        | `Toggle_regex regex  ->
          Regex.matches regex file
    in
    if not (Bool.equal is_match result)
    then raise_s [%sexp "invalid result"
                      , { index : int
                        ; file  : string
                        ; input : string
                        }]
  in
  check true  (1, "path/to/file") "file";
  check true  (1, "path/to/file") "1";
  check true  (2, "path_to_file") "1-3";
  check true  (2, "path_to_file") "1 2 3";
  check false (1, "path/to/file") "dir";
  check false (2, "path/to/file") "3";
  check false (2, "path/to/file") "1 3-4";
  check false (2, "path/to/file") "2 foobar"
;;

let navigate ~to_string ~available ~configuration direction =
  let menu = Array.of_list available in
  let length = Array.length menu in
  if length = 0
  then None
  else Some (
    let by_name a b = String.compare (to_string a) (to_string b) in
    let current =
      match direction with
      | `Pred_view -> List.min_elt configuration ~cmp:by_name
      | `Succ_view -> List.max_elt configuration ~cmp:by_name
    in
    let index =
      Option.bind current ~f:(fun value ->
        Option.map ~f:fst
          (Array.findi menu ~f:(fun _ alpha -> by_name value alpha = 0)))
    in
    let next_index =
      match index with
      | None -> 0
      | Some index ->
        let delta = match direction with `Pred_view -> -1 | `Succ_view -> 1 in
        Int.((index + delta) % length)
    in
    menu.(next_index))
;;

let toggle ~menu_name ~to_string ~display_prefix_in_list ~available ~configuration =
  let to_string =
    match display_prefix_in_list with
    | None -> to_string
    | Some prefix ->
      let max_prefix_size =
        List.fold available ~init:0
          ~f:(fun acc elt -> max acc (String.length (prefix elt)))
      in
      (fun elt -> sprintf "%*s%s%s" max_prefix_size (prefix elt)
                    (if max_prefix_size > 0 then " " else "") (to_string elt))
  in
  let available = Array.of_list available in
  let menu = Array.map available ~f:(fun a-> to_string a, a) in
  let length = Array.length menu in
  let index a =
    let str = to_string a in
    Option.map ~f:fst (Array.findi menu ~f:(fun _ (str', _) -> String.equal str str'))
  in
  let selection = Queue.of_list (List.filter_map configuration ~f:index) in
  let rec loop ?(help=false) ?message () =
    printf "%s:\n" menu_name;
    let max_size =
      menu
      |> Array.length
      |> Int.to_string_hum
      |> String.length
    in
    Array.iteri menu ~f:(print_item ~max_size);
    printf "\n";
    printf "Current selection to show:\n";
    if Queue.is_empty selection
    then printf "<empty-selection>\n"
    else Queue.iter selection ~f:(fun index -> print_item ~max_size index menu.(index));
    printf "\n";
    let f input =
      match String.lowercase input with
      | "?"          -> Ok `Help
      | "q" | "quit" -> Ok `Quit
      | "a"          -> Ok `Show_again
      | "all"        -> Ok `All
      | "none"       -> Ok `None
      | ""           -> Ok (if Queue.is_empty selection then `Empty else `Show_again)
      | _            -> parse_input input
    in
    (if help
     then
       printf "\
Use the following command to toggle the item selected
?              : Show this help
a|<enter>      : Show the diff again
all            : Select all the available items
none           : Select none of the available items
([0-9]+ )+     : Toggle some items based on their index. Ranges: %%d-%%d
(re:)?<rgx>    : Toggle items matching the regexp provided
q|quit         : Quit
");
    Option.iter message ~f:(printf "<!> %s\n");
    match%bind
      Async_interactive.ask_dispatch_gen ~f "Toggle item selection ?/all/none/[0-9]+"
    with
    | `Help -> loop ~help:true ()
    | `Quit -> return `Quit
    | `Empty ->
      let module Choice = Review_util.Choice in
      (match%bind
         Async_interactive.ask_dispatch_with_help
           "No item selected. "
           [ Async_interactive.Choice.default Choice.Mode.selected_files
           ; Choice.Mode.file_by_file
           ; Choice.Mode.global_diff
           ; Choice.quit
           ]
       with
       | `Selected_files -> loop ()
       | (`File_by_file | `Global_diff | `Quit) as other -> return other)
    | `Show_again ->
      let selection =
        List.map (Queue.to_list selection) ~f:(fun i -> available.(i))
      in
      if List.is_empty selection
      then loop ~message:"current selection is empty" ()
      else return (`New_configuration selection)
    | `All ->
      let present =
        Int.Hash_set.of_list (Queue.to_list selection)
      in
      for key = 0 to length - 1 do
        if not (Hash_set.mem present key) then Queue.enqueue selection key
      done;
      loop ()
    | `None ->
      Queue.clear selection;
      loop ()
    | (`Toggle_indexes _ | `Toggle_regex _) as toggle ->
      let toggle =
        match toggle with
        | `Toggle_indexes indexes -> Hash_set.mem indexes
        | `Toggle_regex regex ->
          (fun index -> Regex.matches regex (fst menu.(index)))
      in
      let present =
        Int.Hash_set.of_list (Queue.to_list selection)
      in
      let hit = ref false in
      for key = 0 to length - 1 do
        if toggle key
        then (
          hit := true;
          if Hash_set.mem present key
          then Queue.filter_inplace selection ~f:(fun index -> not (Int.equal index key))
          else Queue.enqueue selection key)
      done;
      let message = Option.some_if (not !hit) "no match found" in
      loop ?message ()
  in
  loop ()
;;
