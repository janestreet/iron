open! Core
open Import

let create inputs ?preserve_input_ordering feature_path make_output =
  let outputs = ref [] in
  let emit ~part input_option ~depth =
    let output =
      make_output ~feature:(String.concat [ String.make (2 * depth) ' '
                                          ; Feature_name.to_string part
                                          ])
        input_option
    in
    outputs := output :: !outputs
  in
  let inputs =
    let inputs = List.map inputs ~f:(fun input -> (feature_path input, input)) in
    if Option.is_some preserve_input_ordering
    then inputs
    else List.sort inputs ~cmp:(fun (f1, _) (f2, _) -> Feature_path.compare f1 f2)
  in
  let (_ : Feature_name.t list) =
    List.fold inputs ~init:[] ~f:(fun prior_parts (feature_path, input) ->
      let parts = Feature_path.parts feature_path in
      let rec show part parts ~depth =
        match parts with
        | [] -> emit ~part ~depth (Some input)
        | part2 :: parts ->
          emit ~part ~depth None;
          show part2 parts ~depth:(depth + 1);
      in
      let rec loop parts prior_parts ~depth =
        match parts with
        | [] -> ()
        | part :: parts ->
          match prior_parts with
          | [] -> show part parts ~depth
          | prior_part :: prior_parts ->
            if Feature_name.equal part prior_part && not (List.is_empty parts)
            then loop parts prior_parts ~depth:(depth + 1)
            else show part parts ~depth
      in
      loop parts prior_parts ~depth:0;
      parts)
  in
  List.rev !outputs
;;
