open! Core
open! Async
open! Import

module Key  = struct
  include Get_rpc_stats.Key
  include Hashable.Make_plain (Get_rpc_stats.Key)
end

module Data = struct
  include Get_rpc_stats.Data

  let add t1 t2 =
    { took = Time.Span.(t1.took + t2.took)
    ; hits = t1.hits + t2.hits
    }
  ;;
end

module Merge = struct
  module Arg = struct
    type t =
      | By
      | Rpc
      | Version
    [@@deriving compare, enumerate, sexp_of]
  end

  type t =
    { by          : bool
    ; rpc_name    : bool
    ; rpc_version : bool
    }
  [@@deriving fields, sexp_of]

  let of_args (args : Arg.t list) =
    let by = ref false in
    let rpc_name = ref false in
    let rpc_version = ref false in
    List.iter args ~f:(function
      | By          -> by := true
      | Rpc         -> rpc_name    := true
      | Version     -> rpc_version := true);
    { by          = !by
    ; rpc_name    = !rpc_name
    ; rpc_version = !rpc_version
    }
  ;;

  let not_shown =
    { Key.
      by          = User_name.of_string "users"
    ; rpc_name    = "rpcs"
    ; rpc_version = 0
    }
  ;;

  let representant t { Key. by; rpc_name; rpc_version } =
    { Key.
      by          = if t.by          then not_shown.by          else by
    ; rpc_name    = if t.rpc_name    then not_shown.rpc_name    else rpc_name
    ; rpc_version = if t.rpc_version then not_shown.rpc_version else rpc_version
    }
  ;;
end

let command =
  Command.async' ~summary:"output the server's RPC statistics"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     and sort_by_hits =
       no_arg_flag "-sort-by-hits" ~doc:" sort by decreasing number of hits"
     and aggregate_columns =
       map ~f:Merge.of_args
         (enum_list "-merge" ~doc:"COLUMN[,COLUMN...] aggregate columns by|rpc|version"
            (module Merge.Arg))
     and only_users =
       users_option ~switch:"-only-users"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map stats = Get_rpc_stats.rpc_to_server_exn () in
       let data_cell field to_string =
         Ascii_table.Column.string ~header:(Field.name field)
           (Ascii_table.Column.cell (fun (_, d) -> to_string (Field.get field d)))
       in
       let key_cell merge_field field to_string =
         if Field.get merge_field aggregate_columns
         then None
         else
           Some
             (Ascii_table.Column.string ~header:(Field.name field)
                (Ascii_table.Column.cell (fun (k, _) -> to_string (Field.get field k))))
       in
       let stats =
         let stats_table = Key.Table.create () in
         let keep_user =
           match only_users with
           | Some set -> (fun user -> Set.mem set user)
           | None -> const true
         in
         List.iter stats ~f:(fun (key, data) ->
           if keep_user key.by
           then
             Hashtbl.update stats_table (Merge.representant aggregate_columns key)
               ~f:(function
                 | None -> data
                 | Some data' -> Data.add data data'));
         Hashtbl.to_alist stats_table
       in
       let rows =
         let cmp : Data.t -> Data.t -> int =
           if sort_by_hits
           then (fun d1 d2 -> Int.compare       d2.hits d1.hits)
           else (fun d1 d2 -> Time.Span.compare d2.took d1.took)
         in
         List.sort stats ~cmp:(fun (_, d1) (_, d2) -> cmp d1 d2)
       in
       let columns =
         let took = data_cell Data.Fields.took Time.Span.to_short_string in
         let hits = data_cell Data.Fields.hits Int.to_string_hum in
         (if sort_by_hits then [ hits ; took ] else [ took ; hits ])
         @
         List.filter_opt
           [ key_cell Merge.Fields.by          Key.Fields.by          User_name.to_string
           ; key_cell Merge.Fields.rpc_name    Key.Fields.rpc_name    Fn.id
           ; key_cell Merge.Fields.rpc_version Key.Fields.rpc_version Int.to_string
           ]
       in
       Ascii_table.create ~rows ~columns
       |> Ascii_table.to_string ~display_ascii ~max_output_columns
       |> print_endline)
;;
