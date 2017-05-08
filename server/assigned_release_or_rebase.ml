open! Core
open! Import

type t =
  { assignee : User_name.t
  ; assigned : Next_step.t list
  }
[@@deriving sexp_of]

module Make (Feature : sig

    type t

    val continuous_release_status : t -> Continuous_release_status.t
    val first_owner               : t -> User_name.t
    val next_steps                : t -> Next_step.t list
    val owners                    : t -> User_name.t list
    val release_process           : t -> Release_process.t
    val who_can_release_into_me   : t -> Who_can_release_into_me.t

  end) = struct

  let create feature ~parent =
    let next_steps = Feature.next_steps feature in
    let has_next_step_release = List.mem next_steps Release ~equal:Next_step.equal in
    if not has_next_step_release
    then None
    else (
      let feature_first_owner = Feature.first_owner feature in
      match parent with
      | None -> Some { assignee = feature_first_owner; assigned = [ Release ] }
      | Some parent ->
        let parent_release_process = Feature.release_process parent in
        let should_wait_for_continuous_release =
          match parent_release_process, Feature.continuous_release_status feature with
          | Continuous, `Pending_or_working_on_it -> true
          | Direct, _ | _, `Not_working_on_it -> false
        in
        if should_wait_for_continuous_release
        then None
        else (
          let owner_assignee =
            match Feature.who_can_release_into_me parent with
            | My_owners_and_child_owners -> Some feature_first_owner
            | My_owners ->
              let child_owners = Feature.owners feature |> User_name.Set.of_list in
              List.find (Feature.owners parent) ~f:(Set.mem child_owners)
          in
          let must_call_fe_rebase_separately =
            match next_steps, parent_release_process with
            | Rebase :: _, Direct -> true
            | _, _ -> false
          in
          let assignee, (assigned : Next_step.t list) =
            match owner_assignee, must_call_fe_rebase_separately with
            | Some assignee, true  -> assignee                  , [ Rebase; Release ]
            | Some assignee, false -> assignee                  , [ Release         ]
            | None         , true  -> feature_first_owner       , [ Rebase          ]
            | None         , false -> Feature.first_owner parent, [ Release         ]
          in
          Some { assignee; assigned }))
  ;;
end

include Make (Feature)

let%test_module "" =
  (module struct

    module Feature = struct
      type t =
        { continuous_release_status : Continuous_release_status.t
        ; next_steps                : Next_step.t sexp_list
        ; owners                    : User_name.t list
        ; release_process           : Release_process.t
        ; who_can_release_into_me   : Who_can_release_into_me.t
        }
      [@@deriving fields]

      let first_owner t = List.hd_exn t.owners
    end

    include Make (Feature)

    let both_owner    = User_name.of_string "both"
    let feature_owner = User_name.of_string "child"
    let parent_owner  = User_name.of_string "parent"

    let features =
      let open List.Let_syntax in
      let%map continuous_release_status =
        [ `Not_working_on_it; `Pending_or_working_on_it ]
      and next_steps =
        [ [ Next_step.Rebase; Release ]
        ; [ Release ]
        ]
      and owners =
        [ [ feature_owner ]
        ; [ feature_owner ; both_owner ]
        ]
      in
      { Feature.
        continuous_release_status
      ; next_steps
      ; owners
      ; release_process = Direct
      ; who_can_release_into_me = My_owners
      }
    ;;

    let parents =
      let open List.Let_syntax in
      let%map release_process = Release_process.all
      and who_can_release_into_me = Who_can_release_into_me.all
      in
      { Feature.
        continuous_release_status = `Not_working_on_it
      ; next_steps = []
      ; owners = [ parent_owner; both_owner ]
      ; release_process
      ; who_can_release_into_me
      }
    ;;

    module Input = struct
      type t =
        { process    : string
        ; status     : string
        ; who_can    : string
        ; is_rebased : bool
        ; co_owned   : bool
        }
      [@@deriving compare]
    end

    module Line = struct
      type nonrec t =
        { input  : Input.t
        ; result : t option
        }

      let columns =
        let open Ascii_table in
        let status =
          Column.of_to_string ~header:"status" Fn.id
            (Column.cell (fun t -> t.input.status))
        and who_can =
          Column.of_to_string ~header:"who can" Fn.id
            (Column.cell (fun t -> t.input.who_can))
        and process =
          Column.of_to_string ~header:"process" Fn.id
            (Column.cell (fun t -> t.input.process))
        and is_rebased =
          Column.of_to_string ~header:"rebased" Fn.id
            (Column.cell (fun t ->
               if t.input.is_rebased
               then "rebased"
               else "no"))
        and co_owned =
          Column.of_to_string ~header:"co-owned" Fn.id
            (Column.cell (fun t ->
               if t.input.co_owned
               then "co-owned"
               else "no"))
        and assignee =
          Column.of_to_string ~header:"assignee" Fn.id
            (Column.cell (fun t ->
               match t.result with
               | None -> ""
               | Some t -> User_name.to_string t.assignee))
        and assigned =
          Column.of_to_string ~header:"assigned" Fn.id
            (Column.cell (fun t ->
               match t.result with
               | None -> ""
               | Some t ->
                 List.map t.assigned ~f:Next_step.to_string_hum
                 |> String.concat ~sep:", "))
        in
        [ process
        ; status
        ; who_can
        ; is_rebased
        ; co_owned
        ; assignee
        ; assigned
        ]
      ;;

      let create (feature : Feature.t) ~(parent : Feature.t option) =
        let who_can =
          match parent with
          | None -> "child"
          | Some parent ->
            match parent.who_can_release_into_me with
            | My_owners -> "parent"
            | My_owners_and_child_owners -> "all"
        and process =
          match parent with
          | None -> "direct"
          | Some parent ->
            match parent.release_process with
            | Direct -> "direct"
            | Continuous -> "continuous"
        and is_rebased =
          match feature.next_steps with
          | Rebase :: _ -> false
          | _ -> true
        and co_owned =
          match parent with
          | None -> false
          | Some parent ->
            List.find feature.owners
              ~f:(fun t -> List.mem ~equal:User_name.equal parent.owners t)
            |> Option.is_some
        and status =
          match feature.continuous_release_status with
          | `Pending_or_working_on_it -> "busy"
          | `Not_working_on_it -> "idle"
        in
        { result = create feature ~parent
        ; input =
            { status
            ; who_can
            ; process
            ; is_rebased
            ; co_owned
            }
        }
      ;;
    end

    let%expect_test _ =
      let rows =
        List.concat_map features ~f:(fun feature ->
          Line.create feature ~parent:None
          :: List.map parents ~f:(fun parent ->
            Line.create feature ~parent:(Some parent)))
        |> List.sort ~cmp:(fun t1 t2 -> Input.compare t1.Line.input t2.input)
      in
      let table = Ascii_table.create ~columns:Line.columns ~rows in
      print_endline (Ascii_table.to_string table
                       ~display_ascii:true ~max_output_columns:500);
      [%expect {|
        |---------------------------------------------------------------------------------|
        | process    | status | who can | rebased | co-owned | assignee | assigned        |
        |------------+--------+---------+---------+----------+----------+-----------------|
        | continuous | busy   | all     | no      | no       |          |                 |
        | continuous | busy   | all     | no      | co-owned |          |                 |
        | continuous | busy   | all     | rebased | no       |          |                 |
        | continuous | busy   | all     | rebased | co-owned |          |                 |
        | continuous | busy   | parent  | no      | no       |          |                 |
        | continuous | busy   | parent  | no      | co-owned |          |                 |
        | continuous | busy   | parent  | rebased | no       |          |                 |
        | continuous | busy   | parent  | rebased | co-owned |          |                 |
        | continuous | idle   | all     | no      | no       | child    | release         |
        | continuous | idle   | all     | no      | co-owned | child    | release         |
        | continuous | idle   | all     | rebased | no       | child    | release         |
        | continuous | idle   | all     | rebased | co-owned | child    | release         |
        | continuous | idle   | parent  | no      | no       | parent   | release         |
        | continuous | idle   | parent  | no      | co-owned | both     | release         |
        | continuous | idle   | parent  | rebased | no       | parent   | release         |
        | continuous | idle   | parent  | rebased | co-owned | both     | release         |
        | direct     | busy   | all     | no      | no       | child    | rebase, release |
        | direct     | busy   | all     | no      | co-owned | child    | rebase, release |
        | direct     | busy   | all     | rebased | no       | child    | release         |
        | direct     | busy   | all     | rebased | co-owned | child    | release         |
        | direct     | busy   | child   | no      | no       | child    | release         |
        | direct     | busy   | child   | no      | no       | child    | release         |
        | direct     | busy   | child   | rebased | no       | child    | release         |
        | direct     | busy   | child   | rebased | no       | child    | release         |
        | direct     | busy   | parent  | no      | no       | child    | rebase          |
        | direct     | busy   | parent  | no      | co-owned | both     | rebase, release |
        | direct     | busy   | parent  | rebased | no       | parent   | release         |
        | direct     | busy   | parent  | rebased | co-owned | both     | release         |
        | direct     | idle   | all     | no      | no       | child    | rebase, release |
        | direct     | idle   | all     | no      | co-owned | child    | rebase, release |
        | direct     | idle   | all     | rebased | no       | child    | release         |
        | direct     | idle   | all     | rebased | co-owned | child    | release         |
        | direct     | idle   | child   | no      | no       | child    | release         |
        | direct     | idle   | child   | no      | no       | child    | release         |
        | direct     | idle   | child   | rebased | no       | child    | release         |
        | direct     | idle   | child   | rebased | no       | child    | release         |
        | direct     | idle   | parent  | no      | no       | child    | rebase          |
        | direct     | idle   | parent  | no      | co-owned | both     | rebase, release |
        | direct     | idle   | parent  | rebased | no       | parent   | release         |
        | direct     | idle   | parent  | rebased | co-owned | both     | release         |
        |---------------------------------------------------------------------------------| |}]
    ;;
  end)
;;
