open! Core
open! Import

type t =
  { add_inheritable_attributes                      : Inheritable_attributes.t
  ; set_crs_shown_in_todo_only_for_users_reviewing  : bool option
  ; set_xcrs_shown_in_todo_only_for_users_reviewing : bool option
  ; add_owners                                      : User_name.t list
  ; add_properties                                  : Properties.t
  ; set_release_process                             : Release_process.t option
  ; set_who_can_release_into_me                     : Who_can_release_into_me.t option
  ; add_send_email_to                               : Email_address.Set.t
  ; add_send_email_upon                             : Send_email_upon.Set.t
  ; add_whole_feature_followers                     : User_name.Set.t
  ; add_whole_feature_reviewers                     : User_name.Set.t
  }
[@@deriving fields, sexp_of]

let empty =
  { add_inheritable_attributes                      = Inheritable_attributes.empty
  ; set_crs_shown_in_todo_only_for_users_reviewing  = None
  ; set_xcrs_shown_in_todo_only_for_users_reviewing = None
  ; add_owners                                      = []
  ; add_properties                                  = Property.Map.empty
  ; set_release_process                             = None
  ; set_who_can_release_into_me                     = None
  ; add_send_email_to                               = Email_address.Set.empty
  ; add_send_email_upon                             = Send_email_upon.Set.empty
  ; add_whole_feature_followers                     = User_name.Set.empty
  ; add_whole_feature_reviewers                     = User_name.Set.empty
  }
;;

let filter_owners owners ~remove_members_of =
  let set = User_name.Set.of_list remove_members_of in
  List.filter owners ~f:(fun user -> not (Set.mem set user))
;;

let enqueue_feature_updates_for_inheritable_attributes ~changes ~feature
      { Inheritable_attributes.crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; owners
      ; properties
      ; release_process
      ; send_email_to
      ; send_email_upon
      ; who_can_release_into_me
      ; whole_feature_followers
      ; whole_feature_reviewers
      } =
  let attrs = Feature.inheritable_attributes feature in
  let add change = Queue.enqueue changes change in
  if Option.is_some crs_shown_in_todo_only_for_users_reviewing
  then add (`Set_inheritable_crs_shown_in_todo_only_for_users_reviewing
              crs_shown_in_todo_only_for_users_reviewing);
  if Option.is_some xcrs_shown_in_todo_only_for_users_reviewing
  then add (`Set_inheritable_xcrs_shown_in_todo_only_for_users_reviewing
              xcrs_shown_in_todo_only_for_users_reviewing);
  let owners =
    filter_owners owners ~remove_members_of:(Feature_inheritable_attributes.owners attrs)
  in
  if not (List.is_empty owners)
  then add (`Add_inheritable_owners owners);
  if not (Map.is_empty properties)
  then add (`Set_inheritable_properties properties);
  if Option.is_some release_process
  then add (`Set_inheritable_release_process release_process);
  if Option.is_some who_can_release_into_me
  then add (`Set_inheritable_who_can_release_into_me who_can_release_into_me);
  let send_email_to =
    Set.diff send_email_to (Feature_inheritable_attributes.send_email_to attrs)
  in
  if not (Set.is_empty send_email_to)
  then add (`Add_inheritable_send_email_to send_email_to);
  let send_email_upon =
    Set.diff send_email_upon (Feature_inheritable_attributes.send_email_upon attrs)
  in
  if not (Set.is_empty send_email_upon)
  then add (`Add_inheritable_send_email_upon send_email_upon);
  let whole_feature_followers =
    Set.diff whole_feature_followers
      (Feature_inheritable_attributes.whole_feature_followers attrs)
  in
  if not (Set.is_empty whole_feature_followers)
  then add (`Add_inheritable_whole_feature_followers whole_feature_followers);
  let whole_feature_reviewers =
    Set.diff whole_feature_reviewers
      (Feature_inheritable_attributes.whole_feature_reviewers attrs)
  in
  if not (Set.is_empty whole_feature_reviewers)
  then add (`Add_inheritable_whole_feature_reviewers whole_feature_reviewers);
;;

let to_feature_updates feature
      { add_inheritable_attributes
      ; set_crs_shown_in_todo_only_for_users_reviewing
      ; set_xcrs_shown_in_todo_only_for_users_reviewing
      ; add_owners
      ; add_properties
      ; set_release_process
      ; set_who_can_release_into_me
      ; add_send_email_to
      ; add_send_email_upon
      ; add_whole_feature_followers
      ; add_whole_feature_reviewers
      } : Iron_protocol.Change_feature.Update.t list =
  let changes = Queue.create () in
  let add change = Queue.enqueue changes change in
  enqueue_feature_updates_for_inheritable_attributes ~changes ~feature
    add_inheritable_attributes;
  Option.iter set_crs_shown_in_todo_only_for_users_reviewing
    ~f:(fun crs_shown_in_todo_only_for_users_reviewing ->
      add (`Set_crs_shown_in_todo_only_for_users_reviewing
             crs_shown_in_todo_only_for_users_reviewing));
  Option.iter set_xcrs_shown_in_todo_only_for_users_reviewing
    ~f:(fun xcrs_shown_in_todo_only_for_users_reviewing ->
      add (`Set_xcrs_shown_in_todo_only_for_users_reviewing
             xcrs_shown_in_todo_only_for_users_reviewing));
  let owners = filter_owners add_owners ~remove_members_of:(Feature.owners feature) in
  if not (List.is_empty owners) then add (`Add_owners owners);
  if not (Map.is_empty add_properties)
  then add (`Set_properties add_properties);
  Option.iter set_release_process ~f:(fun release_process ->
    add (`Set_release_process release_process));
  Option.iter set_who_can_release_into_me
    ~f:(fun who_can_release_into_me
         -> add (`Set_who_can_release_into_me who_can_release_into_me));
  let send_email_to =
    Set.diff add_send_email_to (Feature.send_email_to feature)
  in
  if not (Set.is_empty send_email_to)
  then add (`Add_send_email_to send_email_to);
  let send_email_upon =
    Set.diff add_send_email_upon (Feature.send_email_upon feature)
  in
  if not (Set.is_empty send_email_upon)
  then add (`Add_send_email_upon send_email_upon);
  let whole_feature_followers =
    Set.diff add_whole_feature_followers (Feature.whole_feature_followers feature)
  in
  if not (Set.is_empty whole_feature_followers)
  then add (`Add_whole_feature_followers whole_feature_followers);
  let whole_feature_reviewers =
    Set.diff add_whole_feature_reviewers (Feature.whole_feature_reviewers feature)
  in
  if not (Set.is_empty whole_feature_reviewers)
  then add (`Add_whole_feature_reviewers whole_feature_reviewers);
  Queue.to_list changes
;;

let maybe_set_crs_shown_in_todo_only_for_users_reviewing
      t ~crs_shown_in_todo_only_for_users_reviewing =
  if Option.is_none crs_shown_in_todo_only_for_users_reviewing
  then t
  else { t with set_crs_shown_in_todo_only_for_users_reviewing
                = crs_shown_in_todo_only_for_users_reviewing }
;;

let maybe_set_xcrs_shown_in_todo_only_for_users_reviewing
      t ~xcrs_shown_in_todo_only_for_users_reviewing =
  if Option.is_none xcrs_shown_in_todo_only_for_users_reviewing
  then t
  else { t with set_xcrs_shown_in_todo_only_for_users_reviewing
                = xcrs_shown_in_todo_only_for_users_reviewing }
;;

let add_owners t ~owners =
  { t with
    add_owners = t.add_owners @ filter_owners owners ~remove_members_of:t.add_owners
  }
;;

let add_properties t ~properties =
  { t with
    add_properties =
      Map.fold properties ~init:t.add_properties ~f:(fun ~key ~data acc ->
        Map.add acc ~key ~data)
  }
;;

let maybe_set_release_process t ~release_process =
  if Option.is_none release_process
  then t
  else { t with set_release_process = release_process }
;;

let maybe_set_who_can_release_into_me t ~who_can_release_into_me =
  if Option.is_none who_can_release_into_me
  then t
  else { t with set_who_can_release_into_me = who_can_release_into_me }
;;

let add_send_email_to t ~send_email_to =
  { t with
    add_send_email_to =
      Set.union t.add_send_email_to send_email_to
  }
;;

let add_send_email_upon t ~send_email_upon =
  { t with
    add_send_email_upon =
      Set.union t.add_send_email_upon send_email_upon
  }
;;

let add_whole_feature_followers t ~whole_feature_followers =
  { t with
    add_whole_feature_followers =
      Set.union t.add_whole_feature_followers whole_feature_followers
  }
;;

let add_whole_feature_reviewers t ~whole_feature_reviewers =
  { t with
    add_whole_feature_reviewers =
      Set.union t.add_whole_feature_reviewers whole_feature_reviewers
  }
;;

let add_inheritable_attributes t
      ~inheritable_attributes:
      { Inheritable_attributes.crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; owners
      ; properties
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; whole_feature_followers
      ; whole_feature_reviewers
      }
  =
  let attrs = t.add_inheritable_attributes in
  { t with
    add_inheritable_attributes =
      { crs_shown_in_todo_only_for_users_reviewing =
          Option.first_some crs_shown_in_todo_only_for_users_reviewing
            attrs.crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing =
          Option.first_some xcrs_shown_in_todo_only_for_users_reviewing
            attrs.xcrs_shown_in_todo_only_for_users_reviewing
      ; owners = attrs.owners @ filter_owners owners ~remove_members_of:attrs.owners
      ; properties =
          Map.merge attrs.properties properties ~f:(fun ~key:_ -> function
            | `Left v | `Right v -> Some v
            | `Both (_old, new_) -> Some new_)
      ; release_process =
          Option.first_some release_process attrs.release_process
      ; send_email_to = Set.union send_email_to attrs.send_email_to
      ; send_email_upon = Set.union send_email_upon attrs.send_email_upon
      ; who_can_release_into_me =
          Option.first_some who_can_release_into_me attrs.who_can_release_into_me
      ; whole_feature_followers =
          Set.union whole_feature_followers attrs.whole_feature_followers
      ; whole_feature_reviewers =
          Set.union whole_feature_reviewers attrs.whole_feature_reviewers
      }
  }
;;

let add_inherited_from_parent t ~parent_inheritable_attributes =
  let { Inheritable_attributes.
        crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; owners
      ; properties
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; whole_feature_followers
      ; whole_feature_reviewers
      } as inheritable_attributes =
    Feature_inheritable_attributes.to_protocol parent_inheritable_attributes
  in
  t
  |> maybe_set_crs_shown_in_todo_only_for_users_reviewing
       ~crs_shown_in_todo_only_for_users_reviewing
  |> maybe_set_xcrs_shown_in_todo_only_for_users_reviewing
       ~xcrs_shown_in_todo_only_for_users_reviewing
  |> add_owners                        ~owners
  |> add_properties                    ~properties
  |> maybe_set_release_process         ~release_process
  |> maybe_set_who_can_release_into_me ~who_can_release_into_me
  |> add_send_email_to                 ~send_email_to
  |> add_send_email_upon               ~send_email_upon
  |> add_whole_feature_followers       ~whole_feature_followers
  |> add_whole_feature_reviewers       ~whole_feature_reviewers
  |> add_inheritable_attributes        ~inheritable_attributes
;;
