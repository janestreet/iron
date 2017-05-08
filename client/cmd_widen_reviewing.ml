open! Core
open! Async
open! Import

(* This code is based on the following principles:

   - it should increase the size of the reviewing set or fail
   - it should not add a non w-f-r if the feature isn't seconded
   - it should prefer the first owner to other w-f-r's, and prefer the other
   w-f-r's to non w-f-r's.
   - it should, when possible, express the reviewing set using
   [`Whole_feature_reviewers] or [`All].
*)

let command =
  Command.async'
    ~summary:"grow the set of users currently reviewing"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind feature =
         Get_feature.rpc_to_server_exn { feature_path; rev_zero = None }
       in
       if not feature.review_is_enabled
       then failwith "review is not enabled; must first run [fe enable-review]";
       let whole_feature_reviewers = feature.whole_feature_reviewers in
       let add users = Reviewing.add feature.reviewing users ~whole_feature_reviewers in
       let first_owner = List.hd_exn feature.owners in
       let reviewing =
         if not (Feature.user_is_currently_reviewing feature first_owner)
         then add (User_name.Set.singleton first_owner)
         else (
           let forbidden_whole_feature_reviewers =
             match feature.reviewing with
             | `All -> failwith "cannot widen review; everyone is already reviewing"
             | `Whole_feature_reviewers -> User_name.Set.empty
             | `Only permitted    -> Set.diff  whole_feature_reviewers permitted
             | `All_but forbidden -> Set.inter whole_feature_reviewers forbidden
           in
           if not (Set.is_empty forbidden_whole_feature_reviewers)
           then add forbidden_whole_feature_reviewers
           else if is_none feature.seconder
           then failwith "feature is not seconded; a seconder must run [fe second]"
           else `All)
       in
       if Reviewing.equal feature.reviewing reviewing
       then raise_s [%sexp "report Iron bug in widen-reviewing", (feature : Feature.t)];
       Cmd_change.change_feature ~feature_path ~updates:[ `Set_reviewing reviewing ] ())
;;
