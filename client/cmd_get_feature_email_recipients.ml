open! Core
open! Async
open! Import

let command =
  Command.async' ~summary:"output recipients of feature emails"
    (let open Command.Let_syntax in
     let%map_open feature_path = feature_path_or_current_bookmark
     and sent_upon =
       enum_required "sent-upon" (module Get_feature_email_recipients.Sent_upon)
         ~doc:""
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%map { recipients } =
         Get_feature_email_recipients.rpc_to_server_exn { feature_path; sent_upon }
       in
       Set.iter recipients ~f:(Print.printf !"%{Email_address}\n"))
;;
