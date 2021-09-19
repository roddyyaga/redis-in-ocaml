open Async

let handle fd =
  let ic = Reader.create fd in
  let writev = Faraday_async.writev_of_fd fd in
  let output_buffer = Faraday.create 16 in
  let write_resp resp =
    Faraday_async.serialize output_buffer
      ~yield:(fun buffer -> Resp.faraday buffer resp |> return)
      ~writev
  in
  Angstrom_async.parse_many Resp_angstorm.parse
    (fun parsed -> parsed |> Commands.handle |> write_resp)
    ic
  >>| Result.get_ok

let create_server port =
  Tcp.Server.create_sock_inet ~on_handler_error:`Raise
    (Tcp.Where_to_listen.of_port port) (fun _address socket ->
      handle (Socket.fd socket))

let main () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Debug);
  ignore @@ create_server 6379;
  ignore @@ Scheduler.go ()
