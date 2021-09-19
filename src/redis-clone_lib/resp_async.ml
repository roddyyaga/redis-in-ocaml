open Async

let handle (ic, oc) =
  Angstrom_async.parse_many Resp_angstorm.parse
    (fun parsed ->
      parsed |> Commands.handle |> Resp.encode |> Async_unix.Writer.write oc
      |> return)
    ic
  >>| Result.get_ok

let create_server port =
  Async_unix.Tcp.(
    Server.create_inet ~max_connections:1 ~backlog:16 ~on_handler_error:`Raise
      (Where_to_listen.of_port port) (fun _ reader writer ->
        handle (reader, writer)))

let main () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Debug);
  ignore @@ create_server 6379;
  ignore @@ Scheduler.go ()
