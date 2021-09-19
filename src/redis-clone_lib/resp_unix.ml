open Core

let handle (ic, oc) =
  Angstrom_unix.parse_many Resp_angstorm.parse
    (fun parsed ->
      let buf = parsed |> Commands.handle |> Resp.encode |> Bytes.of_string in
      ignore @@ Unix.single_write oc ~buf)
    ic
  |> snd |> Result.ok_or_failwith

let create_server port =
  let server = Unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  Unix.bind server ~addr:(ADDR_INET (Unix.Inet_addr.localhost, port));
  Unix.listen server ~backlog:16;
  let rec loop () =
    let client = Unix.accept server |> fst in
    let ic = Unix.in_channel_of_descr client in
    handle (ic, client);
    loop ()
  in
  loop ()

let main () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Debug);
  create_server 6379
