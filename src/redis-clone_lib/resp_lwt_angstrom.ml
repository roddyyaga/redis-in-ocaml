open Lwt.Infix
open Lwt.Syntax

let handle (ic, oc) =
  Angstrom_lwt_unix.parse_many Resp_angstorm.parse
    (fun parsed -> parsed |> Commands.handle |> Resp.encode |> Lwt_io.write oc)
    ic

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.on_failure
    (handle (ic, oc))
    (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e)));
  Logs_lwt.debug (fun m -> m "New connection") >>= Lwt.return

let listen_address = Unix.inet_addr_loopback

let create_server sock =
  let rec serve () = Lwt_unix.accept sock >>= accept_connection >>= serve in
  serve

let create_socket port =
  let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  let+ () = Lwt_unix.(bind sock @@ ADDR_INET (listen_address, port)) in
  Lwt_unix.listen sock 16;
  sock

let main () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Debug);
  let server =
    let* sock = create_socket 6379 in
    create_server sock ()
  in
  Lwt_main.run @@ server
