open Core;;
open Async;;
open CalendarLib;;

let get_current_datetime () =
  Printer.Calendar.sprint "%a, %d %b %Y %H:%M:%S" (Calendar.now ())
;;

let response_headers () = [
  ("Content-Type", "text/html");
  ("Content-Length", "48")
]

let write_response_headers w headers =
  let hdrs = List.fold headers ~init:"" ~f:(fun acc (k, v) -> Printf.sprintf "%s%s: %s\n" acc k v) in
  Writer.write w hdrs;
  Writer.write w "\n";
  Writer.flushed w

let run ()= 
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error: `Raise
      (Tcp.Where_to_listen.of_port 8765)
      (fun _addr r w ->
        Request.read r
        >>= function
        | (request_line, _, Some content) ->
          print_string (Bytes.to_string content);
          Writer.write w (request_line.version ^ " " ^ "200" ^ " " ^ "OK"  ^"\n");
          Writer.flushed w
        | (request_line, _, None) ->
          Writer.write w (request_line.version ^ " " ^ "200" ^ " " ^ "OK"  ^"\n");
          Writer.flushed w
        >>= fun () ->
        write_response_headers w (response_headers ())
        >>= fun () ->
        Writer.write w "<html><body><h1>Hello, Ocaml</h1></body></html>\n";
        Writer.flushed w)
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

let () =
  run ();
  never_returns (Scheduler.go ())
