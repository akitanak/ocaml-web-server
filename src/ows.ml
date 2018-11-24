open Core
open Async
open CalendarLib

let get_current_datetime () =
  Printer.Calendar.sprint "%a, %d %b %Y %H:%M:%S" (Calendar.now ())
;;

let response_headers () = [
  { HttpHeader.field_name = "Content-Type"; HttpHeader.field_value = "text/html" };
  { HttpHeader.field_name = "Content-Length"; HttpHeader.field_value = "48" };
]

let run ()= 
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error: `Raise
      (Tcp.Where_to_listen.of_port 8765)
      (fun _addr r w ->
        Request.read r
        >>= function
        | (request_line, _, Some content) ->
          print_string (get_current_datetime () ^ " " ^ request_line.http_method ^ " " ^ Bytes.to_string content ^ "\n");
          let status_line = { Response.version = request_line.version; Response.http_status = 200; Response.reason_phrase = "OK" } in
          Response.write w status_line (response_headers ()) "<html><body><h1>Hello, Ocaml</h1></body></html>\n"
        | (request_line, _, None) ->
          let status_line = { Response.version = request_line.version; Response.http_status = 200; Response.reason_phrase = "OK" } in
          Response.write w status_line (response_headers ()) "<html><body><h1>Hello, Ocaml</h1></body></html>\n"
      )
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

let () =
  run ();
  never_returns (Scheduler.go ())
