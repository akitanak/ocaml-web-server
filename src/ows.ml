open Core
open Async
open CalendarLib

let get_current_datetime () =
  Printer.Calendar.sprint "%a, %d %b %Y %H:%M:%S" (Calendar.now ())
;;

let response_headers () = [
  { HttpHeader.field_name = "Content-Type"; HttpHeader.field_value = "text/html" };
]

let run ()= 
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error: `Raise
      (Tcp.Where_to_listen.of_port 8765)
      (fun _addr r w ->
        Request.read r
        >>= function
        | (request_line, _, _) ->
          try_with(fun () -> RequestHandler.handle_request request_line)
          >>= function
          | Ok content ->
            let status_line = { Response.version = request_line.version; Response.http_status = 200; Response.reason_phrase = "OK" } in
            let content_type = { HttpHeader.field_name = "Content-Length"; HttpHeader.field_value = string_of_int (String.length content)} in
            Response.write w status_line (content_type :: (response_headers ())) content
          | Error _ ->
            let status_line = { Response.version = request_line.version; Response.http_status = 404; Response.reason_phrase = "Not Found" } in
            let content = "Not Found" in
            let content_type = { HttpHeader.field_name = "Content-Length"; HttpHeader.field_value = string_of_int (String.length content)} in
            Response.write w status_line (content_type :: (response_headers ())) content
      )
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

let () =
  run ();
  never_returns (Scheduler.go ())
