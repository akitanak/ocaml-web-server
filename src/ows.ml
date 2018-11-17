open Core;;
open Async;;
open CalendarLib;;

let get_current_datetime () =
  Printer.Calendar.sprint "%a, %d %b %Y %H:%M:%S" (Calendar.now ())
;;

type request_line =
  { 
    http_method: string;
    request_target: string;
    version: string;
  }

type header =
  {
    field_name: string;
    field_value: string;
  }

(* type request_headers =
  {
    request_line: request_line;
    headers: header list;
  } *)

let extract_request_line line =
  match String.split line ~on:' ' with
  | [http_method; request_target; version] ->  Some { http_method; request_target; version }
  | _ -> None

let read_request_line r =
  Reader.read_line r
  >>| function
  | `Eof -> None
  | `Ok line -> extract_request_line line

let extract_header line =
  match String.lsplit2 line ~on:':' with
  | Some (name, value) -> Some { field_name = String.strip name; field_value = String.strip value }
  | _ -> None

let rec read_headers acc r =
  Reader.read_line r
  >>= function
  | `Eof -> return acc
  | `Ok line -> 
    if (String.length line) = 0 then return acc
    else
      match extract_header line with
      | Some header -> read_headers (header :: acc) r
      | _ -> return acc

let read_content r headers =
  let exist_content_len h = h.field_name = "Content-Length" in
  match List.find headers ~f:exist_content_len with
  | None -> return None
  | Some content_length ->
    match int_of_string_opt content_length.field_value with
    | None -> return None
    | Some length ->
      let buffer = Bytes.create length in
      Reader.read r ~len:length buffer
      >>|function
      | `Eof -> None
      | `Ok _ -> Some buffer


let read_request r =
  read_request_line r
  >>= fun maybe_status_line ->
  read_headers [] r
  >>= fun headers ->
  read_content r headers
  >>| fun maybe_content ->
  (maybe_status_line, headers, maybe_content)

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
        read_request r
        >>= function
        | (None, _, _) ->
          Writer.write w "HTTP/1.1 400 Bad Request\n";
          Writer.flushed w
        | (Some request_line, _, Some content) ->
          print_string (Bytes.to_string content);
          Writer.write w (request_line.version ^ " " ^ "200" ^ " " ^ "OK"  ^"\n");
          Writer.flushed w
        | (Some request_line, _, None) ->
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
