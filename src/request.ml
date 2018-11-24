open Core
open Async

type request_line =
  { 
    http_method: string;
    request_target: string;
    version: string;
  }

exception Invalid_status_line of string;;

let status_line_regex = Re.Posix.compile_pat "([A-Z]+)[ ]([^ ]+)[ ](HTTP/[0-9].[0-9])"

let extract_request_line line =
  try
    let extracted = Re.exec status_line_regex line in
    {
      http_method = Re.get extracted 1;
      request_target = Re.get extracted 2;
      version = Re.get extracted 3;
    }
  with
  Not_found_s _ -> raise (Invalid_status_line "status line must be consists http method, request target, http version separated with space.")

let read_request_line r =
  Reader.read_line r
  >>| function
  | `Eof -> raise (Invalid_status_line "No status line")
  | `Ok line -> extract_request_line line

let read_content r headers =
  match HttpHeader.find headers "Content-Length" with
  | None -> return None
  | Some content_length ->
    match int_of_string_opt content_length.field_value with
    | None -> return None
    | Some length ->
      let buffer = Bytes.create length in
      Reader.read r ~len:length buffer
      >>| function
      | `Eof -> None
      | `Ok _ -> Some buffer

let read r =
  read_request_line r
  >>= fun status_line ->
  HttpHeader.read_headers [] r
  >>= fun headers ->
  read_content r headers
  >>| fun maybe_content ->
  (status_line, headers, maybe_content)
