open Core;;
open Async;;

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
      >>| function
      | `Eof -> None
      | `Ok _ -> Some buffer

let read r =
  read_request_line r
  >>= fun status_line ->
  read_headers [] r
  >>= fun headers ->
  read_content r headers
  >>| fun maybe_content ->
  (status_line, headers, maybe_content)
