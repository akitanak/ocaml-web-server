open Core;;
open Async;;

type status_line =
  {
    version: string;
    http_status: int;
    reason_phrase: string;
  }

let write_status_line w status_line =
  Writer.write w (status_line.version ^ " " ^ string_of_int status_line.http_status ^ " " ^ status_line.reason_phrase ^ "\n");
  Writer.flushed w

let write_content w content = 
  Writer.write w content;
  Writer.flushed w

let write w status_line headers content =
  write_status_line w status_line
  >>= fun () ->
  HttpHeader.write_headers w headers
  >>= fun () ->
  write_content w content
