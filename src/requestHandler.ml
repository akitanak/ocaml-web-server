open Core
open Async

let read_content request_target =
  let base_dir = "/var/www/html" in
  Reader.file_contents (base_dir ^ request_target)

let handle_request request_line =
  match request_line with
  | { Request.http_method = "GET"; Request.request_target = request_target; Request.version = _ } ->
    read_content request_target
  | { Request.http_method = _; Request.request_target = _; Request.version = _ } ->
    return "No content"
