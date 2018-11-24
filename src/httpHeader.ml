open Core
open Async

type header =
  {
    field_name: string;
    field_value: string;
  }

let find headers name =
  List.find headers ~f:(fun header -> header.field_name = name)

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

let write_header w header =
  Writer.write w (header.field_name ^ ": " ^ header.field_value ^ "\n");
  Writer.flushed w

let rec write_headers w headers =
  match headers with
  | hd :: rest ->
    write_header w hd
    >>= fun () ->
    write_headers w rest
  | [] ->
    Writer.write w "\n";
    Writer.flushed w
