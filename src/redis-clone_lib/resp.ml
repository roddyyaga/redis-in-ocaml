type t =
  | Simple_string of string
  | Error of string
  | Integer of int
  | Bulk_string of string
  | Array of t list
  | Null_bulk_string
  | Null_array
[@@deriving variants]

let rec encode = function
  | Simple_string s -> Printf.sprintf "+%s\r\n" s
  | Error s -> Printf.sprintf "-%s\r\n" s
  | Integer i -> Printf.sprintf ":%d\r\n" i
  | Bulk_string s -> Printf.sprintf "$%d\r\n%s\r\n" (String.length s) s
  | Array xs ->
      let elements = List.map encode xs |> String.concat "" in
      Printf.sprintf "*%d\r\n%s" (List.length xs) elements
  | Null_bulk_string -> "$-1\r\n"
  | Null_array -> "*-1\r\n"

let rec faraday t value =
  let open Faraday in
  match value with
  | Simple_string s ->
      write_char t '+';
      write_string t s;
      write_string t "\r\n"
  | Error s ->
      write_char t '-';
      write_string t s;
      write_string t "\r\n"
  | Integer i ->
      write_char t ':';
      write_string t (string_of_int i);
      write_string t "\r\n"
  | Bulk_string s ->
      let length = String.length s in
      write_char t '$';
      write_string t (string_of_int length);
      write_string t "\r\n";
      write_string t s;
      write_string t "\r\n"
  | Array xs ->
      let length = List.length xs in
      write_char t '*';
      write_string t (string_of_int length);
      write_string t "\r\n";
      List.iter (faraday t) xs
  | Null_bulk_string -> write_string t "$-1\r\n"
  | Null_array -> write_string t "*-1\r\n"
