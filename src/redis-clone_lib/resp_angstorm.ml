open Angstrom

let ( let* ) = ( >>= )

let ( let+ ) = ( >>| )

let is_cr = function '\r' -> true | _ -> false

let string_line = take_till is_cr <* string "\r\n"

let parse_simple_string = string_line >>| Resp.simple_string

let parse_error = string_line >>| Resp.error

let parse_integer = string_line >>| fun x -> x |> int_of_string |> Resp.integer

let parse_bulk_string =
  let* length = string_line >>| int_of_string in
  match length with
  | -1 -> Resp.null_bulk_string |> return
  | non_negative -> take non_negative <* take 2 >>| Resp.bulk_string

let parse =
  fix (fun parse ->
      let parse_array =
        let* length = string_line >>| int_of_string in
        match length with
        | -1 -> Resp.null_array |> return
        | non_negative -> count non_negative parse >>| Resp.array
      in
      let* kind = any_char in
      match kind with
      | '+' -> parse_simple_string
      | '-' -> parse_error
      | ':' -> parse_integer
      | '$' -> parse_bulk_string
      | '*' -> parse_array
      | other -> fail (Printf.sprintf "Unexpected type indicator %c" other))
