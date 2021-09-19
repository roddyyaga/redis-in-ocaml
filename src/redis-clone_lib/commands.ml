open Core

module Simple = struct
  type t =
    | S of string
    | Null
    | Error of string
    | Integer of int
    | Array of t list
  [@@deriving hash, compare, equal, sexp]

  let rec of_resp (resp : Resp.t) =
    match resp with
    | Simple_string s | Bulk_string s -> S s
    | Error s -> Error s
    | Integer i -> Integer i
    | Array xs -> Array (List.map ~f:of_resp xs)
    | Null_bulk_string | Null_array -> Null
end

let get_set_table = Hashtbl.create (module Simple)

let deque_table = Hashtbl.create (module Simple)

let command () = Resp.Simple_string "OK"

let set args =
  let open Simple in
  let () =
    match args with
    | [ key; S data ] -> Hashtbl.set get_set_table ~key ~data
    | _ -> ()
  in
  Resp.Simple_string "OK"

let get key =
  match Hashtbl.find get_set_table key with
  | Some data -> Resp.Bulk_string data
  | None -> Resp.Null_bulk_string

let config =
  let open Simple in
  function
  | [ S "GET"; S "save" ] ->
      Resp.(Array [ Simple_string "save"; Simple_string "" ])
  | [ S "GET"; S "appendonly" ] ->
      Resp.(Array [ Simple_string "appendonly"; Simple_string "no" ])
  | _ -> Resp.(Array [])

let lpush args =
  match args with
  | [ key; value ] ->
      let dq =
        Hashtbl.find_or_add deque_table key ~default:(fun () -> Deque.create ())
      in
      Deque.enqueue_front dq value;
      Deque.length dq |> Resp.integer
  | _ -> assert false

let handle resp =
  match Simple.of_resp resp with
  | Array xs -> (
      match List.hd_exn xs with
      | S "COMMAND" -> command ()
      | S "SET" -> set (List.tl_exn xs)
      | S "GET" -> get (List.nth_exn xs 1)
      | S "CONFIG" -> config (List.tl_exn xs)
      | S "LPUSH" -> lpush (List.tl_exn xs)
      | _ -> failwith "Not implemented" )
  | _ -> failwith "Unexpected non-array command"
