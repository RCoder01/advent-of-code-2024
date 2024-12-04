let ( % ) a b c = a c |> b
let map2 f (a, b) = (f a, f b)
let uncurry f (x, y) = f x y

let get_fname day is_test =
  "data/day" ^ Stdlib.string_of_int day ^ (if is_test then "t" else "") ^ ".txt"

let read_file fname =
  let chan = open_in fname in
  let len = in_channel_length chan in
  let result = Bytes.create len in
  really_input chan result 0 len;
  Bytes.to_string result

let get_input day = get_fname day % read_file
let string_empty s = String.length s == 0

let rec remove_nth list n =
  match (n, list) with
  | 0, _ :: rest -> rest
  | n, v :: rest -> v :: remove_nth rest (n - 1)
  | _, [] -> []

let remove_last list = remove_nth list (List.length list - 1)

let rec sublist n list =
  match (n, list) with
  | 0, _ -> []
  | _, [] -> []
  | n, hd :: tl -> hd :: sublist (n - 1) tl

let windows2 list =
  List.map2 (fun a b -> (a, b)) (remove_last list) (List.tl list)

let rec windows n list =
  if n <= 0 || List.length list < n then []
  else sublist n list :: windows n (List.tl list)

let sum = List.fold_left ( + ) 0
let product = List.fold_left ( * ) 1
let any pred = List.filter pred % List.length % (>) 0
let all pred list = not (any (fun e -> not (pred e)) list)
let none pred list = not (any pred list)

let split_opt list =
  match list with [] -> None | head :: rest -> Some (head, rest)

let hd_opt list = split_opt list |> Option.map (fun (hd, _) -> hd)
let tl_opt list = split_opt list |> Option.map (fun (_, tl) -> tl)

let rec range n =
  if n == 0 then [] else range (n - 1) |> List.map (( + ) 1) |> List.cons 0

let string_rev =
  String.to_seq % List.of_seq % List.rev % List.to_seq % String.of_seq

let rec list_transpose list =
  let merge (hds, tls) (hd, tl) = (hd :: hds, tl :: tls) in
  let hds, tls =
    List.filter_map split_opt list |> List.fold_left merge ([], [])
  in
  if List.is_empty hds then []
  else List.rev hds :: list_transpose (List.rev tls)

let string_transpose =
  List.map (String.to_seq % List.of_seq)
  % list_transpose
  % List.map (List.to_seq % String.of_seq)
