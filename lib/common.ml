let map2 f (a, b) = (f a, f b)

let uncurry f (x, y) = f x y

let (%) a b c = a c |> b

let get_fname day is_test =
    "data/day" ^ Stdlib.string_of_int day ^ (if is_test then "t" else "") ^ ".txt"

let read_file fname =
    let chan = open_in fname in
    let len = in_channel_length chan in
    let result = Bytes.create len in
    really_input chan result 0 len;
    Bytes.to_string result

let get_input day is_test = get_fname day is_test |> read_file

let string_empty s = String.length s == 0

let rec remove_nth list n =
    match n, list with
    | 0, _ :: rest -> rest
    | n, v :: rest -> v :: remove_nth rest (n - 1)
    | _, [] -> []

let remove_last list = remove_nth list (List.length list - 1)

let windows list = List.map2 (fun a b -> (a, b)) (remove_last list) (List.tl list)

let sum = List.fold_left (+) 0
let product = List.fold_left ( * ) 1

let all = List.fold_left (&&) true
let any = List.fold_left (||) false
let none list = not (any list)
let bool_sum list = List.map Bool.to_int list |> sum

let split_opt list = match list with
    | [] -> None
    | head :: rest -> Some (head, rest)
let hd_opt list = split_opt list |> Option.map (fun (hd, _) -> hd)
let tl_opt list = split_opt list |> Option.map (fun (_, tl) -> tl)

let rec range n =
    if n == 0 then
        []
    else
        range (n - 1)
        |> List.map ((+) 1)
        |> List.cons 0
