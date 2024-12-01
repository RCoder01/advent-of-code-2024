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
