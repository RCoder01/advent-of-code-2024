let get_fname day is_test =
    let day = Stdlib.string_of_int day in
    let test_suffix = if is_test then "t" else "" in
    "data/day" ^ day ^ test_suffix ^ ".txt"

let read_file fname =
    let chan = open_in fname in
    let len = in_channel_length chan in
    let result = (Bytes.create len) in
    really_input chan result 0 len;
    (Bytes.to_string result)

