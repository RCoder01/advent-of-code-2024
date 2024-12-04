open Common

let rec matches pattern string =
  if String.length string == 0 then []
  else
    try
      if Str.search_forward pattern string 0 != max_int then
        let m = Str.matched_string string in
        m :: matches pattern (Str.string_after string (Str.match_end ()))
      else []
    with Not_found -> []

let mul_pat = {|mul(\([0-9]*\),\([0-9]*\))|}

let apply_mul str =
  let () = assert (Str.string_match (Str.regexp mul_pat) str 0) in
  (Str.matched_group 1 str |> Stdlib.int_of_string)
  * (Str.matched_group 2 str |> Stdlib.int_of_string)

let part1 is_test =
  get_input 3 is_test
  |> matches (Str.regexp mul_pat)
  |> List.map apply_mul |> sum

let do_pat = {|\(do()\)|}
let dont_pat = {|\(don't()\)|}

let complete_pat =
  {|\(|} ^ mul_pat ^ {|\)\|\(|} ^ do_pat ^ {|\)\|\(|} ^ dont_pat ^ {|\)|}

let part2 is_test =
  let rec filter_sum filter list =
    match list with
    | value :: rest ->
        if Str.string_match (Str.regexp do_pat) value 0 then
          filter_sum true rest
        else if Str.string_match (Str.regexp dont_pat) value 0 then
          filter_sum false rest
        else (if filter then apply_mul value else 0) + filter_sum filter rest
    | [] -> 0
  in
  get_input 3 is_test |> matches (Str.regexp complete_pat) |> filter_sum true
