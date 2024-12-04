open Common

let parse_line line =
  String.split_on_char ' ' line |> List.filter_map Stdlib.int_of_string_opt

let get_lists is_test =
  get_input 2 is_test |> String.split_on_char '\n'
  |> List.filter (string_empty % not)
  |> List.map parse_line

let diffs = windows2 % List.map (uncurry ( - ))
let ok_inc i = i <= -1 && i >= -3
let ok_dec i = i >= 1 && i <= 3

let is_safe temps =
  diffs temps |> all ok_inc || diffs temps |> all ok_dec

let part1 is_test = get_lists is_test |> List.filter is_safe |> List.length

let part2 is_test =
  let report_is_safe temps =
    List.length temps + 1
    |> range
    |> List.map (remove_nth temps)
    |> any is_safe
  in
  get_lists is_test |> List.filter report_is_safe |> List.length
