open Common

let parse_line line =
  String.split_on_char ' ' line |> List.filter_map Stdlib.int_of_string_opt

let get_lists is_test =
  get_input 2 is_test |> String.split_on_char '\n'
  |> List.filter (string_empty % not)
  |> List.map parse_line

let diffs list = windows list |> List.map (uncurry ( - ))
let ok_inc i = i <= -1 && i >= -3
let ok_dec i = i >= 1 && i <= 3

let is_safe temps =
  diffs temps |> List.map ok_inc |> all || diffs temps |> List.map ok_dec |> all

let part1 is_test = get_lists is_test |> List.map is_safe |> bool_sum

let part2 is_test =
  let report_is_safe temps =
    List.length temps + 1
    |> range
    |> List.map (remove_nth temps)
    |> List.map is_safe |> any
  in
  get_lists is_test |> List.map report_is_safe |> bool_sum
