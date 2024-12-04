open Common

let rec count pat string =
  if String.length string < String.length pat then 0
  else
    (String.equal (String.sub string 0 (String.length pat)) pat |> Bool.to_int)
    + count pat (Str.string_after string 1)

let count2 string = count "XMAS" string + count (string_rev "XMAS") string

let rec diagonalize_align strings =
  match split_opt strings with
  | Some (str, rest) ->
      str :: (List.map (fun s -> " " ^ s) rest |> diagonalize_align)
  | None -> []

let diagonalize strings =
  diagonalize_align strings |> string_transpose |> List.map String.trim

let part1 is_test =
  let lines = get_input 4 is_test |> String.split_on_char '\n' in
  [
    lines;
    string_transpose lines;
    diagonalize lines;
    List.map string_rev lines |> diagonalize;
  ]
  |> List.map (List.map count2 % sum)
  |> sum

let is_mas_x view =
  match view with
  | [ [ a; _; b ]; [ _; 'A'; _ ]; [ c; _; d ] ] -> 
    [[a; b; c; d]; [a; c; b; d]; [b; d; a; c]; [c; d; a; b]]
    |> any (List.equal (==) ['M'; 'M'; 'S'; 'S'])
  | _ -> false

let part2 is_test =
  get_input 4 is_test
  |> String.split_on_char '\n'
  |> List.map (String.to_seq % List.of_seq)
  |> windows 3
  |> List.map list_transpose
  |> List.map (windows 3)
  |> List.flatten
  |> List.filter is_mas_x
  |> List.length
