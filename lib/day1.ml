open Common

let parse_line line =
    let pairs = String.split_on_char ' ' line
    |> List.filter_map Stdlib.int_of_string_opt in
    match pairs with
    | a :: b :: [] -> Some (a, b)
    | _ -> None

let get_lists is_test =
    get_input 1 is_test
    |> String.split_on_char '\n'
    |> List.filter (string_empty % not)
    |> List.filter_map parse_line
    |> List.split
    |> map2 (List.sort compare)

let part1 is_test =
    get_lists is_test
    |> uncurry (List.map2 (-))
    |> List.map abs
    |> List.fold_left (+) 0

let part2 is_test =
    let (left, right) = get_lists is_test in
    let get_score e =
        List.map (((==) e) % Bool.to_int) right
        |> List.fold_left (+) 0
        |> ( * ) e in
    List.map get_score left
    |> List.fold_left (+) 0
