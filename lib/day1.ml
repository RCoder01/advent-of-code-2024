open Common

let parse_line line =
    let pairs = String.split_on_char ' ' line
    |> List.filter_map Stdlib.int_of_string_opt in
    match pairs with
    | a :: b :: [] -> Some (a, b)
    | _ -> None

let get_lists is_test =
    get_fname 1 is_test
    |> read_file
    |> String.split_on_char '\n'
    |> List.filter (fun s -> (String.length s) != 0)
    |> List.filter_map parse_line
    |> List.split
    |> map2 (List.sort compare)

let part1 is_test =
    get_lists is_test
    |> uncurry (List.map2 compare)
    |> List.map abs
    |> List.fold_left (+) 0

let rec groupby func list =
    match list with
    | a :: b :: rest -> if (func a b) != 0 then
        [a] :: (groupby func (b :: rest))
    else
        (match groupby func (b :: rest) with
        | first :: rest -> (a :: first) :: rest
        | [] -> [[a]]
        )
    | l -> [l]

let part2 is_test =
    let (left, right) = get_lists is_test in
    let counts = groupby compare right
    |> List.map (fun l -> (List.hd l, List.length l)) in
    let get_score e =
        List.assq_opt e counts
        |> Option.value ~default:0
        |> ( *) e in
    List.map get_score left
    |> List.fold_left (+) 0
