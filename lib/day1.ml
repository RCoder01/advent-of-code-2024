let parse_line line =
    let substrs = String.split_on_char ' ' line in
    let pairs = List.filter_map Stdlib.int_of_string_opt substrs in
    match pairs with
    | a :: b :: [] -> Some (a, b)
    | _ -> None

let get_lists is_test =
    let filename = Common.get_fname 1 is_test in
    let str = Common.read_file filename in
    let lines = String.split_on_char '\n' str in
    let lines = List.filter (fun s -> (String.length s) <> 0) lines in
    let halves = List.filter_map parse_line lines in
    let (left, right) = List.split halves in
    let compare a b = a - b in
    let left = List.sort compare left in
    let right = List.sort compare right in
    (left, right)

let part1 is_test =
    let (left, right) = get_lists is_test in
    let diff a b = a - b in
    let diffs = List.map2 diff left right in
    let abs i = if i < 0 then -i else i in
    let diffs = List.map abs diffs in
    List.fold_left (fun acc e -> acc + e) 0 diffs

let rec groupby func list =
    match list with
    | a :: b :: rest -> if (func a b) != 0 then
        [a] :: (groupby func (b :: rest))
    else
        (match groupby func (b :: rest) with
        | first :: rest -> (a :: first) :: rest
        | [] -> [[a]]
        )
    | a :: [] -> [[a]]
    | [] -> []

let part2 is_test =
    let (left, right) = get_lists is_test in
    let cmp a b = a - b in
    let right = groupby cmp right in
    let counts = List.map (fun l -> (List.hd l, List.length l)) right in
    let get_score e =
        let count_opt = List.assq_opt e counts in
        let count = Option.value count_opt ~default:0 in
        e * count in
    let scores = List.map get_score left in
    List.fold_left (fun a b -> a + b) 0 scores

