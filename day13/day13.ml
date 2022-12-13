open Packet

let parse parser s =
  Lexing.from_string s |> parser Packet_lexer.token

let parse_pairs = parse Packet_parser.packet_pair_list


let%test "parse_pairs, simplest" =
  parse_pairs "[]
[]

[]
[]
" = [([], []); ([], [])]


let%test "parse_pairs" =
  parse_pairs "[[],[[],5]]
[[8],[7,2,10]]
" = [([PList []; PList [PList []; PInt 5]],
      [PList [PInt 8]; PList [PInt 7; PInt 2; PInt 10]])]

let compare left right =
  let rec recur left right =
    match (left, right) with
    (* Ints against ints *)
    | (PInt left, PInt right) when left = right -> None
    | (PInt left, PInt right) -> Some (left < right)
    (* Type promotions (list vs int) *)
    | (PInt _ as left, (PList _ as right)) -> recur (PList [left]) right
    | (PList _ as left, (PInt _ as right)) -> recur left (PList [right])
    (* List against list *)
    | (PList [], PList []) -> None
    | (PList (_ :: _), PList []) -> Some false
    | (PList [], PList (_ :: _)) -> Some true
    | (PList (left_hd :: left_tl), PList (right_hd :: right_tl)) ->
       match recur left_hd right_hd with
       | Some a -> Some a
       | None -> recur (PList left_tl) (PList right_tl)

  in
  match recur (PList left) (PList right) with
  | Some true -> -1
  | Some false -> 1
  | None -> 0

let is_right_order left right = (compare left right) <= 0


let%test "is_right_order, empty" = is_right_order [] []

let%test "is_right_order, equal" = is_right_order [PList [PInt 3; PList []]] [PList [PInt 3; PList []]]

let%test "is_right_order, pair 1" =
  is_right_order
    [PInt 1; PInt 1; PInt 3; PInt 1; PInt 1]
    [PInt 1; PInt 1; PInt 5; PInt 1; PInt 1]

let%test "is_right_order, pair 1 inverted" =
  is_right_order
    [PInt 1; PInt 1; PInt 5; PInt 1; PInt 1]
    [PInt 1; PInt 1; PInt 3; PInt 1; PInt 1]
  |> not

let%test "is_right_order, pair 2" =
  is_right_order
    [PList [PInt 1]; PList [PInt 2; PInt 3; PInt 4]]
    [PList [PInt 1]; PInt 4]

let%test "is_right_order, pair 2 inverted" =
  is_right_order
    [PList [PInt 1]; PInt 4]
    [PList [PInt 1]; PList [PInt 2; PInt 3; PInt 4]]
  |> not

let%test "is_right_order, pair 3" =
  is_right_order
    [PInt 9]
    [PList [PInt 8; PInt 7; PInt 6]]
  |> not

let%test "is_right_order, pair 4" =
  is_right_order
    [PList [PInt 4; PInt 4]; PInt 4; PInt 4]
    [PList [PInt 4; PInt 4]; PInt 4; PInt 4; PInt 4]

let%test "is_right_order, pair 5" =
  is_right_order
    [PInt 7; PInt 7; PInt 7; PInt 7]
    [PInt 7; PInt 7; PInt 7]
  |> not

let%test "is_right_order, pair 6" =
  is_right_order
    []
    [PInt 3]

let%test "is_right_order, pair 7" =
  is_right_order
    [PList [PList [PList []]]]
    [PList []]
  |> not


let solve_part1 pairs =
  List.mapi (fun idx pair -> (idx, pair)) pairs
  |> List.filter_map (fun (idx, (a, b)) -> if is_right_order a b then Some (idx + 1) else None)
  |> List.fold_left (+) 0


let%test_unit "solve part 1 for example" =
  [%test_eq: Base.int]
    (In_channel.with_open_text "example_input" In_channel.input_all |> parse_pairs |> solve_part1)
    13

let%expect_test "solve part 1" =
  In_channel.with_open_text "input" In_channel.input_all |> parse_pairs |> solve_part1
  |> print_int;
  [%expect {| 5198 |}]


let parse_packets = parse Packet_parser.packet_list

let solve_part2 packets =
  let dividers = [[PList [PInt 2]]; [PList [PInt 6]]] in
  let indexed_packets =
    dividers @ packets
    |> List.sort compare
    |> List.mapi (fun idx p -> (idx, p))
  in
  List.map
    (fun div ->
      List.find_map (fun (idx, p) -> if p = div then Some (idx + 1) else None) indexed_packets
      |> Option.get
    )
    dividers
  |> List.fold_left ( * ) 1

let%test_unit "solve part 2 for example" =
  [%test_eq: Base.int]
    (In_channel.with_open_text "example_input" In_channel.input_all |> parse_packets |> solve_part2)
    140

let%expect_test "solve part 2" =
  In_channel.with_open_text "input" In_channel.input_all |> parse_packets |> solve_part2
  |> print_int;
  [%expect {| 22344 |}]
