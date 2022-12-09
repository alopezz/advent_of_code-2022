
module IntPairs =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        0 -> Stdlib.compare y0 y1
      | c -> c
  end

module CoordSet = Set.Make(IntPairs)

type move =
  | Right
  | Left
  | Down
  | Up


type rope_state =
  {head: int * int; tail: (int * int) list}


let repetition times elt =
  Seq.repeat elt |> Seq.take times |> List.of_seq


let parse_instruction line =
  Scanf.sscanf line "%c %d"
    (fun c steps ->
      (match c with
      | 'R' -> Right
      | 'L' -> Left
      | 'U' -> Up
      | 'D' -> Down
      | _ -> failwith "Bad instruction")
      |> repetition steps
    )

let%test "parse instruction" = parse_instruction "R 4" = [Right; Right; Right; Right]
let%test "parse instruction" = parse_instruction "L 3" = [Left; Left; Left]


let sign = function
  | 0 -> 0
  | x when x > 0 -> 1
  | _ -> -1


let follow_head (x0, y0) ((x1, y1) as tail) =
  if Int.abs (y0 - y1) > 1 || Int.abs (x0 - x1) > 1
  then (x1 + sign (x0 - x1), y1 + sign (y0 - y1))
  else tail

let apply_move {head = (x0, y0); tail} direction =
  let new_head =
    match direction with
    | Right -> (x0 + 1, y0)
    | Left -> (x0 - 1, y0)
    | Up -> (x0, y0 + 1)
    | Down -> (x0, y0 - 1)
  in
  let _, new_tail =
    List.fold_left_map
      (fun head tail -> let new_tail = follow_head head tail in (new_tail, new_tail))
      new_head
      tail
  in
  {head = new_head; tail = new_tail}

let%test "move and follow, 1" =
  apply_move {head = (2, 2); tail = [(1, 1)]} Up = {head = (2, 3); tail = [2, 2]}

let%test "move and follow, 2" =
  apply_move {head = (2, 2); tail = [(1, 1)]} Right = {head = (3, 2); tail = [(2, 2)]}


let follow_instructions tail_length =
  List.fold_left_map
    (fun state instruction ->
      let new_state = apply_move state instruction in
      (new_state, List.rev new_state.tail |> List.hd)
    )
    {head = (0, 0); tail = (0, 0) |> repetition tail_length}


let solve n instructions =
  let (_final_state, tail_positions) = follow_instructions n instructions in
  CoordSet.of_list ((0, 0) :: tail_positions) |> CoordSet.cardinal

let read_instructions ic =
  In_channel.input_all ic
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s > 0)
  |> List.map parse_instruction
  |> List.concat


let solve_part1 = solve 1


let%test_unit "Solve part 1, example" =
  let input =
    "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2" |> String.split_on_char '\n'
  in
  let result =
    input
    |> List.map parse_instruction |> List.concat
    |> solve_part1
  in
  [%test_eq: Base.int] result 13
  
let%expect_test "Solve part 1, real input" =
  In_channel.with_open_text "input" read_instructions
  |> solve_part1
  |> print_int;
  [%expect {| 5883 |}]


let solve_part2 = solve 9


let%test_unit "Solve part 2, example" =
  let input =
    "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20" |> String.split_on_char '\n'
  in
  let result =
    input
    |> List.map parse_instruction |> List.concat
    |> solve_part2
  in
  [%test_eq: Base.int] result 36


let%expect_test "Solve part 2, real input" =
  In_channel.with_open_text "input" read_instructions
  |> solve_part2
  |> print_int;
  [%expect {| 2367 |}]
