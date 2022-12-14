(* For storing positions *)
module IntPairs =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        0 -> Stdlib.compare y0 y1
      | c -> c
  end

module CoordSet = Set.Make(IntPairs)


let range a b =
  let (a, b) = if a > b then (b, a) else (a, b) in
  List.init (b - a + 1) ((+) a)

let build_wall pos_0 pos_1 =
  match (pos_0, pos_1) with
  | ((x0, y0), (x1, _)) when x0 <> x1 -> range x0 x1 |> List.map (fun x -> (x, y0))
  | ((x0, y0), (_, y1)) when y0 <> y1 -> range y0 y1 |> List.map (fun y -> (x0, y))
  | _ -> [pos_0]

let%test_unit "build horizontal wall" =
  [%test_eq: (Base.int * Base.int) Base.list] (build_wall (498, 6) (496, 6)) [(496, 6); (497, 6); (498, 6)]

let%test_unit "build vertical wall" =
  [%test_eq: (Base.int * Base.int) Base.list] (build_wall (502, 4) (502, 9))
    [(502, 4); (502, 5); (502, 6); (502, 7); (502, 8); (502, 9)]


let build_rock_path path =
  let rec build rocks points =
    match points with
    | [] | [_] -> rocks
    | a :: b :: rest ->
       let rocks' =
         build_wall a b
         |> CoordSet.of_list
         |> CoordSet.union rocks
       in
       build rocks' (b :: rest)
  in
  build CoordSet.empty path

let%test_unit "build rock path, 1" =
  let path = build_rock_path [(498, 4); (498, 6); (496, 6)] in
  [%test_eq: Base.int] 5 (CoordSet.cardinal path)

let%test_unit "build rock path, 2" =
  let path = build_rock_path [(503, 4); (502, 4); (502, 9); (494, 4)] in
  [%test_eq: Base.int] 15 (CoordSet.cardinal path)


let build_map paths =
  List.fold_left
    (fun rocks path -> CoordSet.union rocks (build_rock_path path))
    CoordSet.empty
    paths

let cave_floor rocks =
  CoordSet.fold (fun (_, y1) y0 -> Int.max y0 y1) rocks 0


let solve_part1 paths =
  let rocks = build_map paths in
  let floor = cave_floor rocks in

  let rec simulate rocks_and_sand sand_unit =
    match sand_unit with
    (* Endless void, we're done *)
    | (_, y) when y = floor -> CoordSet.diff rocks_and_sand rocks |> CoordSet.cardinal
    (* Keep moving *)
    | (x, y) when not (CoordSet.mem (x, y + 1) rocks_and_sand) -> simulate rocks_and_sand (x, y + 1)
    | (x, y) when not (CoordSet.mem (x - 1, y + 1) rocks_and_sand) -> simulate rocks_and_sand (x - 1, y + 1)
    | (x, y) when not (CoordSet.mem (x + 1, y + 1) rocks_and_sand) -> simulate rocks_and_sand (x + 1, y + 1)
    (* Sand unit is at rest *)
    | (x, y) -> simulate (CoordSet.add (x, y) rocks_and_sand) (500, 0)
  in
  simulate rocks (500, 0)

let%test_unit "solve part 1 for example" =
  [[(498, 4); (498, 6); (496, 6)];
   [(503, 4); (502, 4); (502, 9); (494, 9)]]
  |> solve_part1
  |> [%test_eq: Base.int] 24


let parse_line line =
  Str.split (Str.regexp " -> ") line
  |> List.map (fun s ->
         match String.split_on_char ',' s with
         | [x; y] -> (int_of_string x, int_of_string y)
         | _ -> failwith "Bad input")

let parse s =
  String.split_on_char '\n' s
  |> List.filter (fun s -> String.length s > 0)
  |> List.map parse_line

let%test_unit "parse input" =
  parse "
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
"
  |> [%test_eq: (Base.int * Base.int) Base.list Base.list]
       [[(498, 4); (498, 6); (496, 6)];
        [(503, 4); (502, 4); (502, 9); (494, 9)]]

let%expect_test "solve part 1" =
  In_channel.with_open_text "input" In_channel.input_all
  |> parse
  |> solve_part1
  |> print_int;
  [%expect {| 644 |}]


let solve_part2 paths =
  let rocks = build_map paths in
  let floor = (cave_floor rocks) + 2 in

  let rec simulate rocks_and_sand sand_unit =
    match sand_unit with
    (* Hit the floor *)
    | (x, y) when y = floor -> simulate (CoordSet.add (x, y - 1) rocks_and_sand) (500, 0)
    (* Keep moving *)
    | (x, y) when not (CoordSet.mem (x, y + 1) rocks_and_sand) -> simulate rocks_and_sand (x, y + 1)
    | (x, y) when not (CoordSet.mem (x - 1, y + 1) rocks_and_sand) -> simulate rocks_and_sand (x - 1, y + 1)
    | (x, y) when not (CoordSet.mem (x + 1, y + 1) rocks_and_sand) -> simulate rocks_and_sand (x + 1, y + 1)
    (* Sand unit is at rest, we're done if this is the starting point *)
    | (500, 0) -> 1 + (CoordSet.diff rocks_and_sand rocks |> CoordSet.cardinal)
    | (x, y) -> simulate (CoordSet.add (x, y) rocks_and_sand) (500, 0)
  in
  simulate rocks (500, 0)

let%test_unit "solve part 2 for example" =
  [[(498, 4); (498, 6); (496, 6)];
   [(503, 4); (502, 4); (502, 9); (494, 9)]]
  |> solve_part2
  |> [%test_eq: Base.int] 93

let%expect_test "solve part 2"=
  In_channel.with_open_text "input" In_channel.input_all
  |> parse
  |> solve_part2
  |> print_int;
  [%expect {| 27324 |}]
