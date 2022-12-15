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

type pair = IntPairs.t

type sensor = { coord: pair; beacon: pair }

let parse s =
  String.split_on_char '\n' s
  |> List.filter (fun s -> String.length s > 0)
  |> List.map
       (fun s ->
         Scanf.sscanf s "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
           (fun x y b_x b_y -> {coord = (x, y); beacon = (b_x, b_y)})
       )


let%test_unit "Parse input" =
  let sensors = parse
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
"
  in
  [%test_eq: (Base.int * Base.int) Base.list] [(2, 18); (9, 16)] (List.map (fun s -> s.coord) sensors);
  [%test_eq: (Base.int * Base.int) Base.list] [(-2, 15); (10, 16)] (List.map (fun s -> s.beacon) sensors)


let merge a b =
  match (a, b) with
  | ((x0, x1), (y0, y1)) when x0 <= y0 && x1 >= y0 -> Some (x0, Int.max x1 y1)
  | ((x0, x1), (y0, y1)) when y0 <= x0 && y1 >= x0 -> Some (y0, Int.max x1 y1)
  | _ -> None


(* This function tries to merge [cur_line] against some line in [lines].
   If successful, it will return [Some new_lines] with the merged result,
   and it will return [None] if no merge occurred.
 *)
let merge_line_into_lines cur_line lines =
  let rec merge_rec prev_lines = function
    | hd :: tl ->
       (match merge cur_line hd with
        | Some merged -> Some (merged :: (prev_lines @ tl))
        | None -> merge_rec (hd :: prev_lines) tl
       )
    | [] -> None

  in merge_rec [] lines


(* This function tries to perform a single merge. Returns an option type with
   the merged lines if a merge occured, and [None] if no merge occured.
 *)
let merge_lines_once lines =
  let rec merge_rec prev_lines = function
    | hd :: tl ->
       (match merge_line_into_lines hd (prev_lines @ tl) with
        | Some merged -> Some merged
        | None -> merge_rec (hd :: prev_lines) tl
       )
    | [] -> None
  in
  merge_rec [] lines  

let merge_lines lines =
  let rec loop lines =
    match merge_lines_once lines with
    | Some lines -> loop lines
    | None -> lines
  in
  loop lines
  |> List.sort IntPairs.compare

let%test_unit "intersect 2 overlapping lines" =
  merge_lines [(3, 8); (1, 4)]
  |> [%test_eq: (Base.int * Base.int) Base.list] [(1, 8)];
  merge_lines [(0, 4); (3, 9)]
  |> [%test_eq: (Base.int * Base.int) Base.list] [(0, 9)]

let%test_unit "intersect 2 lines, one containing the other" =
  merge_lines [(3, 5); (1, 7)]
  |> [%test_eq: (Base.int * Base.int) Base.list] [(1, 7)];
  merge_lines [(1, 8); (4, 5)]
  |> [%test_eq: (Base.int * Base.int) Base.list] [(1, 8)]
  
  
let%test_unit "intersect 3 lines, all overlapping" =
  merge_lines [(1, 4); (4, 5); (4, 8)]
  |> [%test_eq: (Base.int * Base.int) Base.list] [(1, 8)]

let%test_unit "intersect 3 lines, with jump" =
  merge_lines [(1, 4); (10, 12); (4, 8)]
  |> [%test_eq: (Base.int * Base.int) Base.list] [(1, 8); (10, 12)]

let manhattan (x0, y0) (x1, y1) =
  Int.abs (x0 - x1) + Int.abs (y0 - y1)

let combine_sensor_ranges_at_y target_y sensors =
  List.map
    (fun {coord; beacon} ->
      let distance = manhattan coord beacon in
      let (x0, y0) = coord in
      let distance_at_y = distance - (Int.abs (y0 - target_y)) in
      if distance_at_y <= 0 then []
      else
        [x0 - distance_at_y, x0 + distance_at_y]      
    )
    sensors
  |> List.concat
  |> merge_lines


let solve_part1 target_y sensors =
  let count = combine_sensor_ranges_at_y target_y sensors
    |> List.map (fun (a, b) -> b - a + 1)
    |> List.fold_left (+) 0
  in
  (* Remove beacons on the line from the count *)
  let beacon_set = CoordSet.of_list (List.map (fun {beacon; _} -> beacon) sensors) in
  count - (CoordSet.filter (fun (_, y0) -> y0 = target_y) beacon_set |> CoordSet.cardinal)

let%test_unit "solve part 1 for example" =
  let sensors = In_channel.with_open_text "example_input" In_channel.input_all |> parse in
  solve_part1 10 sensors
  |> [%test_eq: Base.int] 26

let%expect_test "solve part 1" =
  let sensors = In_channel.with_open_text "input" In_channel.input_all |> parse in
  solve_part1 2000000 sensors
  |> print_int;
  [%expect {| 5100463 |}]

let solve_part2 max_coord sensors =
  Seq.ints 0
  |> Seq.take_while (fun x -> x <= max_coord)
  |> Seq.find_map
       (fun target_y ->
         match combine_sensor_ranges_at_y target_y sensors with
         | [(_, x0); (x1, _)] when x1 > x0 + 1 -> Some (x0 + 1, target_y)
         | [(x0, _)] when x0 > 0 -> Some (0, target_y)
         | [(_, x1)] when x1 < max_coord -> Some (max_coord, target_y)
         | _ -> None
       )
  |> Option.get
  |> (fun (x, y) -> x * 4000000 + y)

let%test_unit "solve part 2 for example" =
  let sensors = In_channel.with_open_text "example_input" In_channel.input_all |> parse in
  solve_part2 20 sensors
  |> [%test_eq: Base.int] 56000011

let%expect_test "solve part 2" =
  let sensors = In_channel.with_open_text "input" In_channel.input_all |> parse in
  solve_part2 4000000 sensors
  |> print_int;
  [%expect {| 11557863040754 |}]
