let example_input () =
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
"


type map =
  {
    grid: int array array;
    start: int * int;
    end_: int * int;
    width: int;
    height: int;
  }

let get map (y, x) = map.grid.(y).(x)

let rec height_of_char c =
  match c with
  | 'S' -> 0
  | 'E' -> height_of_char 'z'
  | _ -> (int_of_char c) - (int_of_char 'a')


let parse_input s =
  let lines = String.split_on_char '\n' s |> List.filter (fun s -> String.length s > 0) in
  let height = List.length lines in
  let width = String.length (List.hd lines) in

  let grid = Array.make_matrix height width 0 in
  let start = ref (0, 0) in
  let end_ = ref (0, 0) in

  (* Fill the grid array *)
  List.iteri
    (fun y line ->
      String.to_seq line
      |> Seq.iteri
           (fun x char ->
             Array.set grid.(y) x (height_of_char char);
             match char with
             | 'S' -> start := (y, x)
             | 'E' -> end_ := (y, x)
             | _ -> ()
           )
    )
    lines;
  {
    grid = grid;
    start = !start;
    end_ = !end_;
    width = width;
    height = height;
  }

let%test_unit "parse input" =
  let map = example_input () |> parse_input in
  [%test_eq: Base.int] (get map (0, 0)) 0;
  [%test_eq: Base.int] (get map (2, 5)) 25;
  [%test_eq: Base.int] (get map (2, 3)) 18;
  [%test_eq: Base.int] map.width 8;
  [%test_eq: Base.int] map.height 5;
  [%test_eq: Base.int * Base.int] map.start (0, 0);
  [%test_eq: Base.int * Base.int] map.end_ (2, 5)


(* We'll need a set of pairs to store already visited points to avoid repeating them *)
module IntPairs =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        0 -> Stdlib.compare y0 y1
      | c -> c
  end

module CoordSet = Set.Make(IntPairs)


(* Return locations that can be walked to from a given location *)
let walk map (y0, x0) =
  [(y0 + 1, x0); (y0 - 1, x0); (y0, x0 + 1); (y0, x0 - 1)]
  |> List.filter  (* Bounds *)
       (fun (y, x) -> (y >= 0 && y < map.height && x >= 0 && x < map.width))
  |> List.filter (* height check *)
       (fun (y, x) -> get map (y, x) <= (get map (y0, x0)) + 1)


let find_path starting_locations map =
  (* Breadth-first search *)
  let rec find visited steps locs =
    if (List.exists (fun loc -> loc = map.end_) locs)
    then steps
    else
      let new_locs =
        List.map (walk map) locs
        |> List.concat
        (* Convert to a set and back to only get unique new locations *)
        |> CoordSet.of_list
        |> CoordSet.to_seq
        |> List.of_seq
        (* Don't revisit already visited locations *)
        |> List.filter (fun loc -> not (CoordSet.mem loc visited))
      in
      find
        (List.fold_left (Fun.flip CoordSet.add) visited new_locs)
        (steps + 1)
        new_locs

  in
  find CoordSet.empty 0 starting_locations


let solve_part1 map =
  find_path [map.start] map


let%test_unit "Part 1, example" =
  [%test_eq: Base.int] (example_input () |> parse_input |> solve_part1) 31


let%expect_test "Part 1, input" =
  let input = In_channel.with_open_text "input" In_channel.input_all in
  parse_input input
  |> solve_part1
  |> print_int;
  [%expect {| 456 |}]

let solve_part2 map =
  let starts =
    map.grid
    |> Array.mapi
         (fun y row ->
           Array.to_seqi row
           |> Seq.filter_map (fun (x, h) -> if h = 0 then Some (y, x) else None)
           |> List.of_seq)
    |> Array.to_list
    |> List.concat
  in
  find_path starts map

let%test_unit "Part 2, example" =
  [%test_eq: Base.int] (example_input () |> parse_input |> solve_part2) 29

let%expect_test "Part 2, input" =
  let input = In_channel.with_open_text "input" In_channel.input_all in
  parse_input input
  |> solve_part2
  |> print_int;
  [%expect {| 454 |}]
