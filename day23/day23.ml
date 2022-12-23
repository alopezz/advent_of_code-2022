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
module CoordMap = Map.Make(IntPairs)

(* We'll use (y, x) with y going top-down *)

let to_tuple a b = (a, b)

let parse s =
  String.split_on_char '\n' s
  |> List.filter (fun s -> String.length s > 0)
  |> List.mapi to_tuple
  |> List.fold_left
       (fun elves (y, line) ->
         String.to_seqi line
         |> Seq.fold_left
              (fun elves (x, c) ->
                match c with
                | '#' -> CoordSet.add (y, x) elves
                | _ -> elves
              )
              elves
       )
       CoordSet.empty


let has_elf coord elves =
  CoordSet.mem coord elves

let%test_unit "parse" =
  let elves = 
    ".....
..##.
..#..
.....
..##.
.....
"
  |> parse
  in
  [%test_eq: Base.bool] true (has_elf (1, 2) elves);
  [%test_eq: Base.bool] true (has_elf (1, 3) elves);
  [%test_eq: Base.bool] true (has_elf (4, 3) elves);
  [%test_eq: Base.bool] false (has_elf (0, 0) elves);
  [%test_eq: Base.bool] false (has_elf (0, 1) elves);
  [%test_eq: Base.bool] false (has_elf (-1, -1) elves)
  
  
type state = {
    elves: CoordSet.t;
    directions: (IntPairs.t -> CoordSet.t -> IntPairs.t option) list
  }


let tuple_sum (y0, x0) (y1, x1) = (y0 + y1, x0 + x1)

let north = (-1, 0)
let south = (1, 0)
let west = (0, -1)
let east = (0, 1)
let northeast = tuple_sum north east
let northwest = tuple_sum north west
let southeast = tuple_sum south east
let southwest = tuple_sum south west

let make_direction checks target =
  fun coord elves ->
  if List.exists (fun (y, x) -> has_elf (y, x) elves) (List.map (tuple_sum coord) checks)
  then None
  else Some (tuple_sum coord target)

let nobody_around =
  make_direction [north; south; west; east; northeast; northwest; southeast; southwest] (0, 0)

let play_round {elves; directions} =
  (* We create a map of destinations to count how many elves go to a destination,
     and then we can set as valid destinations only those that are targeted once. *)
  let valid_targets =
    CoordSet.fold
      (fun elf result ->
        let elf' = List.find_map (fun decide -> decide elf elves) (nobody_around :: directions) in
        let elf' =
          match elf' with
          | Some elf' -> elf'
          | None -> elf
        in
        CoordMap.update elf' (function | None -> Some 1 | Some x -> Some (x + 1)) result
      )
      elves
      CoordMap.empty
    |> CoordMap.to_seq
    |> Seq.filter_map (fun (k, v) -> if v = 1 then Some k else None)
    |> CoordSet.of_seq
  in
  (* Now we apply the actual move *)
  let new_elves =
    CoordSet.fold
      (fun elf result ->
        let elf' = List.find_map (fun decide -> decide elf elves) (nobody_around :: directions) in
        let elf' =
          match elf' with
          | Some elf' -> elf'
          | None -> elf
        in
        if CoordSet.mem elf' valid_targets
        then CoordSet.add elf' result
        else CoordSet.add elf result
      )
      elves
      CoordSet.empty
  in
  {
    elves = new_elves;
    directions = List.tl directions @ [List.hd directions]
  }

let init_directions =
  [
    make_direction [north; northwest; northeast] north;
    make_direction [south; southwest; southeast] south;
    make_direction [west; southwest; northwest] west;
    make_direction [east; southeast; northeast] east;
  ]

(* Sequence returning function *)
let play_game state =
  Seq.iterate play_round state

let render {elves; _} =
  let ys = CoordSet.to_seq elves |> Seq.map (fun (y, _x) -> y) in
  let xs = CoordSet.to_seq elves |> Seq.map (fun (_y, x) -> x) in
  let min_y = Seq.fold_left Int.min 0 ys in
  let max_y = Seq.fold_left Int.max 0 ys in
  let min_x = Seq.fold_left Int.min 0 xs in
  let max_x = Seq.fold_left Int.max 0 xs in
  List.init (max_y - min_y + 1)
    (fun rel_y ->
      List.init (max_x - min_x + 1)
        (fun rel_x ->
          if has_elf (min_y + rel_y, min_x + rel_x) elves
          then '#'
          else '.'
        )
      |> List.to_seq
      |> String.of_seq
    )
  |> String.concat "\n"
  

let solve_part1 elves =
  let initial_state = {
      elves;
      directions = init_directions
    }
  in
  let {elves = final_elves; _} =
    play_game initial_state
    |> Seq.drop 10
    |> Seq.uncons
    |> Option.get
    |> (fun (hd, _tl) -> hd)
  in
  let ys = CoordSet.to_seq final_elves |> Seq.map (fun (y, _x) -> y) in
  let xs = CoordSet.to_seq final_elves |> Seq.map (fun (_y, x) -> x) in
  let min_y = Seq.fold_left Int.min 0 ys in
  let max_y = Seq.fold_left Int.max 0 ys in
  let min_x = Seq.fold_left Int.min 0 xs in
  let max_x = Seq.fold_left Int.max 0 xs in
  (max_y - min_y + 1) * (max_x - min_x + 1) - CoordSet.cardinal final_elves
  

let%test_unit "solve part 1 for example" =
  "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..
"
  |> parse
  |> solve_part1
  |> [%test_eq: Base.int] 110

let%expect_test "solve part 1" =
  let elves = In_channel.with_open_text "input" In_channel.input_all |> parse in
  solve_part1 elves
  |> print_int;
  [%expect {| 4236 |}]


let solve_part2 elves =
  let initial_state = {
      elves;
      directions = init_directions
    }
  in
  let rec count_until_stop round state =
    let state' = play_round state in
    let round' = round + 1 in
    if state'.elves = state.elves then round'
    else count_until_stop round' state'
  in
  count_until_stop 0 initial_state

let%test_unit "solve part 1 for example" =
  "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..
"
  |> parse
  |> solve_part2
  |> [%test_eq: Base.int] 20

let%expect_test "solve part 2" =
  let elves = In_channel.with_open_text "input" In_channel.input_all |> parse in
  solve_part2 elves
  |> print_int;
  [%expect {| 1023 |}]


