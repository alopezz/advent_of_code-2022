type forest = {
    height: int;
    width: int;
    grid: ((int * int), int) Hashtbl.t
  }

let locate coord {grid; _} =
  Hashtbl.find grid coord


let parse_input lines =
  let height = ref 0 in
  let width = ref 0 in
  let grid = Hashtbl.create (100*1000) in
  Seq.take_while (fun line -> String.length line > 0) lines
  |> Seq.iteri
    (fun y line ->
      height := y + 1;
      String.to_seq line
      |> Seq.iteri
           (fun x char ->
             width := x + 1;
             Hashtbl.add grid (y, x) ((int_of_char char) - (int_of_char '0')))
    );
  {height = !height; width = !width; grid}


let example_input () =
  "30373
25512
65332
33549
35390
"
  |> String.split_on_char '\n'
  |> List.to_seq


let%test_unit "parse input" =
  let map = example_input () |> parse_input in
  [%test_eq: Base.int] map.height 5;
  [%test_eq: Base.int] map.width 5;
  [%test_eq: Base.int] (locate (1, 3) map) 1;
  [%test_eq: Base.int] (locate (4, 4) map) 0


(* Return coordinates of visible trees *)
let find_visible tree_row =
  let rec find result h row =
    match row with
    | (coord, hd) :: rest -> if hd > h then find (coord::result) hd rest else find result h rest
    | [] -> result
  in
  (* 0 height trees are still counted, so initial height needs to be under that *)
  find [] (-1) tree_row


let find_visible_trees map =
  let vertical = Seq.ints 0 |> Seq.take map.height |> List.of_seq in
  let horizontal = Seq.ints 0 |> Seq.take map.width |> List.of_seq in

  List.map
    (fun (slow, fast, order_coords) ->
      (* [fast] is the list of indices in the direction of the visibility check,
         [slow] is the list of indices to iterate over,
         [order_coords] is a function that returns (y, x) coords given the slow and fast indices *)
      slow
      |> List.map
           (fun a ->
             fast
             |> List.map (fun b -> order_coords a b)
             |> List.map (fun coord -> (coord, locate coord map))
             |> find_visible)
      |> List.concat)
    [
      (horizontal, vertical, (fun x y -> (y, x)));
      (vertical, horizontal, (fun y x -> (y, x)));
      (horizontal, vertical |> List.rev, (fun x y -> (y, x)));
      (vertical, horizontal |> List.rev, (fun y x -> (y, x)));
    ]
  |> List.concat


module IntPairs =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        0 -> Stdlib.compare y0 y1
      | c -> c
  end

module CoordSet = Set.Make(IntPairs)


let count_visible_trees map =
  find_visible_trees map |> CoordSet.of_list |> CoordSet.cardinal


let%test_unit "example for part 1" =
  let map = example_input () |> parse_input in
  [%test_eq: Base.int] (count_visible_trees map) 21


let read_input filename parse_input =
  In_channel.with_open_text filename
    (fun ic ->
      Seq.of_dispenser (fun () -> In_channel.input_line ic)
      |> parse_input)


let%expect_test "Part 1 solution" =
  read_input "input" parse_input
  |> count_visible_trees
  |> print_int;
  [%expect {| 1785 |}]


let measure_view height trees =
  let rec measure c t =
    match t with
    | hd :: rest when hd < height -> measure (c + 1) rest
    | _ :: _ -> c + 1
    | [] -> c
  in
  measure 0 trees


let find_viewing_distances (y, x) map =
  let up = Seq.ints 0 |> Seq.take y |> List.of_seq |> List.rev in
  let down = Seq.ints (y + 1) |> Seq.take (map.height - y - 1) |> List.of_seq in
  let left = Seq.ints 0 |> Seq.take x |> List.of_seq |> List.rev in
  let right = Seq.ints (x + 1) |> Seq.take (map.width - x - 1) |> List.of_seq in
  let current_height = locate (y, x) map in

  let locator ?fixed_y ?fixed_x =
    match (fixed_y, fixed_x) with
    | None, Some x -> (fun y -> locate (y, x) map)
    | Some y, _ -> (fun x -> locate (y, x) map)
    | _ -> (fun _ -> locate (y, x) map)
  in
  
  [
    measure_view current_height (List.map (locator ~fixed_x:x) up);
    measure_view current_height (List.map (locator ~fixed_y:y) left);
    measure_view current_height (List.map (locator ~fixed_y:y) right);
    measure_view current_height (List.map (locator ~fixed_x:x) down)
  ]


let%test_unit "viewing distances" =
  let map = example_input () |> parse_input in
  [%test_eq: Base.int Base.list] (find_viewing_distances (1, 2) map) [1; 1; 2; 2]


let scenic_score coord map =
  (* multiply viewing distances *)
  find_viewing_distances coord map |> List.fold_left ( * ) 1

let%test_unit "scenic score, example" =
  let map = example_input () |> parse_input in
  [%test_eq: Base.int] (scenic_score (1, 2) map) 4


let best_scenic_score map =
  let vertical = Seq.ints 0 |> Seq.take map.height in
  let horizontal = Seq.ints 0 |> Seq.take map.width in

  Seq.product vertical horizontal
  |> Seq.map (fun (y, x) -> scenic_score (y, x) map)
  |> Seq.fold_left (fun a b -> if a > b then a else b) 0


let%test_unit "highest scenic score, example" =
  let map = example_input () |> parse_input in
  [%test_eq: Base.int] (best_scenic_score map) 8

let%expect_test "highest scenic score, input" =
  read_input "input" parse_input
  |> best_scenic_score
  |> print_int;
  [%expect {| 345168 |}]
