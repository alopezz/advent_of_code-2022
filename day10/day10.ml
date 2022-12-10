let run_instruction instruction x =
  match String.split_on_char ' ' instruction with
  | ["noop"] -> [x]
  | ["addx"; v] -> [x; x + (int_of_string v)]
  | _ -> failwith (Printf.sprintf "Bad instruction: %s" instruction)


let run_all_instructions instructions =
  List.fold_left
      (fun states instruction ->
        let cur_state = List.hd states in
        let new_states = run_instruction instruction cur_state in
        (new_states |> List.rev) @ states)
      [1]
      instructions
    |> List.rev
  |> Array.of_list


let read_input filename =
  In_channel.with_open_text filename In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s > 0)


let%test_unit "Example part 1, specific examples" =
  let states = read_input "example_input" |> run_all_instructions in
  [%test_eq: Base.int] states.(19) 21;
  [%test_eq: Base.int] states.(59) 19;
  [%test_eq: Base.int] states.(219) 18


let interesting_strengths states =
  Seq.ints 0
  |> Seq.map (fun x -> 20 + 40*x)
  |> Seq.take_while (fun cycle -> cycle <= 220)
  |> Seq.map (fun cycle -> states.(cycle - 1) * cycle)


let%test_unit "Example part 1, specific examples" =
  let strengths =
    read_input "example_input"
    |> run_all_instructions
    |> interesting_strengths
    |> List.of_seq
  in
  [%test_eq: Base.int Base.list] strengths [420; 1140; 1800; 2940; 2880; 3960]


let solve_part1 instructions =
  run_all_instructions instructions
  |> interesting_strengths
  |> Seq.fold_left (+) 0

let%test_unit "Example, part 1" =
  let result = read_input "example_input" |> solve_part1 in
  [%test_eq: Base.int] result 13140


let%expect_test "Part 1, actual input" =
  read_input "input"
  |> solve_part1
  |> print_int;
  [%expect {| 15680 |}]


(* [xs] is an array of int representing register values in each cycle
   This function applies the drawing rules to return a boolean array
   indicating what pixels should be drawn. *)
let draw xs =
  let screen = Array.make 240 false in

  for i = 0 to 239 do
    let x = Int.rem i 40 in
    if x >= xs.(i) - 1 && x <= xs.(i) + 1
    then
      screen.(i) <- true
  done;

  screen

(* Takes a boolean array with the state of each pixel and renders it
   as a string for display *)
let render xs =
  let render_line line_start =
    Array.sub xs line_start 40
    |> Array.to_seq
    |> Seq.map (fun x -> if x then '#' else '.')
    |> String.of_seq
  in
  List.map render_line (List.init 6 (( * ) 40))
  |> String.concat "\n"


let%expect_test "Part 2" =
  read_input "input"
  |> run_all_instructions
  |> draw
  |> render
  |> print_endline;
  [%expect {|
    ####.####.###..####.#..#..##..#..#.###..
    ...#.#....#..#.#....#..#.#..#.#..#.#..#.
    ..#..###..###..###..####.#....#..#.#..#.
    .#...#....#..#.#....#..#.#.##.#..#.###..
    #....#....#..#.#....#..#.#..#.#..#.#....
    ####.#....###..#....#..#..###..##..#.... |}]
