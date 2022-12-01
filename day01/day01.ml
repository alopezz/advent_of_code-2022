(* We'll represent individual elves as a list
 * with the calories on each food item
 *)
type elf = int list

(** Getting elves from textual input **)
let elves_of_lines_seq seq =
  Seq.fold_left
    (fun elves line ->
      match line with
      | "" -> [] :: elves
      | line  ->
         (match elves with
          | [] -> [[int_of_string line]]
          | cur_elf :: rest -> (int_of_string line :: cur_elf) :: rest)
    )
    []
    seq

let read_input filename =
  In_channel.with_open_text filename
    (fun ic ->
      Seq.forever (fun () -> In_channel.input_line ic)
      |> Seq.take_while Option.is_some
      |> Seq.map Option.get
      |> elves_of_lines_seq)

let%test_unit _ =
  let result = (elves_of_lines_seq (List.to_seq ["3000"; ""; "4000"; "5000"])) in
  [%test_eq: Base.int Base.list Base.list] result [[5000; 4000]; [3000]]


(** Puzzle solution **)

let max_int =
  List.fold_left
    (fun max cur -> if max > cur then max else cur)
    0

let sum_int = List.fold_left (+) 0

let most_calories elves =
  List.map sum_int elves
  |> max_int

let%test_unit "most calories, example" =
  let elves = [[1000;
                2000;
                3000];
               [4000];
               [5000;
                6000];
               [7000;
                8000;
                9000];
               [10000]]
  in
  let result = most_calories elves in
  [%test_eq: Base.int] result 24000


(* Inverts the comparator to sort in descending order *)
let sort_elves = List.sort (fun a b -> - Int.compare a b)

let top_n n elves =
  List.map sum_int elves
  |> sort_elves |> List.to_seq
  |> Seq.take n |> List.of_seq

let top_three = top_n 3

let%test_unit "top three, example" =
  let elves = [[1000;
                2000;
                3000];
               [4000];
               [5000;
                6000];
               [7000;
                8000;
                9000];
               [10000]]
  in
  let result = top_three elves |> sum_int in
  [%test_eq: Base.int] result 45000


let%expect_test "part1" =
  read_input "input"
  |> most_calories
  |> print_int;
  [%expect {| 71124 |}]

let%expect_test "part2" =
  read_input "input"
  |> top_three
  |> sum_int
  |> print_int;
  [%expect {| 204639 |}]
