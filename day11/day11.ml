type throw = {worry_level: int; target: int}

type monkey =
  {
    items: int Queue.t;
    op: (int -> int);
    test: (int -> throw);
    divisor: int;
    mutable inspections: int
  }

let make_monkey starting op test_divisor if_true if_false =
  {
    items = starting |> List.to_seq |> Queue.of_seq;
    op;
    test = (fun x ->
      if (Int.rem x test_divisor) = 0
      then {worry_level = x; target = if_true}
      else {worry_level = x; target = if_false}
    );
    divisor = test_divisor;
    inspections = 0
  }

let example_monkeys () = [|
    make_monkey [79; 98] (fun old -> old * 19) 23 2 3;
    make_monkey [54;65; 75; 74] (fun old -> old + 6) 19 2 0;
    make_monkey [79; 60; 97] (fun old -> old * old) 13 1 3;
    make_monkey [74] (fun old -> old + 3) 17 0 1;
  |]


(* Update [target] monkey's queue by adding new [worry_level] value. *)
let throw monkeys {worry_level; target} =
  Queue.add worry_level monkeys.(target).items

(* Process all items in a monkey's queue.
   Mutates [monkey] (its queue and inspections) and another monkey
   from [monkeys] (the target monkey's queue). *)
let monkey_do monkeys control_worry monkey =
  while not (Queue.is_empty monkey.items) do
    monkey.inspections <- monkey.inspections + 1;
    Queue.take monkey.items
    |> monkey.op
    |> control_worry
    |> monkey.test
    |> throw monkeys
  done

let do_round monkeys control_worry =
  Array.iter (monkey_do monkeys control_worry) monkeys

let do_rounds monkeys n control_worry =
  for _i = 1 to n do
    do_round monkeys control_worry
  done

let solve n control_worry monkeys =
  do_rounds monkeys n control_worry;

  Array.to_list monkeys
  |> List.map (fun m -> m.inspections)
  |> List.sort (fun a b -> - Int.compare a b)
  |> function a :: b :: _ -> a * b | _ -> 0

let solve_part1 = solve 20 (fun w -> (float_of_int w) /. 3. |> Float.trunc |> int_of_float)

let%test_unit "Example, part 1" =
  [%test_eq: Base.int] (example_monkeys () |> solve_part1) 10605


let input_monkeys () = [|
    make_monkey [78; 53; 89; 51; 52; 59; 58; 85] (fun old -> old * 3) 5 2 7;
    make_monkey [64] (fun old -> old + 7) 2 3 6;
    make_monkey [71; 93; 65; 82] (fun old -> old + 5) 13 5 4;
    make_monkey [67; 73; 95; 75; 56; 74] (fun old -> old + 8) 19 6 0;
    make_monkey [85; 91; 90] (fun old -> old + 4) 11 3 1;
    make_monkey [67; 96; 69; 55; 70; 83; 62] (fun old -> old * 2) 3 4 1;
    make_monkey [53; 86; 98; 70; 64] (fun old -> old + 6) 7 7 0;
    make_monkey [88; 64] (fun old -> old * old) 17 2 5
  |]

let%expect_test "Part 1" =
  input_monkeys () |> solve_part1 |> print_int;
  [%expect {| 50616 |}]

let solve_part2 monkeys =
  (* This is the main trick for part 2: we need to keep the worry
     level low to avoid integer overflow, while keeping all the
     monkeys' test functions equivalent. *)
  let divisor = Array.fold_left (fun a m -> a * m.divisor) 1 monkeys in
  solve 10000 (fun w -> Int.rem w divisor) monkeys

let%test_unit "Example, part 2" =
  [%test_eq: Base.int] (example_monkeys () |> solve_part2) 2713310158

let%expect_test "Part 2" =
  input_monkeys () |> solve_part2 |> print_int;
  [%expect {| 11309046332 |}]
