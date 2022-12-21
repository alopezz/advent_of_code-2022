let parse parser s =
  Lexing.from_string s |> parser Monkey_lexer.token

let parse_monkey s =
  parse Monkey_parser.monkey s

let%test "parse monkey" =
  parse_monkey "cczh: sllz + lgvd" = ("cczh", `Sum ("sllz", "lgvd"))

let%test "parse monkey" =
  parse_monkey "zczc: 2" = ("zczc", `Value 2)

let parse_all s =
  String.split_on_char '\n' s
  |> List.filter (fun s -> String.length s > 0)
  |> List.map parse_monkey

let example () =
  "
   root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
"
  |> parse_all


let monkey_op op construct =
  (fun a b ->
    match (a, b) with
    | (`Value a, `Value b) -> `Value (op a b)
    | (a, b) -> construct (a, b))

let (-*-) a b = monkey_op ( * ) (fun x -> `Mul x) a b
let (-/-) a b = monkey_op (/) (fun x -> `Div x) a b
let (-+-) a b = monkey_op (+) (fun x -> `Sum x) a b
let (---) a b = monkey_op (-) (fun x -> `Sub x) a b


let rec eval monkey_table monkey_name =
  (* Memoize resolved values to avoid recomputation *)
  let memoize value =
    (match value with
     | `Value x -> Hashtbl.replace monkey_table monkey_name (`Value x)
     | _ -> ()
    );
    value
  in

  let monkey = Hashtbl.find monkey_table monkey_name in
  let eval = eval monkey_table in
  (match monkey with
   | `Value x -> `Value x
   | `Human -> `Human
   | `Mul (a, b) -> (eval a) -*- (eval b)
   | `Div (a, b) -> (eval a) -/- (eval b)
   | `Sum (a, b) -> (eval a) -+- (eval b)
   | `Sub (a, b) -> (eval a) --- (eval b)
  ) |> memoize


let solve_part1 monkeys =
  (* Build a hashtable with all the monkeys *)
  let h = Hashtbl.of_seq @@ List.to_seq monkeys in

  match eval h "root" with
  | `Value x -> x
  | _ -> failwith "The answer can't be known"


let%test_unit "part 1 example" =
  [%test_eq: Base.int] (solve_part1 @@ example ()) 152

let%expect_test "part 1" =
  In_channel.with_open_text "input" In_channel.input_all
  |> parse_all
  |> solve_part1
  |> print_int;
  [%expect {| 80326079210554 |}]


(*
  Rules for part 2: the root monkey's operation must be replaced by a
  comparison equality (which we want to make true).
  We need to find out the right value for [humn] (instead of the given value)
 *)
let solve_part2 monkeys =
  (* Build a hashtable with all the monkeys *)
  let h = Hashtbl.of_seq @@ List.to_seq monkeys in

  (* Replace the human value with an unknown *)
  Hashtbl.replace h "humn" `Human;

  (* Evaluate both sides of the "root" equality *)
  let (a, b) =
    match Hashtbl.find h "root" with
    | `Mul (a, b) | `Div (a, b) | `Sum (a, b) | `Sub (a, b) -> (eval h a, eval h b)
    | _ -> failwith "Invalid"
  in

  (* Isolate the variable for `Human *)
  let rec solve a b =
    match (a, b) with
    | (`Value a, `Human) -> a
    | (`Value a, `Mul (rhs, `Value c)) | (`Value a, `Mul (`Value c, rhs)) -> solve (`Value (a / c)) rhs
    | (`Value a, `Sum (rhs, `Value c)) | (`Value a, `Sum (`Value c, rhs)) -> solve (`Value (a - c)) rhs
    | (`Value a, `Sub (rhs, `Value c)) -> solve (`Value (a + c)) rhs
    | (`Value a, `Sub (`Value c, rhs)) -> solve (`Value (c - a)) rhs
    | (`Value a, `Div (rhs, `Value c)) -> solve (`Value (a * c)) rhs
    | (`Value a, `Div (`Value c, rhs)) -> solve (`Value (c / a)) rhs
    | (x, `Value a) -> solve (`Value a) x
    | (_a, _b) -> failwith "Something went wrong on the evaluation stage"

    in solve a b

let%test_unit "part 2 example" =
  [%test_eq: Base.int] (solve_part2 @@ example ()) 301

let%expect_test "part 2" =
  In_channel.with_open_text "input" In_channel.input_all
  |> parse_all
  |> solve_part2
  |> print_int;
  [%expect {| 3617613952378 |}]
