let rec ( ** ) base exp =
  match exp with
  | 0 -> 1
  | n -> base * (base ** (n - 1))

let int_of_snafu_digit = function
  | '1' -> 1
  | '2' -> 2
  | '0' -> 0
  | '-' -> -1
  | '=' -> -2
  | _ -> raise (Invalid_argument "invalid snafu digit")

let int_of_snafu snafu =
  String.to_seq snafu
  |> List.of_seq
  |> List.rev
  |> List.mapi (fun idx digit -> (int_of_snafu_digit digit) * (5 ** idx))
  |> List.fold_left (+) 0


let%test_unit "convert from SNAFU to decimal" =
  [("1", 1);
   ("2", 2);
   ("1=", 3);
   ("1-", 4);
   ("10", 5);
   ("11", 6);
   ("12", 7);
   ("2=", 8);
   ("2-", 9);
   ("20", 10);
   ("1=0", 15);
   ("1-0", 20);
   ("1=11-2", 2022);
   ("1-0---0", 12345);
   ("1121-1110-1=0", 314159265)]
  |> List.iter (fun (snafu, dec) -> [%test_eq: Base.int] dec (int_of_snafu snafu))


let parse s =
  String.split_on_char '\n' s
  |> List.filter (fun s -> String.length s > 0)


let snafu_digit_of_int = function
  | -2 -> '='
  | -1 -> '-'
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | _ -> raise (Invalid_argument "int is out of [-2, 2] range")


let snafu_of_int n =
  let rec loop_digit i n digits =
    if n = 0 then digits
    else
      let rem = Int.rem n (5 ** (i + 1)) in
      let digit_rem = rem / (5 ** i) in
      let digit_rem = if digit_rem > 2 then digit_rem - 5 else digit_rem in
      let final_rem = digit_rem * (5 ** i) in
    loop_digit (i + 1) (n - final_rem) @@ snafu_digit_of_int digit_rem::digits
  in
  loop_digit 0 n [] |> List.to_seq |> String.of_seq


let solve_part1 snafu_numbers =
  List.map int_of_snafu snafu_numbers
  |> List.fold_left (+) 0
  |> snafu_of_int

let%test_unit "example" =
  let input = "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122
" |> parse
  in
  [%test_eq: Base.string] "2=-1=0" (solve_part1 input)

let%expect_test "solve with input" =
  In_channel.(with_open_text "input" input_all)
  |> parse
  |> solve_part1
  |> print_string
