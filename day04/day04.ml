type section = {start : int; end_ : int}


let parse_line line =
  Scanf.sscanf line "%d-%d,%d-%d"
    (fun a b x y ->
      ({start = a; end_ = b}, {start = x; end_ = y}))

let%test_unit "parsing works" =
  let a, b = parse_line "2-4,6-8" in
  [%test_eq: Base.int] a.start 2;
  [%test_eq: Base.int] a.end_ 4;
  [%test_eq: Base.int] b.start 6;
  [%test_eq: Base.int] b.end_ 8


let read_input filename =
  In_channel.with_open_text filename
    (fun ic ->
      Seq.of_dispenser (fun () -> In_channel.input_line ic)
      |> Seq.map parse_line
      |> List.of_seq)

let count predicate list =
  List.filter predicate list |> List.length

let fully_contained_in a b =
  a.start >= b.start && a.end_ <= b.end_

let pair_fully_overlaps (a, b) =
  fully_contained_in a b || fully_contained_in b a

let%test_unit "detect full overlaps" =
  let result = pair_fully_overlaps ({start = 2; end_ = 4}, {start = 1; end_ = 4}) in
  [%test_eq: Base.bool] result true;
  let result = pair_fully_overlaps ({start = 2; end_ = 4}, {start = 3; end_ = 5}) in
  [%test_eq: Base.bool] result false

let contained_in num range =
  num >= range.start && num <= range.end_

let pair_overlaps (a, b) =
  contained_in a.start b || contained_in b.start a || contained_in a.end_ b || contained_in b.end_ a

let%test_unit "detects any overlaps" =
  let result = pair_overlaps ({start = 2; end_ = 4}, {start = 1; end_ = 4}) in
  [%test_eq: Base.bool] result true;
  let result = pair_overlaps ({start = 2; end_ = 4}, {start = 3; end_ = 5}) in
  [%test_eq: Base.bool] result true;
  let result = pair_overlaps ({start = 3; end_ = 5}, {start = 2; end_ = 4}) in
  [%test_eq: Base.bool] result true;
  let result = pair_overlaps ({start = 5; end_ = 9}, {start = 2; end_ = 4}) in
  [%test_eq: Base.bool] result false

let count_fully_overlapping_pairs = count pair_fully_overlaps

let%test_unit "count overlaps fully" =
  let example_input = [
      "2-4,6-8";
      "2-3,4-5";
      "5-7,7-9";
      "2-8,3-7";
      "6-6,4-6";
      "2-6,4-8"
    ] in
  let result = List.map parse_line example_input |> count_fully_overlapping_pairs in
  [%test_eq: Base.int] result 2

let count_overlapping_pairs = count pair_overlaps

let%test_unit "count overlaps" =
  let example_input = [
      "2-4,6-8";
      "2-3,4-5";
      "5-7,7-9";
      "2-8,3-7";
      "6-6,4-6";
      "2-6,4-8"
    ] in
  let result = List.map parse_line example_input |> count_overlapping_pairs in
  [%test_eq: Base.int] result 4

let%expect_test "Part 1" =
  read_input "input"
  |> count_fully_overlapping_pairs
  |> print_int;
  [%expect {| 475 |}]

let%expect_test "Part 2" =
  read_input "input"
  |> count_overlapping_pairs
  |> print_int;
  [%expect {| 825 |}]
