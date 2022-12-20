(* An element in the sequence, with links to the next and previous elements *)
(* The use of option types for the links solves a chicken-and-egg
   problem, in that you need an existing element to link to be able to instantiate the type. *)
type element = {
    value: int;
    mutable prev: element option;
    mutable next: element option;
  }

let get_next elt =
  Option.get elt.next

let get_prev elt =
  Option.get elt.prev

(* Create linked list of basic int list *)
let linked_of_list lst =
  let linked = List.map (fun x -> {value = x; prev = None; next = None}) lst in
  let _ = List.fold_left (fun prev elt -> elt.prev <- Some prev; elt) (List.rev linked |> List.hd) linked in
  let _ = List.fold_left (fun next elt -> elt.next <- Some next; elt) (List.hd linked) (List.rev linked) in
  linked
  

let%test_unit "make linked list" =
  let linked = linked_of_list [1; 2; -3] in
  [%test_eq: Base.int] 1 (List.hd linked).value;
  [%test_eq: Base.int] 2 (List.hd linked |> get_next).value;
  [%test_eq: Base.int] (-3) (List.hd linked |> get_next |> get_next).value;
  [%test_eq: Base.int] 1 (List.hd linked |> get_next |> get_next |> get_next).value;
  [%test_eq: Base.int] (-3) (List.hd linked |> get_prev).value;
  [%test_eq: Base.int] 2 (List.hd linked |> get_prev |> get_prev).value

let linked_to_list linked =
  let n = List.length linked in
  let rec iter lst elt =
    if List.length lst = n then lst
    else iter (elt.value :: lst) (get_next elt)
  in
  iter [] (List.hd linked) |> List.rev

let rotate_left elt =
  let prev = (elt |> get_prev |> get_prev) in
  let next = (elt |> get_prev) in
  (elt |> get_prev |> get_prev).next <- Some elt;
  (elt |> get_prev).prev <- Some elt;
  (elt |> get_prev).next <- elt.next;
  (elt |> get_next).prev <- elt.prev;
  elt.prev <- Some prev;
  elt.next <- Some next

let rotate_right elt =
  let prev = (elt |> get_next) in
  let next = (elt |> get_next |> get_next) in
  (elt |> get_next |> get_next).prev <- Some elt;
  (elt |> get_next).next <- Some elt;
  (elt |> get_next).prev <- elt.prev;
  (elt |> get_prev).next <- elt.next;
  elt.prev <- Some prev;
  elt.next <- Some next

let move n elt =
  let rotate = if elt.value < 0 then rotate_left else rotate_right in
  for _ = 1 to Int.rem (Int.abs elt.value) (n - 1) do
    rotate elt
  done

let%test_unit "example movement" =
  let linked = linked_of_list [1; 2; -3; 3; -2; 0; 4] in
  move (List.length linked) (List.hd linked);

  [%test_eq: Base.int Base.list]
    [1; -3; 3; -2; 0; 4; 2]
    (linked_to_list linked)
    
let move_all lst =
  List.iter (move (List.length lst)) lst

let%test_unit "full example 1 sequence" =
  let linked = linked_of_list [1; 2; -3; 3; -2; 0; 4] in
  move_all linked;

  [%test_eq: Base.int Base.list]
    [1; 2; -3; 4; 0; 3; -2]
    (linked_to_list linked)

(* Infinite sequence starting from the first found 0 *)
let seq_of_linked linked =
  List.to_seq (linked_to_list linked) |> Seq.cycle |> Seq.drop_while (fun x -> x <> 0)

(* Assumes [seq] is infinite *)
let take_every n seq =
  Seq.unfold
       (fun seq ->
         let seq = Seq.drop n seq in
         let (hd, _) = Seq.uncons seq |> Option.get in
         Some (hd, seq)
       )
       seq

let solve decrypt lst =
  lst
  |> decrypt
  |> seq_of_linked
  |> take_every 1000
  |> Seq.take 3
  |> Seq.fold_left (+) 0

let solve_part1 =
  solve (fun lst ->
      let linked = linked_of_list lst in
      move_all linked;
      linked)

let%test_unit "example for part 1" =
  solve_part1 [1; 2; -3; 3; -2; 0; 4]
  |> [%test_eq: Base.int] 3

let%expect_test "solve part 1" =
  In_channel.with_open_text "input" In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s > 0)
  |> List.map (int_of_string)
  |> solve_part1
  |> print_int;
  [%expect {| 988 |}]

let solve_part2 =
  solve
    (fun lst ->
      let linked =
        List.map (( * ) 811589153) lst
        |> linked_of_list
      in
      for _ = 1 to 10 do
        move_all linked
      done;
      linked)

let%test_unit "example for part 2" =
  solve_part2 [1; 2; -3; 3; -2; 0; 4]
  |> [%test_eq: Base.int] 1623178306

let%expect_test "solve part 2" =
  In_channel.with_open_text "input" In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s > 0)
  |> List.map (int_of_string)
  |> solve_part2
  |> print_int;
  [%expect {| 7768531372516 |}]
