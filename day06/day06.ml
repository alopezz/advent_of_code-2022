
module CharSet = Set.Make(Char)


let are_all_different s =
  String.length s = (String.to_seq s |> CharSet.of_seq |> CharSet.cardinal)


(* Finds the position of the first unique sequence of characters of length [size] in string [input_string] *)
let find_unique_sequence size input_string =
  let len = String.length input_string in
  Seq.ints 0 |> Seq.take_while (fun idx -> idx < len - size - 1)
  |> Seq.find_map
       (fun n ->
         if String.sub input_string n size |> are_all_different
         then Some (n + size)
         else None
       )
  |> Option.get

let find_start_of_packet = find_unique_sequence 4

let%test_unit "find start of packet, example 1" = [%test_eq: Base.int] (find_start_of_packet "bvwbjplbgvbhsrlpgdmjqwftvncz") 5
let%test_unit "find start of packet, example 2" = [%test_eq: Base.int] (find_start_of_packet "nppdvjthqldpwncqszvftbrmjlhg") 6
let%test_unit "find start of packet, example 3" = [%test_eq: Base.int] (find_start_of_packet "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") 10
let%test_unit "find start of packet, example 4" = [%test_eq: Base.int] (find_start_of_packet "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") 11


let find_start_of_message = find_unique_sequence 14

let%test_unit "find start of message, example 1" = [%test_eq: Base.int] (find_start_of_message "mjqjpqmgbljsphdztnvjfqwrcgsmlb") 19
let%test_unit "find start of message, example 2" = [%test_eq: Base.int] (find_start_of_message "bvwbjplbgvbhsrlpgdmjqwftvncz") 23
let%test_unit "find start of message, example 3" = [%test_eq: Base.int] (find_start_of_message "nppdvjthqldpwncqszvftbrmjlhg") 23
let%test_unit "find start of message, example 4" = [%test_eq: Base.int] (find_start_of_message "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") 29
let%test_unit "find start of message, example 4" = [%test_eq: Base.int] (find_start_of_message "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") 26


let read_input filename =
  In_channel.with_open_text filename
    (fun ic -> In_channel.input_all ic)

let%expect_test "Part 1" =
  read_input "input"
  |> find_start_of_packet
  |> print_int;
  [%expect {| 1816 |}]

let%expect_test "Part 2" =
  read_input "input"
  |> find_start_of_message
  |> print_int;
  [%expect {| 2625 |}]
