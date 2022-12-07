
module String = struct
  (* Extend [String] with a few extra helpers *)
  include String

  let get_opt s idx =
    try
      Some s.[idx]
    with
      Invalid_argument _ -> None

  let is_empty s =
    (String.length s) = 0
end


let parse_crates s =
  let lines = String.split_on_char '\n' s |> List.rev in
  let max_line_length = List.map String.length lines |> List.fold_left Int.max 0
  in
  let n_stacks = (max_line_length + 1) / 4 in
  let stacks = Array.make n_stacks [] in

  List.iter
    (fun line ->
      List.iter
        (fun idx ->
          match String.get_opt line (4*idx + 1) with
          | None | Some ' ' -> ()
          | Some c -> stacks.(idx) <- c :: stacks.(idx)
        )
        (List.init n_stacks Fun.id)
    )
    lines;
  stacks

let%test_unit "parsing crates" =
  let result = parse_crates "    [D]    
[N] [C]    
[Z] [M] [P]"
  in
  [%test_eq: Base.char Base.list Base.array] result [|['N'; 'Z']; ['D'; 'C'; 'M']; ['P']|]


let parse_instruction s =
  Scanf.sscanf s "move %d from %d to %d"
    (fun count src dst -> (count, src, dst))


let read_input filename =
  In_channel.with_open_text filename
    (fun ic ->
      (* The use of [input_seq] here relies on side effects, acting as
         an iterator over the lines of the input *)
      let input_seq = Seq.of_dispenser (fun () -> In_channel.input_line ic) in
      (* Read initial crate setup *)
      let stacks =
        input_seq
        |> Seq.take_while (Fun.negate (String.starts_with ~prefix:" 1 "))
        |> List.of_seq
        |> String.concat "\n"
        |> parse_crates
      in
      (* Consume line before beginning of instructions *)
      let _ = Seq.uncons input_seq in
      (* Read instructions *)
      let instructions =
        input_seq
        |> Seq.take_while (Fun.negate String.is_empty)
        |> Seq.map parse_instruction
        |> List.of_seq
      in

     (stacks, instructions))


(* List helpers *)
let take n lst =
  List.to_seq lst |> Seq.take n |> List.of_seq

let drop n lst =
  List.to_seq lst |> Seq.drop n |> List.of_seq


(* Modifies [stacks] in-place, moving [count] from [src] to [dst]
   (1-indexed), and applying the reordering function [order_crates] to
   the moved crates. *)
let move_crates order_crates stacks (count, src, dst) =
  stacks.(dst - 1) <- (take count stacks.(src - 1) |> order_crates) @ stacks.(dst - 1);
  stacks.(src - 1) <- drop count stacks.(src - 1)

(* Returns a string joining the top crates of all stacks *)
let collect_top_crates stacks =
  stacks
  |> Array.to_seq
  |> Seq.map
       (function
         | hd :: _ -> hd
         | _ -> ' ')
  |> String.of_seq


let%expect_test "Puzzle part 1" =
  let (stacks, instructions) = read_input "input" in
  instructions
  |> List.iter (move_crates List.rev stacks);
  collect_top_crates stacks
  |> print_endline;
  [%expect {| VWLCWGSDQ |}]


let%expect_test "Puzzle part 2" =
  let (stacks, instructions) = read_input "input" in
  instructions
  |> List.iter (move_crates Fun.id stacks);
  collect_top_crates stacks
  |> print_endline;
  [%expect {| TCGLQSLPW |}]
