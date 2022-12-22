open Parser_types

let parse parser s =
  Lexing.from_string s |> parser Lexer.token

let parse_notes s =
  parse Parser.notes s

type link =
  [`Same of (int * int)
  | `Left of (int * int)
  | `Right of (int * int)
  | `Up of (int * int)
  | `Down of (int * int)]

type node = {
  left: link option;
  right: link option;
  up: link option;
  down: link option
  }

let make_wrapping_node (y, x) map =
  let n_rows = Array.length map in
  let n_cols = Array.map Array.length map |> Array.fold_left Int.max 0 in

  let rec find_next (y, x) move =
    let (y, x) = move (y, x) in

    (* Wrap around edges *)
    let y = Int.rem (n_rows + y) n_rows in
    let x = Int.rem (n_cols + x) n_cols in

    match map.(y).(x) with
    | Wall -> None
    | Empty -> find_next (y, x) move
    | Open -> Some (`Same (y, x))
    | exception (Invalid_argument _) -> find_next (y, x) move
  in

  match map.(y).(x) with
  | Wall -> None
  | Empty -> None
  | Open ->
     Some {
         left = find_next (y, x) (fun (y, x) -> (y, x - 1));
         right = find_next (y, x) (fun (y, x) -> (y, x + 1));
         up = find_next (y, x) (fun (y, x) -> (y - 1, x));
         down = find_next (y, x) (fun (y, x) -> (y + 1, x));
       }
  | exception (Invalid_argument _) -> None

let graph_of_map make_node map =
  let n_rows = Array.length map in
  let n_cols = Array.map Array.length map |> Array.fold_left Int.max 0 in

  let table = Hashtbl.create (n_rows * n_cols) in

  for y = 0 to n_rows - 1 do
    for x = 0 to n_cols - 1 do
      match make_node (y, x) map with
      | Some node -> Hashtbl.add table (y, x) node
      | None -> ()
    done
  done;
  table

type facing =
  | Left
  | Right
  | Up
  | Down

type player = {
    pos: (int * int);
    facing: facing
  }

let rec move graph d ({pos = (y, x); facing} as player) =
  if d = 0 then player
  else
    let node = Hashtbl.find graph (y, x) in
    let next_position =
      match facing with
      | Left -> node.left
      | Right -> node.right
      | Up -> node.up
      | Down -> node.down
    in
    match next_position with
    | None -> player
    | Some (`Same pos) -> move graph (d - 1) {player with pos}
    | Some (`Left pos) -> move graph (d - 1) {pos; facing = Left}
    | Some (`Right pos) -> move graph (d - 1) {pos; facing = Right}
    | Some (`Up pos) -> move graph (d - 1) {pos; facing = Up}
    | Some (`Down pos) -> move graph (d - 1) {pos; facing = Down}

let turn_left ({facing; _} as player) =
  {player with
    facing = match facing with
             | Left -> Down
             | Right -> Up
             | Up -> Left
             | Down -> Right
  }

let turn_right ({facing; _} as player) =
  {player with
    facing = match facing with
             | Left -> Up
             | Right -> Down
             | Up -> Right
             | Down -> Left
  }

let score_facing = function
  | Right -> 0
  | Down -> 1
  | Left -> 2
  | Up -> 3

let play build_graph map instructions =
  let initial_pos = (
      0,
      Array.to_list map
      |> List.hd
      |> Array.to_list
      |> List.mapi (fun idx elt -> (idx, elt))
      |> List.find_map (fun (idx, elt) -> match elt with
                                          | Open -> Some idx
                                          | _ -> None)
      |> Option.get
    )
  in

  let graph = build_graph map in
  List.fold_left
    (fun player ins ->
      match ins with
      | Move d -> move graph d player
      | TurnLeft -> turn_left player
      | TurnRight -> turn_right player
    )
    {pos = initial_pos; facing = Right}
    instructions

let score_player {pos = (y, x); facing} =
  let (row, col) = (y + 1, x + 1) in
  1000 * row + 4 * col + score_facing facing

let solve_part1 (map, instructions) =
  play (graph_of_map make_wrapping_node) map instructions |> score_player

let%test_unit "part 1 example" =
  In_channel.with_open_text "example_input" In_channel.input_all
  |> parse_notes
  |> solve_part1
  |> [%test_eq: Base.int] 6032

let%expect_test "part 1" =
  In_channel.with_open_text "input" In_channel.input_all
  |> parse_notes
  |> solve_part1
  |> print_int;
  [%expect {| 58248 |}]

(* Difference from part 2 is in how we build the graph (specifically, how we wrap at the edges) *)
let make_cube_node (y, x) map =
  let n_rows = Array.length map in
  let n_cols = Array.map Array.length map |> Array.fold_left Int.max 0 in

  let face_width = 50 in
  let face_height = 50 in

  let rec find_next (y0, x0) move =
    let (y, x) = move (y0, x0) in

    (* Wrap around edges
       This only works for my input's shape, the solution is not general,
       as the individual cases have been manually derived.
     *)
    let h_face x = x / face_width in
    let v_face y = y / face_height in
    let crossed_right x = x >= n_cols in
    let crossed_left x = x < 0 in
    let crossed_top y = y < 0 in
    let crossed_bottom y = y >= n_rows in
    let leftmost_of face_n = face_n * face_width in
    let rightmost_of face_n = (face_n + 1) * face_width - 1 in
    let top_of face_n = face_n * face_height in
    let bottom_of face_n = (face_n + 1) * face_height - 1 in
    let rel_x x = Int.rem x face_width in
    let rel_y y = Int.rem y face_height in

    let yface = function
      | 1 | 2 -> 0
      | 3 -> 1
      | 4 | 5 -> 2
      | 6 -> 3
      | _ -> failwith "Bad face number"
    in
    let xface = function
      | 4 | 6 -> 0
      | 1 | 3 | 5 -> 1
      | 2 -> 2
      | _ -> failwith "Bad face number"
    in

    let leftmost_of n = leftmost_of (xface n) in
    let rightmost_of n = rightmost_of (xface n) in
    let top_of n = top_of (yface n) in
    let bottom_of n = bottom_of (yface n) in

    let node =
      match (y, x) with
      (* [1-6] Walking up from 1, means going right from leftmost edge of 6 *)
      | (y, x) when crossed_top y && h_face x = 1 -> `Right (top_of 6 + rel_x x, leftmost_of 6)
      (* [2-1] Walking up from 2, means going up from bottom of 6 *)
      | (y, x) when crossed_top y && h_face x = 2 -> `Up (bottom_of 6, leftmost_of 6 + rel_x x)
      (* [1-4] Walking left from 1, means going right from the leftmost edge of 4 *)
      | (y, x) when v_face y = 0 && crossed_left x -> `Right (bottom_of 4 - rel_y y, leftmost_of 4)
      (* [2-5] Walking right from 2, means going left from the rightmost edge of 5 *)
      | (y, x) when v_face y = 0 && crossed_right x -> `Left (bottom_of 5 - rel_y y, rightmost_of 5)
      (* [3-4] Walking left from 3, means going down from the top of 4 *)
      | (y, x) when v_face y = 1 && crossed_left x -> `Down (top_of 4, leftmost_of 4 + rel_y y)
      (* [3-2] Walking right from 3, means going up from the bottom of 2 *)
      | (y, x) when v_face y = 1 && crossed_right x -> `Up (bottom_of 2, leftmost_of 2 + rel_y y)
      (* [4-3] Walking up from 4, means going right at the leftmost edge of 3 *)
      | (y, x) when crossed_top y && h_face x = 0 -> `Right (top_of 3 + rel_x x, leftmost_of 3)
      (* Walking left from 4, means going right at the leftmost edge of 1 *)
      | (y, x) when v_face y = 2 && crossed_left x -> `Right (bottom_of 1 - rel_y y, leftmost_of 1)
      (* [5-2] Walking right from 5, means going left from the rightmost edge of 2 *)
      | (y, x) when v_face y = 2 && crossed_right x -> `Left (bottom_of 2 - rel_y y, rightmost_of 2)
      (* Walking left from 6, means going down at the top of 1 *)
      | (y, x) when v_face y = 3 && crossed_left x -> `Down (top_of 1, leftmost_of 1 + rel_y y)
      (* Walking right from 6, means going up at the bottom of 5 *)
      | (y, x) when v_face y = 3 && crossed_right x -> `Up (bottom_of 5, leftmost_of 5 + rel_y y)
      (* [6-2] Walking down from 6, means going down from the top of 2 *)
      | (y, x) when crossed_bottom y && h_face x = 0 -> `Down (top_of 2, leftmost_of 2 + rel_x x)
      (* Walking down from 5, means going left from the rightmost edge of 6 *)
      | (y, x) when crossed_bottom y && h_face x = 1 -> `Left (top_of 6 + rel_x x, rightmost_of 6)
      (* [2-3] Walking down from 2, means going left from the rightmost edge of 3 *)
      | (y, x) when crossed_bottom y && h_face x = 2 -> `Left (top_of 3 + rel_x x, rightmost_of 3)
      | _ -> `Same (y, x)
    in

    let (y, x) =
      match node with
      | `Same (y, x) | `Right (y, x) | `Left (y, x) | `Up (y, x) | `Down (y, x) -> (y, x)
    in

    match map.(y).(x) with
    | Wall -> None
    | Empty -> find_next (y, x) move
    | Open -> Some node
    | exception (Invalid_argument _) -> find_next (y, x) move
  in

  match map.(y).(x) with
  | Wall -> None
  | Empty -> None
  | Open ->
     Some {
         left = find_next (y, x) (fun (y, x) -> (y, x - 1));
         right = find_next (y, x) (fun (y, x) -> (y, x + 1));
         up = find_next (y, x) (fun (y, x) -> (y - 1, x));
         down = find_next (y, x) (fun (y, x) -> (y + 1, x));
       }
  | exception (Invalid_argument _) -> None

let solve_part2 (map, instructions) =
  play (graph_of_map make_cube_node) map instructions |> score_player

(* let%test_unit "part 2 example" = *)
(*   In_channel.with_open_text "example_input" In_channel.input_all *)
(*   |> parse_notes *)
(*   |> solve_part2 *)
(*   |> [%test_eq: Base.int] 5031 *)
let%expect_test "part 2" =
  In_channel.with_open_text "input" In_channel.input_all
  |> parse_notes
  |> solve_part2
  |> print_int;
  [%expect {| 179091 |}]
