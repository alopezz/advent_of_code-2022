(* For storing positions *)
module IntPairs =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        0 -> Stdlib.compare y0 y1
      | c -> c
  end

module CoordSet = Set.Make(IntPairs)


type rock = {
    position: IntPairs.t;
    shape: CoordSet.t
  }

(* For this puzzle, coords will be as (x, y) starting from bottom left (from 0) *)
(* A rock's position also uses its bottom left corner as the reference point *)

(* The infinite sequence of rocks coming down *)
let rocks =
  [
    [(0, 0); (1, 0); (2, 0); (3, 0)];
    [(0, 1); (1, 0); (1, 1); (1, 2); (2, 1)];
    [(0, 0); (1, 0); (2, 0); (2, 1); (2, 2)];
    [(0, 0); (0, 1); (0, 2); (0, 3)];    
    [(0, 0); (1, 0); (0, 1); (1, 1)]
  ]
  |> List.map CoordSet.of_list
  |> List.to_seq
  |> Seq.cycle


(* Returns the set of coordinates that a rock is actually occupying *)
let translate_rock {position = (x, y); shape} =
  CoordSet.map (fun (x1, y1) -> (x1 + x, y1 + y)) shape

(* Returns whether a given rock would collide with either the chamber
   walls or another rock in the chamber *)
let collides rock chamber =
  let shape = translate_rock rock in
  let (x, y) = rock.position in
  x < 0 || y < 0
  || CoordSet.exists (fun (x, _) -> x >= 7) shape
  || not (CoordSet.disjoint shape chamber)


type state =
  {
    chamber: CoordSet.t;
    rock_seq: CoordSet.t Seq.t;
    current_rock: rock;
    settled_rocks: int
  }

let top_of chamber =
  chamber
  |> CoordSet.to_seq
  |> Seq.map (fun (_x, y) -> y + 1)
  |> Seq.fold_left Int.max 0

let spawn_rock rock_seq chamber =
  let (r, rock_seq) = Seq.uncons rock_seq |> Option.get in
  ({position = (2, (top_of chamber) + 3); shape = r}, rock_seq)


let jet_move ({position = (x, y); _} as rock) jet =
  match jet with
  | '>' -> {rock with position = (x + 1, y)}
  | '<' -> {rock with position = (x - 1, y)}
  | _ -> failwith "Bad input"

let fall ({position = (x, y); _} as rock) =
  {rock with position = (x, y - 1)}


let neighbors_of (x, y) =
  [(x - 1, y); (x + 1, y); (x, y - 1)]

let flood_fill chamber =
  let rec fill known_border visited current =
    if CoordSet.is_empty current
    then known_border
    else
      (* Update state *)
       CoordSet.fold
         (fun ((x, y) as cur) (known_border, visited, candidates) ->
           (* Hit a chamber wall *)
           if (x < 0 || x >= 7 || y < 0 || CoordSet.mem cur visited)
           then (known_border, visited, candidates)
           (* Hit a settled rock, we register that as border *)               
           else if (CoordSet.mem cur chamber)
           then (CoordSet.add cur known_border, CoordSet.add cur visited, candidates)
           (* Otherwise, this is a point to keep exploring further from *)
           else (known_border, CoordSet.add cur visited, CoordSet.add cur candidates)
         )
         current
         (known_border, visited, CoordSet.empty)
       |> fun (known_border, visited, current) ->
          (* Compute candidates for next iteration *)
          let next =
            CoordSet.fold
              (fun c acc -> CoordSet.add_seq (neighbors_of c |> List.to_seq) acc)
              current
              CoordSet.empty
          in
          fill known_border visited next
  in
  fill CoordSet.empty CoordSet.empty (CoordSet.of_list [(0, top_of chamber)])


let update_chamber new_rock chamber =
  CoordSet.union chamber (translate_rock new_rock)
  (* Idea: Using a flood filling algorithm to fill the chamber and
     only keep the rocks that are neighbours of the flood filled
     area. *)
  |> flood_fill

(* This function returns a sequence of game states *)
let play_tetris jet =
  (* Spawn initial rock *)
  let chamber = CoordSet.empty in
  let (current_rock, rock_seq) = spawn_rock rocks (CoordSet.empty) in
  
  String.to_seq jet
  |> Seq.cycle
  |> Seq.scan
       (fun ({chamber; rock_seq; current_rock; settled_rocks} as state) ins ->
         (* Pushed by jet if possible *)
         let current_rock =
           if collides (jet_move current_rock ins) chamber
           then current_rock
           else (jet_move current_rock ins)
         in

         (* Fall down *)
         if collides (fall current_rock) chamber
         then
           (
             (* We've touched ground, we add the rock to the chamber and spawn a new one *)
             let chamber = update_chamber current_rock chamber in
             let (current_rock, rock_seq) = spawn_rock rock_seq chamber in
             {
               chamber;
               rock_seq;
               current_rock;
               settled_rocks = settled_rocks + 1
             }
           )
         else
           {state with current_rock = (fall current_rock)}
       )
       {chamber; rock_seq; current_rock; settled_rocks = 0}
  

let solve_part1 jet =
  play_tetris jet
  |> Seq.drop_while (fun {settled_rocks; _} -> settled_rocks < 2022)
  |> Seq.uncons
  |> Option.get
  |> fun (hd, _) -> hd.chamber
  |> top_of

let%test_unit "part 1, example" =
  let jet = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" in
  solve_part1 jet
  |> [%test_eq: Base.int] 3068

let%expect_test "part 1, actual input" =
  let jet = In_channel.with_open_text "input" In_channel.input_line |> Option.get in
  solve_part1 jet
  |> print_int;
  [%expect {| 3119 |}]


let solve_part2 jet =
  play_tetris jet
  |> Seq.drop_while (fun {settled_rocks; _} -> settled_rocks < 1000000000000)
  |> Seq.uncons
  |> Option.get
  |> fun (hd, _) -> hd.chamber
  |> top_of

(* let%test_unit "part 2, example" = *)
(*   let jet = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" in *)
(*   solve_part2 jet *)
(*   |> [%test_eq: Base.int] 1514285714288 *)

let%expect_test "part 2, actual input" =
  let jet = In_channel.with_open_text "input" In_channel.input_line |> Option.get in
  solve_part2 jet
  |> print_int


let render_state {chamber; current_rock; settled_rocks; _} =
  let n_rows = Int.max (top_of chamber) (top_of (translate_rock current_rock)) in
  let arr = Array.make_matrix n_rows 7 '.' in
  CoordSet.iter (fun (x, y) -> arr.(y).(x) <- '#' ) chamber;
  CoordSet.iter (fun (x, y) -> arr.(y).(x) <- '@' ) (translate_rock current_rock);
  Array.to_list arr
  |> List.map (fun arr ->
         Array.to_seq arr |> String.of_seq
         |> Printf.sprintf "|%s|")
  |> (fun x ->
    (Printf.sprintf "settled: %d\n" settled_rocks) ::
      (Printf.sprintf "rocks in chamber: %d" (CoordSet.cardinal chamber)) ::
        "+-------+" :: x
  )
  |> List.rev
  |> String.concat "\n"

            
