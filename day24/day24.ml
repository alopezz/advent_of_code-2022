(* For storing blizzards in a set *)

type blizzard =
  | Up of (int * int)
  | Down of (int * int)
  | Left of (int * int)
  | Right of (int * int)

module Blizzards =
  struct
    type t = blizzard
    let compare a b =
      match (a, b) with
      | (Up (x0, y0), Up (x1, y1))
        | (Down (x0, y0), Down (x1, y1))
        | (Left (x0, y0), Left (x1, y1))
        | (Right (x0, y0), Right (x1, y1)) ->
        (match Stdlib.compare x0 x1 with
           0 -> Stdlib.compare y0 y1
         | c -> c)
      | (Up _, _) -> -1
      | (_, Up _) -> 1
      | (Down _, _) -> -1
      | (_, Down _) -> 1
      | (Left _, _) -> -1
      | (_, Left _) -> 1
  end

module BlizzSet = Set.Make(Blizzards)

(* For storing and deduplicating positions *)
module IntPairs =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        0 -> Stdlib.compare y0 y1
      | c -> c
  end

module CoordSet = Set.Make(IntPairs)


(* We'll write coordinates as (y, x) with y growing from top to bottom *)

type map = {
    blizz: BlizzSet.t;
    target: int * int
  }

let to_tuple a b = (a, b)

let parse s =
  let lines =
    String.split_on_char '\n' s
    |> List.filter (fun s -> String.length s > 0)
  in
  let target = (List.length lines - 1, (String.length @@ List.hd lines) - 2) in
  let blizz =
  List.mapi to_tuple lines
  |> List.fold_left
       (fun blizz (y, line) ->
         String.to_seqi line
         |> Seq.fold_left
              (fun blizz (x, c) ->
                match c with
                | '^' -> BlizzSet.add (Up (y, x)) blizz
                | 'v' -> BlizzSet.add (Down (y, x)) blizz
                | '<' -> BlizzSet.add (Left (y, x)) blizz
                | '>' -> BlizzSet.add (Right (y, x)) blizz
                | _ -> blizz
              )
              blizz
       )
       BlizzSet.empty
  in
  {blizz; target}


let example =
  "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
" |> parse


let%test_unit "parsing" =
  [%test_eq: Base.int * Base.int] (example.target) (5, 6);
  [%test_pred: Base.int * Base.int] (fun (y, x) -> BlizzSet.mem (Right (y, x)) example.blizz) (3, 1);
  [%test_pred: Base.int * Base.int] (fun (y, x) -> not @@ BlizzSet.mem (Left (y, x)) example.blizz) (3, 1)


let bound {target = (y1, x1); _} (y, x) =
  let width = x1 in
  let height = y1 - 1 in
  ((Int.rem (y - 1 + 2*height) height) + 1,
   (Int.rem (x - 1 + 2*width) width) + 1)

let%test_unit "bound" =
  [%test_eq: Base.int * Base.int] (4, 1) (bound example (0, 1));
  [%test_eq: Base.int * Base.int] (1, 6) (bound example (1, 0));
  [%test_eq: Base.int * Base.int] (1, 1) (bound example (5, 1));
  [%test_eq: Base.int * Base.int] (1, 1) (bound example (1, 7))

let move_blizzard map =
  let bound = bound map in

  {map with
    blizz = BlizzSet.map
              (function
               | Up (y, x) -> Up (bound (y - 1, x))
               | Down (y, x) -> Down (bound (y + 1, x))
               | Left (y, x) -> Left (bound (y, x - 1))
               | Right (y, x) -> Right (bound (y, x + 1))
              )
              map.blizz
  }


let has_blizz pos blizz =
  BlizzSet.mem (Up pos) blizz
  || BlizzSet.mem (Down pos) blizz
  || BlizzSet.mem (Left pos) blizz
  || BlizzSet.mem (Right pos) blizz

let out_of_bounds {target = (y1, x1); _} (y0, x0) =
  y0 < 1 || x0 < 1 || y0 >= y1 || x0 > x1


let best_case_for {target = (y1, x1); _} (y0, x0)=
  Int.abs (y0 - y1) + Int.abs (x0 - x1)

let find_candidates ({target; blizz} as map) (y0, x0) =
  [(y0 - 1, x0); (y0 + 1, x0); (y0, x0); (y0, x0 - 1); (y0, x0 + 1)]
  |> List.filter
       (fun p ->
         p = target
         || p = (0, 1)
         || (not (has_blizz p blizz || out_of_bounds map p))
       )

let find_target start target map =
  let rec recur m map candidates =
    if CoordSet.exists (fun pos -> pos = target) candidates
    then (m, map)
    else
      let new_map = move_blizzard map in
      let new_candidates =
        CoordSet.fold
          (fun c cs -> CoordSet.add_seq (find_candidates new_map c |> List.to_seq) cs)
          candidates
          CoordSet.empty
      in recur (m + 1) new_map new_candidates
  in
  recur 0 map (CoordSet.of_list [start])

let solve_part1 map =
  let (m, _map) = find_target (0, 1) map.target map in m


let%test_unit "part 1 on example" =
  [%test_eq: Base.int] 18 (solve_part1 example)

let%expect_test "part 1 on input" =
  In_channel.with_open_text "input" In_channel.input_all
  |> parse
  |> solve_part1
  |> print_int;
  [%expect {| 264 |}]


let solve_part2 map =
  let (m1, map') = find_target (0, 1) map.target map in
  let (m2, map') = find_target map.target (0, 1) map' in
  let (m3, _map) = find_target (0, 1) map.target map' in
  m1 + m2 + m3

let%test_unit "part 2 on example" =
  [%test_eq: Base.int] 54 (solve_part2 example)

let%expect_test "part 2 on input" =
  In_channel.with_open_text "input" In_channel.input_all
  |> parse
  |> solve_part2
  |> print_int;
  [%expect {| 789 |}]
