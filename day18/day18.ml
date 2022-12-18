module SidesHash = struct
  type t = (int * int * int, bool) Hashtbl.t

  let create () =
    Hashtbl.create 6

  let add side h =
    Hashtbl.replace h side true

  let count h =
    6 - (Hashtbl.to_seq_values h |> Seq.filter Fun.id |> Seq.length)
end

module IntTrios =
  struct
    type t = int * int * int
    let compare (x0, y0, z0) (x1, y1, z1) =
      match Stdlib.compare x0 x1 with
      | 0 -> (match Stdlib.compare y0 y1 with
              | 0 -> compare z0 z1
              | c -> c)
      | c -> c
  end

module CoordSetBase = Set.Make(IntTrios)

module CoordSet = struct
  include CoordSetBase

  let pop set =
    match choose_opt set with
    | Some elt -> Some (elt, CoordSetBase.remove elt set)
    | None -> None
end


type block = {
    pos: int * int * int;
    sides: SidesHash.t
  }

let new_block pos = {
    pos;
    sides = SidesHash.create ()
  }

let (@-) {pos = (a0, b0, c0); _} {pos = (a1, b1, c1); _} =
  (a0 - a1, b0 - b1, c1 - c0)

let update_block other block =
  let diff = block @- other in
  match diff with
  | (-1, 0, 0)
    | (1, 0, 0)
    | (0, -1, 0)
    | (0, 1, 0)
    | (0, 0, -1)
    | (0, 0, 1)
    -> SidesHash.add diff block.sides
  | _ -> ()

let solve_part1 blocks =
  let rec iterate past_blocks rest =
    match rest with
    | [] -> ()
    | hd :: tl ->
       List.iter (fun b -> update_block hd b; update_block b hd) past_blocks;
       iterate (hd :: past_blocks) tl
  in

  iterate [] blocks;

  blocks
  |> List.map (fun b -> SidesHash.count b.sides)
  |> List.fold_left (+) 0


let%test_unit "solve part 1 for two cubes" =
  solve_part1 [new_block (1, 1, 1); new_block (2, 1, 1)]
  |> [%test_eq: Base.int] 10

let parse s =
  String.split_on_char '\n' s
  |> List.filter (fun x -> String.length x > 0)
  |> List.map
     (fun line -> Scanf.sscanf line "%d,%d,%d" (fun a b c -> new_block (a, b, c)))

let example () =
  "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
"
  |> parse


let%test_unit "solve part 1 for example" =
  solve_part1 (example ())
  |> [%test_eq: Base.int] 64

let%expect_test "solve part 1" =
  In_channel.with_open_text "input" In_channel.input_all
  |> parse
  |> solve_part1
  |> print_int;
  [%expect {| 3530 |}]


let x_of_block {pos = (x, _, _); _} = x
let y_of_block {pos = (_, y, _); _} = y
let z_of_block {pos = (_, _, z); _} = z

let range min max =
  (Seq.init (max + 1 - min) (fun x -> x + min))

let neighbors_of (x, y, z) =
  [(x, y, z - 1); (x, y, z + 1);
   (x, y - 1, z); (x, y + 1, z);
   (x - 1, y, z); (x + 1, y, z)]
  |> CoordSet.of_list

type region = Interior of CoordSet.t | Exterior

(* The solution for part 2 uses a flood fill approach to find interior
   regions and fill them with blocks, after which we can run the
   part 1 solver to get the expected result *)
let solve_part2 blocks =
  (* Find ranges *)
  let min_x, min_y, min_z = (List.hd blocks).pos in
  let min_x = List.fold_left Int.min min_x (List.map x_of_block blocks) in
  let min_y = List.fold_left Int.min min_y (List.map y_of_block blocks) in
  let min_z = List.fold_left Int.min min_z (List.map z_of_block blocks) in
  let max_x, max_y, max_z = (List.hd blocks).pos in
  let max_x = List.fold_left Int.max max_x (List.map x_of_block blocks) in
  let max_y = List.fold_left Int.max max_y (List.map y_of_block blocks) in
  let max_z = List.fold_left Int.max max_z (List.map z_of_block blocks) in

  let out_of_bounds (x, y, z) =
    x <= min_x || x >= max_x || y <= min_y || y >= max_y || z <= min_z || z >= max_z
  in

  let block_set = (CoordSet.of_list (List.map (fun {pos; _} -> pos) blocks)) in

  let rec fill acc current candidates =
    (* Ran out of candidates *)
    if (CoordSet.is_empty current) then
      match acc with
      | Exterior -> (CoordSet.empty, candidates)
      | Interior acc -> (acc, candidates)
    else
      (* Remove from candidate pool *)
      let candidates' = CoordSet.diff candidates current in
      (* Calculate points for next iteration *)
      let neighbors = CoordSet.fold (fun c acc -> neighbors_of c |> CoordSet.union acc) current CoordSet.empty in
      
      let acc = match acc with
        | Exterior -> Exterior
        | Interior acc ->
           if (CoordSet.exists (fun n -> out_of_bounds n && not (CoordSet.mem n block_set)) neighbors)
           (* We've reached the edges of the known world, this is an exterior region *)
           then Exterior
           else Interior (CoordSet.union acc current)
      in
      (* Keep exploring *)
      fill acc (CoordSet.inter neighbors candidates') candidates'
  in

  (* Generate candidates to start flood fill from *)
  let candidates =
    Seq.product (range min_x max_x) (range min_y max_y)
    |> Seq.map_product (fun z (x, y) -> (x, y, z)) (range min_z max_z)
    |> CoordSet.of_seq
    |> (Fun.flip CoordSet.diff) block_set
  in

  let rec repeatedly_fill block_set candidates =
    match CoordSet.pop candidates with
    | Some (elt, candidates) ->
       let new_blocks, candidates' = fill (Interior CoordSet.empty) (CoordSet.singleton elt) candidates in
       repeatedly_fill (CoordSet.union block_set new_blocks) candidates'
    | None -> block_set
  in

  repeatedly_fill block_set candidates
  |> CoordSet.to_seq
  |> Seq.map new_block
  |> List.of_seq
  |> solve_part1

let%test_unit "solve part2 for example" =
  solve_part2 (example ())
  |> [%test_eq: Base.int] 58

let%expect_test "solve part 2" =
  In_channel.with_open_text "input" In_channel.input_all
  |> parse
  |> solve_part2
  |> print_int;
  [%expect {| 2000 |}]
