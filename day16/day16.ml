type valve = {tunnels: string list; flow_rate: int}

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let parse s =
  String.split_on_char '\n' s
  |> List.filter (fun s -> String.length s > 0)
  |> List.fold_left (
         fun m line ->
         match Str.split (Str.regexp "; tunnels? leads? to valves? ") line with
         | [a; b] ->
            let (name, flow_rate) = Scanf.sscanf a "Valve %s has flow rate=%d" (fun a b -> (a, b)) in
            let tunnels = Str.split (Str.regexp ", ") b in
            StringMap.add name {tunnels; flow_rate} m
         | _ -> failwith (Printf.sprintf "Parsing error for %s\n" line)
       )
       StringMap.empty


let%test_unit "parse" =
  let valves = parse "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnel leads to valve CC
"
  in
  [%test_eq: Base.string Base.list] ["DD"; "II"; "BB"] (StringMap.find "AA" valves).tunnels;
  [%test_eq: Base.string Base.list] ["CC"] (StringMap.find "BB" valves).tunnels;
  [%test_eq: Base.int] 0 (StringMap.find "AA" valves).flow_rate;
  [%test_eq: Base.int] 13 (StringMap.find "BB" valves).flow_rate


type explorer = {
    valve: string;
    cache: StringSet.t
  }

type move =
  | Move of string
  | Open of string * valve

let generate_moves valves {valve = valve_name; cache} =
  let {flow_rate; tunnels} as valve = StringMap.find valve_name valves in
  let moves =
    tunnels
    |> List.filter (fun target -> not (StringSet.mem target cache))
    |> List.map (fun target -> Move target)
  in
  (if flow_rate > 0 then (Open (valve_name, valve)) :: moves else moves)
  |> List.to_seq

(* Extension of Seq.product to arbitrary number of combinations *)
let multiproduct seqs =
  (match seqs with
   | hd :: tl ->
      List.fold_left
        (fun p seq ->
          Seq.product p seq
          |> Seq.map (fun (a, b) -> b :: a)
        )
        (Seq.map (fun x -> [x]) hd)
        tl
   | [] -> Seq.empty)
  |> Seq.map List.rev

let%test_unit "multiproduct" =
  [[1; 2; 3]; [4; 5]; [6]]
  |> List.map List.to_seq
  |> multiproduct
  |> List.of_seq
  |> List.iter
       (fun x -> [%test_eq: Base.int] 3 (List.length x))


let chunk chunk_size lst =
  let rec chunk chunks lst =
    if List.length lst = 0
    then chunks
    else
      let (new_chunk, rest) =
        List.mapi (fun idx elt -> (idx, elt)) lst
        |> List.partition_map (fun (idx, elt) -> if idx < chunk_size then Left elt else Right elt)
      in
      chunk (new_chunk :: chunks) rest
  in
  chunk [] lst |> List.rev


(* This returns the pressure in a very optimistic scenario *)
let pressure_bound n valves minutes_left =
  let _, bound =
    StringMap.to_seq valves
    |> Seq.filter (fun (_key, {flow_rate; _}) -> flow_rate > 0)
    |> Seq.map (fun (_key, {flow_rate; _}) -> flow_rate)
    |> List.of_seq
    |> List.sort (fun a b -> Int.compare b a)
    |> chunk n
    |> List.fold_left
         (fun (m, p) flow_rates -> (m - 1, p + m * (List.fold_left (+) 0 flow_rates)))
         (minutes_left, 0)
  in bound


let find_optimal_pressure explorers minutes valves =
  let best_pressure = ref 0 in

  let rec recur valves explorers minutes_left pressure =
    (* Exit conditions: time's up or we know for certain that we can't get better than
       the best pressure we've found so far. *)
    if minutes_left = 0 || pressure + (pressure_bound (List.length explorers) valves minutes_left) < !best_pressure
    then pressure
    else
      (* Generate all possible moves (i.e. recursive calls) *)
      explorers
      |> List.map (generate_moves valves)
      |> multiproduct
      |> List.of_seq
      (* Filter opening of the same valve by more than one explorer *)
      |> List.filter
           (fun moves ->
             let open_moves = List.filter_map (function | Open (a, _) -> Some a | _ -> None) moves in
             StringSet.of_list open_moves |> StringSet.cardinal = List.length open_moves
           )
      (* Generate composed moves *)
      |> List.map
           (fun moves ->
             (fun valves ->

               let valves_opened = ref false in

               (* Update valves *)
               let valves' =
                 List.fold_left
                   (fun valves move ->
                     match move with
                     | Open (name, valve) ->
                        valves_opened := true;
                        StringMap.add name {valve with flow_rate = 0} valves
                     | Move _ -> valves
                   )
                   valves
                   moves
               in

               (* Update caches *)
               let explorers' =
                 (if !valves_opened
                  then
                    (* If we've opened a valve in this turn, clear all caches *)
                    List.map (fun e -> {e with cache = StringSet.of_list [e.valve]}) explorers
                  else
                    (* Update all caches with their current valve *)
                    List.map (fun e -> {e with cache = StringSet.add e.valve e.cache}) explorers)
                 (* Update positions *)
                 |> List.map2
                      (fun move e ->
                        match move with
                        | Move target -> {e with valve = target}
                        | Open _ -> e
                      )
                      moves
               in

               (* Update pressure *)
               let pressure' =
                 List.fold_left
                   (fun p move ->
                     match move with
                     | Open (_, valve) -> p + (minutes_left - 1) * valve.flow_rate
                     | Move _ -> p
                   )
                   pressure
                   moves
               in
               recur valves' explorers' (minutes_left - 1) pressure'
             )
           )
      (* Apply all moves and take highest resulting pressure *)
      |> List.map
           (fun f ->
             let p = f valves in
             if p > !best_pressure then (best_pressure := p);
             p
           )
      |> List.fold_left Int.max pressure
  in

  let explorers = List.map (fun e -> {valve = e; cache = StringSet.empty}) explorers in
  recur valves explorers minutes 0


let solve_part1 =
  find_optimal_pressure ["AA"] 30

let%test_unit "solve part 1, example" =
  let valves = In_channel.with_open_text "example_input" In_channel.input_all |> parse in
  solve_part1 valves
  |> [%test_eq: Base.int] 1651


let%expect_test "solve part 1" =
  let valves = In_channel.with_open_text "input" In_channel.input_all |> parse in
  solve_part1 valves
  |> print_int;
  [%expect {| 2181 |}]


let solve_part2 =
  find_optimal_pressure ["AA"; "AA"] 26

let%test_unit "solve part 2, minimal" =
 let valves =
   StringMap.empty
   |> StringMap.add "AA" {tunnels = ["BB"; "CC"]; flow_rate = 1}
   |> StringMap.add "BB" {tunnels = ["AA"; "CC"]; flow_rate = 0}
   |> StringMap.add "CC" {tunnels = ["AA"; "BB"]; flow_rate = 0}
 in
 solve_part2 valves
 |> [%test_eq: Base.int] 25

let%test_unit "solve part 2, minimal" =
 let valves =
   StringMap.empty
   |> StringMap.add "AA" {tunnels = ["BB"; "CC"]; flow_rate = 0}
   |> StringMap.add "BB" {tunnels = ["AA"; "CC"]; flow_rate = 1}
   |> StringMap.add "CC" {tunnels = ["AA"; "BB"]; flow_rate = 1}
 in
 solve_part2 valves
 |> [%test_eq: Base.int] (2 * 24)

let%test_unit "solve part 2, minimal" =
 let valves =
   StringMap.empty
   |> StringMap.add "AA" {tunnels = ["BB"]; flow_rate = 1}
   |> StringMap.add "BB" {tunnels = ["CC"]; flow_rate = 1}
   |> StringMap.add "CC" {tunnels = ["AA"]; flow_rate = 0}
 in
 solve_part2 valves
 |> [%test_eq: Base.int] (25 + 24)

let%test_unit "solve part 2, example" =
  let valves = In_channel.with_open_text "example_input" In_channel.input_all |> parse in
  solve_part2 valves
  |> [%test_eq: Base.int] 1707

(* let%expect_test "solve part 2" = *)
(*   let valves = In_channel.with_open_text "input" In_channel.input_all |> parse in *)
(*   solve_part2 valves *)
(*   |> print_int; *)
(*   [%expect {| 2824 |}] *)
