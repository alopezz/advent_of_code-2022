let example = "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
"

(* A struct representing a group of resources of various types *)
type resources = {
    ore: int;
    clay: int;
    obsidian: int
  }

let empty_resources = {
    ore = 0;
    clay = 0;
    obsidian = 0
  }

let (--) a b = {
    ore = a.ore - b.ore;
    clay = a.clay - b.clay;
    obsidian = a.obsidian - b.obsidian
  }

type blueprint = {
    id: int;
    ore_req: resources;
    clay_req: resources;
    obsidian_req: resources;
    geode_req: resources;
  }

let parse_line line =
  Scanf.sscanf
    line
    "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian"
    (fun id ore_ore clay_ore obs_ore obs_clay geo_ore geo_obs ->
      {id;
       ore_req = {empty_resources with ore = ore_ore};
       clay_req = {empty_resources with ore = clay_ore};
       obsidian_req = {empty_resources with ore = obs_ore; clay = obs_clay};
       geode_req = {empty_resources with ore = geo_ore; obsidian = geo_obs}
      }
    )

let parse s =
  String.split_on_char '\n' s
  |> List.filter (fun s -> String.length s > 0)
  |> List.map parse_line


let%test_unit "parse" =
  let blueprints = parse example in
  [%test_eq: Base.int] 2 (List.length blueprints);
  [%test_eq: Base.int Base.list] [1; 2] (List.map (fun {id; _} -> id) blueprints);
  [%test_eq: Base.int Base.list] [4; 2] (List.map (fun {ore_req; _} -> ore_req.ore) blueprints)


type state = {
    resources: resources;
    geode: int;
    ore_robots: int;
    clay_robots: int;
    obsidian_robots: int;
    geode_robots: int
  }

let build what blueprint state =
  match what with
  | `Ore ->
     {state with
       ore_robots = state.ore_robots + 1;
       resources = state.resources -- blueprint.ore_req}
  | `Clay ->
     {state with
       clay_robots = state.clay_robots + 1;
       resources = state.resources -- blueprint.clay_req}
  | `Obsidian ->
     {state with
       obsidian_robots = state.obsidian_robots + 1;
       resources = state.resources -- blueprint.obsidian_req}
  | `Geode ->
     {state with
       geode_robots = state.geode_robots + 1;
       resources = state.resources -- blueprint.geode_req}

let can_build blueprint state what =
  let {resources; _} = build what blueprint state in
  resources.ore >= 0 && resources.clay >= 0 && resources.obsidian >= 0

let mine state =
  {state with
    resources = {
      ore = state.resources.ore + state.ore_robots;
      clay = state.resources.clay + state.clay_robots;
      obsidian = state.resources.obsidian + state.obsidian_robots;
    };
    geode = state.geode + state.geode_robots;
  }

let play action blueprint state =
  let state' = mine state in
  match action with
  | Some material -> build material blueprint state'
  | None -> state'


let key_of_state {resources; ore_robots; clay_robots; obsidian_robots; geode_robots; _} =
  (resources.ore, resources.clay, resources.obsidian, ore_robots, clay_robots, obsidian_robots, geode_robots)


let dedup states =
  let h = Hashtbl.create (List.length states) in

  List.iter
    (fun state ->
      let key = key_of_state state in
      match Hashtbl.find_opt h key with
      | Some geode -> Hashtbl.replace h key (Int.max state.geode geode)
      | None -> Hashtbl.add h key state.geode
    )
    states;

  (* Rebuild states into list *)
  Hashtbl.to_seq h
  |> Seq.map
       (fun ((ore, clay, obsidian, ore_robots, clay_robots, obsidian_robots, geode_robots), geode) ->
         {resources = {ore; clay; obsidian}; ore_robots; clay_robots; obsidian_robots; geode_robots; geode})
  |> List.of_seq

(* Computes a solution with a naive greedy algorithm, to use as a lower bound *)
let rec greedy_run minutes_left blueprint state =
  if minutes_left = 0 then state
  else
    let state' =
      match List.find_opt (can_build blueprint state) [`Geode; `Obsidian; `Clay; `Ore] with
      | Some `Ore when state.ore_robots < 2 -> play (Some `Ore) blueprint state
      | Some `Clay when state.clay_robots < 4 -> play (Some `Clay) blueprint state
      | Some `Obsidian -> play (Some `Obsidian) blueprint state
      | Some `Geode -> play (Some `Geode) blueprint state
      | _ -> play None blueprint state
    in greedy_run (minutes_left - 1) blueprint state'

let worst_case_for minutes_left blueprint state =
  (greedy_run minutes_left blueprint state).geode


(* Computes a best case upper bound for the given state *)
let rec best_case_for minutes_left blueprint state =
  if minutes_left = 0
  then state.geode
  else
    let state = mine state in
    let state' =
      (* Build a robot of each kind without consuming resources, as long
         as the initial resources were there *)
      [`Ore; `Clay; `Obsidian; `Geode]
      |> List.fold_left
           (fun s m ->
             if can_build blueprint state m
             then
               (* Bypass resource consumption *)
               {(build m blueprint state) with resources = state.resources}
             else s
           )
           state
    in
    best_case_for
      (minutes_left - 1)
      blueprint
      state'


let next_states_for blueprint state =
  let building_actions =
    [`Ore; `Clay; `Obsidian; `Geode]
    |> List.filter (can_build blueprint state)
    |> List.map (fun m -> Some m)
  in
  (* We also include the state that involves no building *)
  (None :: building_actions)
  |> List.map (fun action -> play action blueprint state)

let max_requirement field_fun requirements =
  List.map field_fun requirements |> List.fold_left Int.max 0

let cap_resource minutes_left consumption robots reserves =
  (* The first term is the amount of a resource needed to build a robot every remaining turn *)
  (* The second term is the amount of a resource that will be produced by the current amount
     of robots that produce said resource. *)
  Int.min (minutes_left * consumption - (minutes_left - 1) * robots) reserves |> Int.max 0

(* Caps the resources to the maximum that would be relevant to build
   robots, so that we're more likely to get duplicate states at a
   point where a resource is already too high to really matter. *)
let cap_state minutes_left blueprint state =
  let requirements = [blueprint.ore_req; blueprint.clay_req; blueprint.obsidian_req; blueprint.geode_req] in
  let max_ore = max_requirement (fun {ore; _} -> ore) requirements in
  let max_clay = max_requirement (fun {clay; _} -> clay) requirements in
  let max_obs = max_requirement (fun {obsidian; _} -> obsidian) requirements in
  {state with
    resources = {
      ore = cap_resource minutes_left max_ore state.ore_robots state.resources.ore;
      clay = cap_resource minutes_left max_clay state.clay_robots state.resources.clay;
      obsidian = cap_resource minutes_left max_obs state.obsidian_robots state.resources.obsidian
    }
  }

let maximize_geodes minutes blueprint =
  let rec max_geodes minutes states =
    if minutes = 0
    then
      List.map (fun x -> x.geode) states |> List.fold_left Int.max 0
    else if List.length states = 0 then 0
    else (
      (* Generate new states and reduce them by capping and deduping *)
      let states' =
        List.map (next_states_for blueprint) states |> List.concat
        |> List.map (cap_state (minutes - 1) blueprint)
        |> dedup in

      (* Estimate a lower bound for how many geodes we can get, and
         filter out those states for which even an impossibly
         optimistic estimate is not good enough to reach that bound *)
      let best_worst_case =
        List.map (worst_case_for (minutes - 1) blueprint) states'
        |> List.fold_left Int.max 0
      in
      let states' =
        List.filter
          (fun state ->
            let best_case = best_case_for (minutes - 1) blueprint state in
            best_case > 0 && best_case >= best_worst_case
          )
          states'
      in

      max_geodes (minutes - 1) (dedup states')
    )
  in
  (max_geodes minutes
     [{
         resources = empty_resources;
         geode = 0;
         ore_robots = 1;
         clay_robots = 0;
         obsidian_robots = 0;
         geode_robots = 0
  }])

let solve_part1 blueprints =
  List.map (fun b -> b.id * (maximize_geodes 24 b)) blueprints
  |> List.fold_left (+) 0

let%test_unit "solve part 1 for example" =
  parse example |> solve_part1
  |> [%test_eq: Base.int] 33

let%test_unit "solve part 1" =
  In_channel.with_open_text "input" In_channel.input_all
  |> parse
  |> solve_part1
  |> [%test_eq: Base.int] 960

let solve_part2 blueprints =
  blueprints
  |> List.to_seq
  |> Seq.take 3
  |> Seq.map (maximize_geodes 32)
  |> Seq.fold_left ( * ) 1

let%test_unit "solve part 2 for example" =
  parse example |> solve_part2
  |> [%test_eq: Base.int] (62 * 56)

let%test_unit "solve part 2" =
  In_channel.with_open_text "input" In_channel.input_all
  |> parse
  |> solve_part2
  |> [%test_eq: Base.int] 2040
