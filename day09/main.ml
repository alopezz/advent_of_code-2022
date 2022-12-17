let time label f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  Printf.printf "%s: %f\n" label (Unix.gettimeofday () -. t0);
  result

let main () =
  let instructions = Day09.read_instructions stdin in
  time "Time part 1"
    (fun () -> Day09.solve_part1 instructions)
  |> Printf.printf "Part 1: %d\n";
  time "Time part 2"
    (fun () -> Day09.solve_part2 instructions)
  |> Printf.printf "Part 2: %d\n"


let () = time "Total" main
