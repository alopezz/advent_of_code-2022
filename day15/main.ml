let time label f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  Printf.printf "%s: %f\n" label (Unix.gettimeofday () -. t0);
  result

let main () =
  let sensors = In_channel.input_all stdin |> Day15.parse in
  time "Time part 1"
    (fun () -> Day15.solve_part1 2000000 sensors)
  |> Printf.printf "Part 1: %d\n";
  time "Time part 2"
    (fun () -> Day15.solve_part2 4000000 sensors)
  |> Printf.printf "Part 2: %d\n"


let () = time "Total" main
