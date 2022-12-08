let time label f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  Printf.printf "%s: %f\n" label (Unix.gettimeofday () -. t0);
  result

let () =
  let map = Seq.of_dispenser (fun () -> In_channel.input_line stdin) |> Day08.parse_input in
  time "Time part 1" (fun () -> Day08.count_visible_trees map) |> Printf.printf "Part 1: %d\n";
  time "Time part 2" (fun () -> Day08.best_scenic_score map) |> Printf.printf "Part 2: %d\n"
