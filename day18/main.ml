let time label f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  Printf.printf "%s: %f\n" label (Unix.gettimeofday () -. t0);
  result

let main () =
  let blocks = In_channel.input_all stdin |> Day18.parse in
  time "Time part 1"
    (fun () -> Day18.solve_part1 blocks)
  |> Printf.printf "Part 1: %d\n";
  time "Time part 2"
    (fun () -> Day18.solve_part2 blocks)
  |> Printf.printf "Part 2: %d\n"


let () = time "Total" main
