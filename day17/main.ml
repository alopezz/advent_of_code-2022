let time label f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  Printf.printf "%s: %f\n" label (Unix.gettimeofday () -. t0);
  result

let main () =
  let jet = In_channel.input_line stdin |> Option.get in
  time "Time part 1"
    (fun () -> Day17.solve_part1 jet)
  |> Printf.printf "Part 1: %d\n"
  (* time "Time part 2" *)
  (*   (fun () -> Day17.solve_part2 jet) *)
  (* |> Printf.printf "Part 2: %d\n" *)


let () = time "Total" main
