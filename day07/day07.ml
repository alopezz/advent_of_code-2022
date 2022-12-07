type dir =
  {parent: dir option;
   contents: ((string, fs_entry) Hashtbl.t)}
and fs_entry =
  | Directory of dir
  | File of int


let mkdir name ({contents = tbl; _} as cwd) =
  if not (Hashtbl.mem tbl name)
  then Hashtbl.add tbl name (Directory {parent = Some cwd; contents = Hashtbl.create 1000})

let mkfile name size ({contents = tbl; _}) =
  if not (Hashtbl.mem tbl name)
  then Hashtbl.add tbl name (File size)

let cd arg cwd =
  match (cwd, arg) with
  | ({parent = Some parent; _}, "..") -> parent
  | ({parent = None; _}, "..") -> failwith "Attempt to navigate to non-existing parent"
  | ({contents; _}, arg) ->
     (match Hashtbl.find_opt contents arg with
      | Some (Directory dir) -> dir
      | _ -> failwith (Printf.sprintf "Couldn't navigate into %s as it's not an existing folder" arg))


let build_fs input_function =
  let rec read_next cwd =
    match input_function () with
    | None -> ()
    | Some line ->
       (match String.split_on_char ' ' line with
        | ["$"; "cd"; arg] -> read_next (cd arg cwd)
        (* We ignore `ls` because it doesn't really matter *)
        | "$" :: _ -> read_next cwd
        (* We know that any non-command entry is just the result of running ls *)
        | ["dir"; name] -> mkdir name cwd; read_next cwd
        | [size; name] -> mkfile name (int_of_string size) cwd; read_next cwd
        | _ -> failwith (Printf.sprintf "Invalid input: %s" line)
       )
  in
  let root = {parent = None; contents = Hashtbl.create 1000} in
  mkdir "/" root;
  read_next root;
  root
  
let read_input filename =
  In_channel.with_open_text filename
    (fun ic -> (build_fs (fun () -> In_channel.input_line ic)))

let rec get_directory_sizes {contents; _} =
  contents
  |> Hashtbl.to_seq_values
  |> Seq.fold_left
       (fun (acc_size, acc_sizes) entry ->
         match entry with
         | Directory dir ->
            let new_size, new_sizes = get_directory_sizes dir in
            (acc_size + new_size, new_size :: new_sizes @ acc_sizes)
         | File size -> (acc_size + size, acc_sizes)
       )
       (0, [])


let solve_part1 root =
  let (_total_size, dir_sizes) = get_directory_sizes root in
  List.to_seq dir_sizes
  |> Seq.filter (fun x -> x < 100000)
  |> Seq.fold_left (+) 0

let%expect_test "Example 1" =
  read_input "example_input"
  |> solve_part1
  |> print_int;
  [%expect {| 95437 |}]


let%expect_test "Part 1" =
  read_input "input"
  |> solve_part1
  |> print_int;
  [%expect {| 1583951 |}]


let solve_part2 root =
  let (total_size, dir_sizes) = get_directory_sizes root in
  List.sort Int.compare dir_sizes
  |> List.find_map (fun x -> if 70000000 - total_size + x >= 30000000 then Some x else None)
  |> Option.get

let%expect_test "Example 2" =
  read_input "example_input"
  |> solve_part2
  |> print_int;
  [%expect {| 24933642 |}]

let%expect_test "Part 2" =
  read_input "input"
  |> solve_part2
  |> print_int;
  [%expect {| 214171 |}]
