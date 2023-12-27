(* read lines of file *)
let read_file file_path =
  let channel = open_in file_path in
  let rec read_lines lines =
    try
      let chars = List.of_seq (String.to_seq (input_line channel)) in
      let lines = List.append lines [chars] in
      read_lines lines
    with End_of_file ->
      close_in channel;
      lines
    in
    read_lines []

type pos2d = { x: int; y: int};;
let print_position pos =
  Printf.printf "(%d, %d)\n" pos.x pos.y;;

let parse_galaxy_line line rowIdx = 
  let rec parse_galaxy_line_aux line colIdx galaxies = 
    match line with
    | [] -> galaxies
    | h::t ->
      match h with
      | '.' -> parse_galaxy_line_aux t (colIdx + 1) galaxies
      | '#' -> parse_galaxy_line_aux t (colIdx + 1) (List.append galaxies [{x = colIdx; y = rowIdx}])
      | _ -> failwith "invalid character"
    in
  parse_galaxy_line_aux line 0 [];;

let parse_galaxies lines = 
  let rec parse_galaxies_aux lines rowIdx galaxies = 
    match lines with
    | [] -> galaxies
    | h::t -> let galaxies_lines = parse_galaxy_line h rowIdx in
    parse_galaxies_aux t (rowIdx + 1) (List.append galaxies galaxies_lines)
  in
  parse_galaxies_aux lines 0 []

module Set = Set.Make(Int);;

let empty_rows galaxies num_rows = 
  let rows_with_galaxies = Set.of_list (List.map (fun g -> g.y) galaxies) in
  let all_rows = Set.of_list (List.init num_rows (fun i -> i)) in
  Set.diff all_rows rows_with_galaxies;;

let empty_cols galaxies num_cols = 
  let cols_with_galaxies = Set.of_list (List.map (fun g -> g.x) galaxies) in
  let all_cols = Set.of_list (List.init num_cols (fun i -> i)) in
  Set.diff all_cols cols_with_galaxies;;

let expand_galaxies galaxies empty_rows empty_cols = 
  let rec expand_galaxies_aux galaxies exp_galaxies empty_rows empty_cols = 
    match galaxies with
    | [] -> exp_galaxies
    | h::t ->
      let prev_num_empty_rows = Set.cardinal (Set.filter (fun r -> r < h.y) empty_rows) in
      let prev_num_empty_cols = Set.cardinal (Set.filter (fun c -> c < h.x) empty_cols) in
      let exp_galaxy = {x = h.x + prev_num_empty_cols; y = h.y + prev_num_empty_rows} in
      expand_galaxies_aux t (exp_galaxy::exp_galaxies) empty_rows empty_cols
    in
  expand_galaxies_aux galaxies [] empty_rows empty_cols

let make_pairs galaxies = 
  let rec make_pairs_aux galaxies pairs = 
    match galaxies with
    | [] -> pairs
    | h::t -> 
      let pairs = List.append pairs (List.map (fun g -> (h, g)) t) in
      make_pairs_aux t pairs
    in
  make_pairs_aux galaxies [];;

(* Do stuff *)
(* let lines = read_file "ex.txt";; *)
let lines = read_file "input.txt";;
let galaxies = parse_galaxies lines;;
let empty_rows = empty_rows galaxies (List.length lines);;
let empty_cols = empty_cols galaxies (List.length (List.hd lines));;
let galaxies = expand_galaxies galaxies empty_rows empty_cols;;
let pairs = make_pairs galaxies;;
let pair_distances = List.map (fun (g1, g2) -> (Int.abs (g1.x - g2.x) + Int.abs (g1.y - g2.y))) pairs;;
let pair_distance_sum = List.fold_left (+) 0 pair_distances;;

(* Print stuff *)
print_endline "Galaxies:";;
List.iter print_position galaxies;;
print_endline "Empty rows:";;
Set.iter (fun r -> Printf.printf "%d\n" r) empty_rows;;
print_endline "Empty cols:";;
Set.iter (fun r -> Printf.printf "%d\n" r) empty_cols;;
Printf.printf "Pair distance sum: %d\n" pair_distance_sum;;
