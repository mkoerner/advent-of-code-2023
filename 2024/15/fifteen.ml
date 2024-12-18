let read_lines file : string list =
  In_channel.with_open_text file In_channel.input_lines

let split_on cond xs =
  let rec loop rh = function
    | [] -> (List.rev rh, [])
    | x :: xs -> if cond x then (List.rev rh, x :: xs) else loop (x :: rh) xs
  in
  loop [] xs

type pos = int * int
type tile = Wall | Box | Free
type map = tile array array
type move = U | D | L | R

let read_char = function '#' -> Wall | 'O' -> Box | _ -> Free

let read_move = function
  | '<' -> L
  | '^' -> U
  | 'v' -> D
  | '>' -> R
  | _ -> failwith "Error"

let read_map (smap : string list) : map * pos =
  let p = ref (0, 0) in
  let h = smap |> List.length in
  let w = smap |> List.hd |> String.length in
  let map = Array.make_matrix h w Free in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      let c = String.get (List.nth smap i) j in
      map.(i).(j) <- read_char c;
      if c = '@' then p := (i, j) else ()
    done
  done;
  (map, !p)

let read_instructions (instr : string list) =
  instr
  |> List.map (fun s -> s |> String.to_seq |> List.of_seq |> List.map read_move)
  |> List.concat

let read_problem file =
  let lines = read_lines file in
  let map, instr = split_on (fun l -> l = "") lines in
  (read_map map, read_instructions instr)

let move ?(d = 1) (x, y) dir =
  match dir with
  | L -> (x, y - d)
  | R -> (x, y + d)
  | U -> (x - d, y)
  | D -> (x + d, y)

(** scan from position in a direction and return number of contiguous boxes and
    if the tile after is free *)
let rec scan_dir ?(l = 0) (map : map) ((x, y) : pos) (d : move) : int * bool =
  let x', y' = move (x, y) d in
  let entry = map.(x').(y') in
  match entry with
  | Free -> (l, true)
  | Wall -> (l, false)
  | Box -> scan_dir ~l:(l + 1) map (x', y') d

(** try to move robot, update map, and return new position *)
let try_move_robot (map : map) (p : pos) (dir : move) : pos =
  let l, f = scan_dir map p dir in
  let s = move p dir in
  let e = move ~d:(l + 1) p dir in
  if not f then p
  else
    let tmp = map.(fst s).(snd s) in
    map.(fst e).(snd e) <- tmp;
    map.(fst s).(snd s) <- Free;
    s

let gps (r, c) = (100 * r) + c

let sum_gps map =
  let w, h = (Array.length map.(0), Array.length map) in
  let r = ref 0 in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      r := !r + if map.(i).(j) = Box then gps (i, j) else 0
    done
  done;
  !r

let () =
  let file = Sys.argv.(1) in
  let (map, pos), instr = read_problem file in
  let _ = instr |> List.fold_left (fun p i -> try_move_robot map p i) pos in
  Printf.printf "%d\n" (sum_gps map)
